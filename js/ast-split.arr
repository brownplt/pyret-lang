#lang pyret

provide *
import "ast-anf.arr" as N
import ast as A

data Helper:
  | helper(name :: String, args :: List<String>, body :: N.AExpr)
end

data SplitResult:
  | split-result-e(helpers :: List<Helper>, body :: N.AExpr)
  | split-result-l(helpers :: List<Helper>, body :: N.ALettable)
end

fun create-helper(id :: String, e :: N.AExpr) -> Helper:
  fv = freevars-e(e)
  fv.remove(id)
  helper(gensym(id), link(id, fv.to-list()), e)
end

fun <a> unions(ss :: List<Set<a>>) -> Set<a>:
  for fold(unioned from set([]), s from ss):
    unioned.union(s)
  end
end

fun freevars-e(expr :: N.AExpr) -> Set<String>:
  cases(N.AExpr) expr:
    | a-let(_, b, e, body) =>
      freevars-e(body).remove(b.id).union(freevars-l(e))
    | a-var(_, b, e, body) =>
      freevars-e(body).remove(b.id).union(freevars-l(e))
    | a-letrec(_, bs, body) => raise("letrec nyi")
    | a-if(_, c, t, e) =>
      freevars-v(c).union(freevars-e(t)).union(freevars-e(e))
    | a-try(_, body, b, c) =>
      freevars-e(c).remove(b.id).union(freevars-e(body))
    | a-lettable(e) => freevars-l(e)
  end
end

fun freevars-l(e :: N.ALettable) -> Set<String>:
  cases(N.ALettable) e:
    | a-assign(_, _, v) => freevars-v(v)
    | a-app(_, f, args) => freevars-v(f).union(unions(args.map(freevars-v)))
    | a-help-app(_, _, args) => unions(args.map(freevars-v))
    | a-lam(_, args, body) => freevars-e(body).difference(set(args.map(_.id)))
    | a-method(_, args, body) => freevars-e(body).difference(set(args.map(_.id)))
    | a-obj(_, fields) => fields.map(fun(f): freevars-v(_.value) end)
    | a-update(_, super, fields) => freevars-v(super).union(fields.map(fun(f): freevars-v(_.value) end))
    | a-dot(_, obj, _) => freevars-v(obj)
    | a-colon(_, obj, _) => freevars-v(obj)
    | a-get-bang(_, obj, _) => freevars-v(obj)
    | a-val(v) => freevars-v(v)
  end
end

fun freevars-v(v :: N.AVal) -> Set<String>:
  cases(N.AVal) v:
    | a-id(_, id) => set([id])
    | else => set([])
  end
end

fun ast-split(expr :: N.AExpr) -> SplitResult:
  cases(N.AExpr) expr:
    | a-let(l, b, e, body) =>
      cases(N.ALettable) e:
        | a-app(l2, f, args) =>
          rest-split = ast-split(body)
          h = create-helper(b.id, rest-split.body)
          split-result-e(
              link(h, rest-split.helpers),
              N.a-lettable(N.a-split-app(l, f, args, h.name, h.args.map(N.a-id(l, _))))
            )
        | else =>
          rest-split = ast-split(body)
          split-result-e(
              rest-split.helpers,
              N.a-let(l, b, e, rest-split.body)
            )
      end
    | a-if(cond, consq, alt) =>
      consq-result = ast-split-lettable(consq)
      alt-result = ast-split-lettable(alt)
      split-result-e(
          consq-result.helpers + alt-result.helpers,
          N.a-if(cond, consq-result.body, alt-result.body)
        )
    | a-lettable(e) =>
      let-result = ast-split-lettable(e)
      split-result-e(let-result.helpers, N.a-lettable(let-result.body))
    | else => raise("NYI: " + torepr(expr))
  end
end

fun ast-split-lettable(e :: N.ALettable) -> is-split-result-l:
  cases(N.ALettable) e:
    | a-lam(l, args, body) =>
      body-split = ast-split(body)
      split-result-l(
          body-split.helpers,
          N.a-lam(l, args, body-split.body)
        )
    | a-method(l, args, body) =>
      body-split = ast-split(body)
      split-result-l(
          body-split.helpers,
          N.a-method(l, args, body-split.body)
        )
    | else =>
      split-result-l([], e)
  end
end

fun param(l, name):
  N.a-bind(l, name, A.a_blank)
end

check:
  b = A.a_blank
  d = error.location("dummy", -1, -1)
  e1 = N.a-lettable(N.a-val(N.a-num(d, 5)))
  ast-split(e1) is split-result-e([], e1)

  e2 = N.a-let(d, N.a-bind(d, "x", A.a_blank), N.a-val(N.a-num(d, 5)), N.a-lettable(N.a-val(N.a-id(d, "x"))))
  ast-split(e2) is split-result-e([], e2)

  e3 = N.a-let(d, N.a-bind(d, "v", A.a_blank), N.a-app(d, N.a-id(d, "f"), [N.a-num(d, 5)]),
    N.a-lettable(N.a-val(N.a-id(d, "v"))))
  e3-split = ast-split(e3)
  e3-split.helpers.length() is 1
  e3-split.helpers.first.body is
    N.a-lettable(N.a-val(N.a-id(d, "v")))
  e3-split.body is
    N.a-lettable(N.a-split-app(d, N.a-id(d, "f"), [N.a-num(d, 5)], e3-split.helpers.first.name, [N.a-id(d, "v")]))
end

