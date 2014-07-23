#lang pyret

provide *
provide-types *
import "compiler/ast-anf.arr" as N
import ast as A
import pprint as PP
import "compiler/gensym.arr" as G

names = A.global-names

INDENT = 2
# Use this instead of built-in lists because append has quadratic
# behavior in the algorithm below when appending helpers
data ConcatList<a>:
  | concat-empty with:
    to-list-acc(self, rest): rest end,
    map(self, f): self end
  | concat-singleton(element) with:
    to-list-acc(self, rest): link(self.element, rest) end,
    map(self, f): concat-singleton(f(self.element)) end
  | concat-append(left :: ConcatList<a>, right :: ConcatList<a>) with:
    to-list-acc(self, rest :: List):
      self.left.to-list-acc(self.right.to-list-acc(rest))
    end,
    map(self, f): concat-append(self.left.map(f), self.right.map(f)) end
sharing:
  _plus(self, other :: ConcatList):
    concat-append(self, other)
  end,
  to-list(self): self.to-list-acc([list: ]) end
end


data Helper:
  | helper(name :: A.Name, args :: List<A.Name>, body :: N.AExpr) with:
    tosource(self):
      arg-list = PP.nest(INDENT, PP.surround-separate(INDENT, 0, PP.lparen + PP.rparen,
          PP.lparen, PP.commabreak, PP.rparen, self.args.map(_.tosource())))
      header = PP.group(PP.str("helper " + self.name.tostring()) + arg-list + PP.str(" {"))
      PP.surround(INDENT, 1, header, self.body.tosource(), PP.str("}"))
    end
end

fun freevars-helper(h :: Helper):
  cases(Helper) h:
    | helper(name, args, body) =>
      N.freevars-e(body).difference(sets.list-to-tree-set(args))
  end
end

data SplitResult:
  | split-result(helpers :: List<Helper>, body :: N.AExpr, freevars :: Set<A.Name>) with:
    tosource(self):
      PP.vert(self.helpers.map(_.tosource()) + [list: self.body.tosource()])
    end
end

fun freevars-split-result(sr :: SplitResult):
  sr.freevars
end

data SplitResultInt:
  | split-result-int-e(
      helpers :: ConcatList<Helper>,
      body :: N.AExpr,
      freevars :: Set<String>
    )
  | split-result-int-l(
      helpers :: ConcatList<Helper>,
      body :: N.ALettable,
      freevars :: Set<String>
    )
end

fun <a> unions(ss :: List<Set<a>>) -> Set<a>:
  for fold(unioned from sets.empty-tree-set, s from ss):
    unioned.union(s)
  end
end


fun ast-split(expr :: N.AExpr) -> SplitResult:
  r = ast-split-expr(expr)
  split-result(r.helpers.to-list(), r.body, r.freevars)
end

fun ast-split-expr(expr :: N.AExpr) -> SplitResultInt:
  fun handle-bind(l, is-var, b, e, body):
    cases(N.ALettable) e:
      | a-app(l2, f, args) =>
        rest-split = ast-split-expr(body)
        fvs = N.freevars-ann-acc(b.ann, rest-split.freevars.remove(b.id))
        h = if A.is-a-blank(b.ann) or A.is-a-any(b.ann):
          helper(names.make-atom(b.id.toname()), link(b.id, fvs.to-list()), rest-split.body)
        else:
          nonce = G.make-name("chk_" + b.id.toname())
          nonce-name = names.make-atom(nonce)
          ann-check = N.a-let(l, b, N.a-val(N.a-id(l, nonce-name)), rest-split.body)
          helper(names.make-atom(b.id.toname()), link(nonce-name, fvs.to-list()), ann-check)
        end
        split-result-int-e(
            concat-singleton(h) + rest-split.helpers,
            N.a-split-app(l, is-var, f, args, h.name, h.args.map(N.a-id(l, _))),
            fvs.union(unions(args.map(N.freevars-v))).union(N.freevars-v(f))
          )
      | else =>
        e-split = ast-split-lettable(e)
        rest-split = ast-split-expr(body)
        split-result-int-e(
            rest-split.helpers + e-split.helpers,
            if is-var:
              N.a-var(l, b, e-split.body, rest-split.body)
            else:
              N.a-let(l, b, e-split.body, rest-split.body)
            end,
            N.freevars-ann-acc(b.ann, rest-split.freevars.remove(b.id).union(e-split.freevars))
          )
    end
  end
  cases(N.AExpr) expr:
    | a-let(l, b, e, body) =>
      handle-bind(l, false, b, e, body)
    | a-seq(l, e1, e2) =>
      cases(N.a-lettable) e1:
        | a-app(l2, f, args) => handle-bind(l, false, N.a-bind(l2, A.global-names.make-atom("anf_begin_app_dropped"), A.a-blank), e1, e2)
        | else =>
          e1-split = ast-split-lettable(e1)
          e2-split = ast-split-expr(e2)
          split-result-int-e(
            e2-split.helpers + e1-split.helpers,
            N.a-seq(l, e1-split.body, e2-split.body),
            e2-split.freevars.union(e1-split.freevars)
          )
      end
    | a-var(l, b, e, body) =>
      cases(N.ALettable) e:
        | a-val(v) => handle-bind(l, true, b, e, body)
        | else =>
          n = names.make-atom("var-bind")
          ast-split-expr(
            N.a-let(l, N.a-bind(l, n, A.a-blank), e,
              N.a-var(l, b, N.a-val(N.a-id(l, n)), body)))
      end
    | a-if(l, cond, consq, alt) =>
      consq-split = ast-split-expr(consq)
      alt-split = ast-split-expr(alt)
      split-result-int-e(
          consq-split.helpers + alt-split.helpers,
          N.a-if(l, cond, consq-split.body, alt-split.body),
          N.freevars-v(cond).union(consq-split.freevars).union(alt-split.freevars)
        )
    | a-type-let(l, bind, body) =>
      body-split = ast-split-expr(body)
      freevars = cases(N.ATypeBind) bind:
        | a-newtype-bind(l2, name, nameb) => body-split.freevars.remove(name).remove(nameb)
        | a-type-bind(l2, name, ann) =>
          N.freevars-ann-acc(ann, body-split.freevars.remove(name))
      end
      split-result-int-e(
          body-split.helpers,
          N.a-type-let(l, bind, body-split.body),
          freevars
        )
    | a-lettable(e) =>
      cases(N.ALettable) e:
        | a-app(l, f, args) =>
          split-result-int-e(concat-empty, N.a-tail-app(l, f, args), N.freevars-v(f).union(unions(args.map(N.freevars-v))))
        | else =>
          let-result = ast-split-lettable(e)
          split-result-int-e(let-result.helpers, N.a-lettable(let-result.body), let-result.freevars)
      end
    | else => raise("NYI: " + torepr(expr))
  end
end

fun ast-split-lettable(e :: N.ALettable) -> SplitResultInt%(is-split-result-int-l):
  cases(N.ALettable) e:
    | a-lam(l, args, ret, body) =>
      body-split = ast-split-expr(body)
      split-result-int-l(
          body-split.helpers,
          N.a-lam(l, args, ret, body-split.body),
          N.freevars-ann-acc(ret, N.freevars-list-acc(args.map(_.ann), body-split.freevars.difference(sets.list-to-tree-set(args.map(_.id)))))
        )
    | a-method(l, args, ret, body) =>
      body-split = ast-split-expr(body)
      split-result-int-l(
          body-split.helpers,
          N.a-method(l, args, ret, body-split.body),
          N.freevars-ann-acc(ret, N.freevars-list-acc(args.map(_.ann), body-split.freevars.difference(sets.list-to-tree-set(args.map(_.id)))))
        )
    | else =>
      split-result-int-l(concat-empty, e, N.freevars-l(e))
  end
end

fun param(l, name):
  N.a-bind(l, name, A.a-blank)
end

check:
  fun strip-helper(h):
    cases(Helper) h:
      | helper(name, args, body) => helper(name, args, N.strip-loc-expr(body))
    end
  end
  fun split-strip(e):
    res = ast-split-expr(e)
    split-result-int-e(res.helpers.map(strip-helper), N.strip-loc-expr(res.body), N.freevars-e(e))
  end
  b = A.a-blank
  d = N.dummy-loc
  n = A.global-names.make-atom
  e1 = N.a-lettable(N.a-val(N.a-num(d, 5)))
  split-strip(e1) is split-result-int-e(concat-empty, e1, sets.empty-tree-set)

  x = n("x")
  e2 = N.a-let(d, N.a-bind(d, x, A.a-blank), N.a-val(N.a-num(d, 5)), N.a-lettable(N.a-val(N.a-id(d, x))))
  e2-split = split-strip(e2)
  e2-split.helpers.to-list() is [list: ]
  e2-split.body is e2
  e2-split.freevars is sets.empty-tree-set

  v = n("v")
  f = n("f")
  e3 = N.a-let(d, N.a-bind(d, v, A.a-blank), N.a-app(d, N.a-id(d, f), [list: N.a-num(d, 5)]),
    N.a-lettable(N.a-val(N.a-id(d, v))))
  e3-split = split-strip(e3)
  e3-split.helpers.to-list().length() is 1
  e3-split.helpers.to-list().first.body is
    N.a-lettable(N.a-val(N.a-id(d, v)))
  e3-split.body is
    N.a-split-app(d, false, N.a-id(d, f), [list: N.a-num(d, 5)], e3-split.helpers.to-list().first.name, [list: N.a-id(d, v)])
end

