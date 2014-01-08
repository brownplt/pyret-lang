#lang pyret

provide *
import "ast-anf.arr" as N
import "ast-split.arr" as S
import "js-ast.arr" as J
import format as F

j-fun = J.j-fun
j-var = J.j-var
j-id = J.j-id
j-method = J.j-method
j-block = J.j-block
j-true = J.j-true
j-false = J.j-false
j-undefined = J.j-undefined
j-num = J.j-num
j-str = J.j-str
j-return = J.j-return
j-assign = J.j-assign
j-if = J.j-if
j-app = J.j-app

format = F.format

js-id-of = block:
  var js-ids = {}
  fun(id :: String):
    if builtins.has-field(js-ids, id):
      js-ids.[id]
    else: no-hyphens = id.replace("-", "_DASH_")
      safe-id = gensym(no-hyphens)
      js-ids := js-ids.{ [id]: safe-id }
      safe-id
    end
  end
end

fun compile(prog :: S.SplitResult) -> J.JExpr:
  j-fun(["RUNTIME", "NAMESPACE"], j-block([
    j-var(js-id-of("test-print"),
      j-method(J.j-id("NAMESPACE"), "get", [J.j-str("test-print")]))] +
    prog.helpers.map(compile-helper) +
    [compile-e(prog.body)]))
end

fun helper-name(s :: String): "$HELPER_" + s;

fun compile-helper(h :: S.Helper) -> J.JStmt:
  cases(S.Helper) h:
    | helper(name, args, body) =>
      j-var(helper-name(name), j-fun(args.map(js-id-of), compile-e(body)))
  end
end

fun compile-e(expr :: N.AExpr) -> J.JBlock:
  fun maybe-return(e):
    cases(N.AExpr) expr:
      | a-lettable(lettable) => j-block([j-return(compile-l(lettable))])
      | else => compile-e(e)
    end
  end
  cases(N.AExpr) expr:
    | a-let(l, b, e, body) =>
      compiled-body = maybe-return(body)
      j-block([
          link(
              j-var(b.id, compile-l(e)),
              compiled-body.stmts
            )
        ])
    | a-if(l, cond, consq, alt) =>
      compiled-consq = maybe-return(consq)
      compiled-alt = maybe-return(alt)
      j-block([
          j-if(compile-v(cond), compiled-consq, compiled-alt)
        ])
    | a-lettable(l) =>
      j-block([
          j-return(compile-l(l))
        ])
  end
end

fun compile-l(expr :: N.ALettable) -> J.JExpr:
  cases(N.ALettable) expr:
    | a-lam(l, args, body) =>
      j-fun(args.map(_.id), compile-e(body))

    | a-assign(l, id, val) =>
      j-assign(id, compile-v(val), id)

    | a-app(l, f, args) =>
      j-method(compile-v(f), "app", args.map(compile-v))

    | a-split-app(l, f, args, name, helper-args) =>
      j-app(
          j-id(helper-name(name)),
          link(
              j-method(compile-v(f), "app", args.map(compile-v)),
              helper-args.rest.map(compile-v)
            )
        )
    | a-val(v) => compile-v(v)
    | else => raise("NYI: " + torepr(expr))
  end
end

fun compile-v(v :: N.AVal) -> J.JExpr:
  cases(N.AVal) v:
    | a-id(l, id) => j-id(js-id-of(id))
    | a-num(l, n) => j-method(j-id("RUNTIME"), "makeNumber", [j-num(n)])
    | a-str(l, s) => j-method(j-id("RUNTIME"), "makeString", [j-str(s)])
    | a-bool(l, b) =>
      j-bool = if b: j-true else: j-false end
      j-method(j-id("RUNTIME"), "makeBool", [j-bool])
    | a-undefined(l) => j-undefined
  end
end

