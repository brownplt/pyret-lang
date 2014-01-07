#lang pyret

provide *
import "ast-anf.arr" as N
import "ast-split.arr" as S
import format as F

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

fun compile(prog :: S.SplitResult):
 
  format("(function(RUNTIME, NAMESPACE) {
      var ~a = NAMESPACE.get('test-print');
      ~a
      ~a
    })", [js-id-of('test-print'), prog.helpers.map(compile-helper).join-str("\n"), compile-e(prog.body)])
end

fun helper-name(s :: String): "$HELPER_" + s;

fun compile-helper(h :: S.Helper):
  cases(S.Helper) h:
    | helper(name, args, body) =>
      format("function ~a(~a) {
        return ~a
      }", [helper-name(name), args.map(js-id-of).join-str(","), compile-e(body)])
  end
end

fun compile-e(expr :: N.AExpr):
  cases(N.AExpr) expr:
    | a-let(l, b, e, body) =>
      format(
"(function(~a) { return ~a })(~a)",
        [b.id, compile-e(body), compile-l(e)])        
    | a-if(l, cond, consq, alt) =>
      format(
"(~a ? ~a : ~a)",
        [compile-v(cond), compile-e(consq), compile-e(alt)])
    | a-lettable(l) => compile-l(l)
  end
end

fun compile-l(expr :: N.ALettable):
  cases(N.ALettable) expr:
    | a-lam(l, args, body) =>
      format(
"(function(~a) {
  return ~a;
})",
        [args.map(_.id).join-str(","), compile-e(body)])
    | a-assign(l, id, val) =>
      format(
"(function() {
  ~a = ~a;
  return ~a;
})()",
        [id, compile-v(val), id])

    | a-app(l, f, args) =>
      format(
"~a.app(~a)",
        [compile-v(f), args.map(compile-v).join-str(",")])
    | a-split-app(l, f, args, name, helper-args) =>
      format(
"~a(~a.app(~a), ~a)",
        [helper-name(name), compile-v(f), args.map(compile-v).join-str(","), helper-args.rest.map(compile-v).join-str(",")]
        )
    | a-val(v) => compile-v(v)
    | else => raise("NYI: " + torepr(expr))
  end
end

fun compile-v(v :: N.AVal):
  cases(N.AVal) v:
    | a-id(l, id) => js-id-of(id)
    | a-num(l, n) => format("RUNTIME.makeNumber(~a)", [n.tostring()])
    | a-str(l, s) => format("~s", [s])
    | a-bool(l, b) => b.tostring()
    | a-undefined(l) => "undefined"
  end
end
