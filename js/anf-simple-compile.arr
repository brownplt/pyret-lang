#lang pyret

provide *
import "ast-anf.arr" as N
import "ast-split.arr" as S
import "js-ast.arr" as J
import "gensym.arr" as G
import string-dict as D

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
j-list = J.j-list
j-obj = J.j-obj
j-dot = J.j-dot
j-bracket = J.j-bracket
j-field = J.j-field
j-dot-assign = J.j-dot-assign
j-bracket-assign = J.j-bracket-assign
j-try-catch = J.j-try-catch
j-throw = J.j-throw
j-expr = J.j-expr
j-binop = J.j-binop
j-eq = J.j-eq
j-unop = J.j-unop
j-decr = J.j-decr
j-incr = J.j-incr
j-ternary = J.j-ternary
j-null = J.j-null
j-parens = J.j-parens

js-id-of = block:
  var js-ids = D.immutable-string-dict()
  fun(id :: String):
    if js-ids.has-key(id):
      js-ids.get(id)
    else: no-hyphens = id.replace("-", "_DASH_")
      safe-id = G.make-name(no-hyphens)
      js-ids := js-ids.set(id, safe-id)
      safe-id
    end
  end
end

data Env:
  | bindings(vars :: Set<String>, ids :: Set<String>)
end

fun compile(prog :: S.SplitResult, headers :: List<N.AHeader>) -> J.JExpr:
  ids = headers.map(_.name).map(js-id-of)
  filenames = headers.map(fun(h):
      cases(N.AImport) h:
        | a-import-builtin(l, name, _) => "builtin-libs/" + name
        | a-import-file(l, file, _) => file
      end
    end)
  fun inst(id): j-app(j-id(id), [j-id("RUNTIME"), j-id("NAMESPACE")]);
  namespace-ids = [
      "test-print",
      "print",
      "display",
      "tostring",
      "torepr",
      "brander",
      "raise",
      "nothing",
      "builtins",
      "is-nothing",
      "is-number",
      "is-string",
      "is-boolean",
      "is-object",
      "is-function",
      "gensym"
    ]
  namespace-binds = for map(n from namespace-ids):
      j-var(js-id-of(n), j-method(j-id("NAMESPACE"), "get", [j-str(n)]))
    end
  module-id = G.make-name("mod")
  rt-field = fun(name): j-dot(j-id("RUNTIME"), name);
  module-ref = fun(name): j-bracket(rt-field("modules"), j-str(name));
  j-app(j-id("define"), [j-list(filenames.map(j-str)), j-fun(ids, j-block([
      j-return(j-fun(["RUNTIME", "NAMESPACE"],
        j-block([
          j-if(module-ref(module-id),
            j-block([j-return(module-ref(module-id))]),
            j-block([
                j-bracket-assign(rt-field("modules"), j-str(module-id), thunk-app(
                   j-block(
                     [ j-dot-assign(j-id("RUNTIME"), "EXN_STACKHEIGHT", j-num(0)) ] +
                     namespace-binds +
                     for map(id from ids):
                       j-assign(id, j-method(j-id("RUNTIME"), "getField", [inst(id), j-str("provide")]))
                     end +
                     prog.helpers.map(compile-helper) +
                     [compile-e(prog.body)]))),
                j-return(module-ref(module-id))
              ]))
           ])))
    ]))])
end

fun helper-name(s :: String): "$HELPER_" + js-id-of(s);

fun compile-helper(h :: S.Helper) -> J.JStmt:
  cases(S.Helper) h:
    | helper(name, args, body) =>
      j-var(helper-name(name), j-fun(args.map(js-id-of), compile-e(body)))
  end
end

fun maybe-return(e):
  cases(N.AExpr) e:
    | a-lettable(lettable) => j-block([j-return(compile-l(lettable))])
    | else => compile-e(e)
  end
end

fun compile-e(expr :: N.AExpr) -> J.JBlock:
  cases(N.AExpr) expr:
    | a-let(l, b, e, body) =>
      compiled-body = maybe-return(body)
      j-block(
          link(
              j-var(js-id-of(b.id), compile-l(e)),
              compiled-body.stmts
            )
        )
    | a-var(l, b, e, body) =>
      compiled-body = maybe-return(body)
      j-block(link(
                j-var(js-id-of(b.id), j-obj([j-field("$var", compile-l(e)), j-field("$name", j-str(js-id-of(b.id)))])),
                compiled-body.stmts))
    | a-split-app(l, is-var, f, args, name, helper-args) =>
      when is-var: raise("Can't handle splitting on a var yet");
      e = js-id-of(G.make-name("e"))
      z = js-id-of(G.make-name("z"))
      ss = js-id-of(G.make-name("ss"))
      ret = js-id-of(G.make-name("ret"))
      body =
        j-if(j-binop(j-unop(j-dot(j-id("RUNTIME"), "GAS"), j-decr), J.j-gt, j-num(0)),
          j-block([j-expr(j-assign(z, app(f, args)))]),
          j-block([
              j-expr(j-dot-assign(j-id("RUNTIME"), "EXN_STACKHEIGHT", j-num(0))),
              j-throw(j-method(j-id("RUNTIME"), "makeCont", 
                  [j-obj([j-field("go",
                          j-fun([js-id-of("ignored")], j-block([j-return(app(f, args))])))])]))]))
      helper-ids = helper-args.rest.map(_.id).map(js-id-of)
      catch =
        j-if(j-method(j-id("RUNTIME"), "isCont", [j-id(e)]),
          j-block([
              j-var(ss,
                j-obj([
                  j-field("captureExn", j-fun(["exn"],
                    j-block([
                        j-return(add-stack-frame(l, "exn"))
                      ]))),
                  j-field("go", j-fun([js-id-of(helper-args.first.id)],
                    j-block([
                      j-return(j-app(j-id(helper-name(name)),
                          link(
                              j-id(js-id-of(helper-args.first.id)),
                              helper-ids.map(fun(a): j-dot(j-id("this"), a) end))))])))] + 
                  helper-ids.map(fun(a): j-field(a, j-id(a)) end))),
              j-expr(j-bracket-assign(j-dot(j-id(e), "stack"),
                  j-unop(j-dot(j-id("RUNTIME"), "EXN_STACKHEIGHT"), J.j-postincr), j-id(ss))),
              #j-expr(j-method(j-dot(j-id(e), "stack"), "push", [j-id(ss)])),
              j-throw(j-id(e))]),
          j-block([
              j-if(j-method(j-id("RUNTIME"), "isPyretException", [j-id(e)]),
                  j-block([
                      j-expr(add-stack-frame(l, e)),
                      j-throw(j-id(e))
                    ]),
                  j-block([j-throw(j-id(e))]))
            ]))
      j-block([
          j-var(z, j-undefined),
          j-try-catch(body, e, catch),
          j-var(ret, j-app(j-id(helper-name(name)), [j-id(z)] + helper-args.rest.map(compile-v))),
          j-expr(j-unop(j-dot(j-id("RUNTIME"), "GAS"), j-incr)),
          j-return(j-id(ret))
        ])

    | a-lettable(l) =>
      j-block([j-return(compile-l(l))])
  end
end

fun add-stack-frame(l, exn-id):
  j-method(j-dot(j-id(exn-id), "pyretStack"), "push",
      [j-obj([
          j-field("src", j-str(l.file)),
          j-field("line", j-num(l.line)),
          j-field("column", j-num(l.column))
        ])])
end

fun compile-l(expr :: N.ALettable) -> J.JExpr:
  cases(N.ALettable) expr:
    | a-lam(l, args, body) =>
      j-method(j-id("RUNTIME"), "makeFunction", [j-fun(args.map(_.id).map(js-id-of), compile-e(body))])
    
    | a-method(l, args, body) =>

       compiled-body = compile-e(body)

      j-method(j-id("RUNTIME"), "makeMethod", [j-fun([js-id-of(args.first.id)],
        j-block([
          j-return(j-fun(args.rest.map(_.id).map(js-id-of), compiled-body))])),
         
        j-fun(args.map(_.id).map(js-id-of), compiled-body)])

    | a-assign(l, id, val) =>
      j-dot-assign(j-id(js-id-of(id)), "$var", compile-v(val))

    | a-extend(l, obj, vals) =>
      j-method(compile-v(obj), "extendWith", [j-obj(vals.map(compile-field))])

    | a-dot(l, obj, field) =>
      j-method(j-id("RUNTIME"), "getField", [compile-v(obj), j-str(field)])

    | a-colon(l, obj, field) =>
      j-method(j-id("RUNTIME"), "getColonField", [compile-v(obj), j-str(field)])

    | a-app(l, f, args) => app(f, args)

    | a-if(l, cond, consq, alt) =>
      compiled-consq = thunk-app(maybe-return(consq))
      compiled-alt = thunk-app(maybe-return(alt))
      j-parens(j-ternary(j-method(j-id("RUNTIME"), "isPyretTrue", [compile-v(cond)]), compiled-consq, compiled-alt))

    | a-val(v) => compile-v(v)
    | a-obj(l, fields) => 
        j-method(j-id("RUNTIME"), "makeObject", [j-obj(fields.map(fun(f): j-field(f.name, compile-v(f.value));))])
    | else => raise("NYI: " + torepr(expr))
  end
end

fun compile-field(f):
  cases(N.AField) f:
    | a-field(l, name, val) => j-field(name, compile-v(val))
  end
end

fun thunk-app(block):
  j-app(j-parens(j-fun([], block)), [])
end

fun thunk-app-stmt(stmt):
  thunk-app(j-block([stmt]))
end

fun app(f, args):
#  j-app(compile-v(f), args.map(compile-v))
  j-ternary(j-binop(j-dot(compile-v(f), "arity"), j-eq, j-num(args.length())),
    j-method(compile-v(f), "app", args.map(compile-v)),
    thunk-app-stmt(
      j-throw(j-method(j-id("RUNTIME"), "makeMessageException", [
          j-str("Arity mismatch")
        ]))))
end

fun compile-v(v :: N.AVal) -> J.JExpr:
  cases(N.AVal) v:
    | a-id(l, id) => j-id(js-id-of(id))
    | a-id-var(l, id) => j-dot(j-id(js-id-of(id)), "$var")
    | a-id-letrec(l, id) => j-dot(j-id(js-id-of(id)), "$var")
    | a-num(l, n) => j-method(j-id("RUNTIME"), "makeNumber", [j-num(n)])
    | a-str(l, s) => j-method(j-id("RUNTIME"), "makeString", [j-str(s)])
    | a-bool(l, b) =>
      str = if b: "pyretTrue" else: "pyretFalse";
      j-dot(j-id("RUNTIME"), str)
    | a-undefined(l) => j-undefined
  end
end

