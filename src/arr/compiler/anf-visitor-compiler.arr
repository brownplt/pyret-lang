#lang pyret

provide *
import ast as A
import "./ast-anf.arr" as N
import "./ast-split.arr" as S
import "./js-ast.arr" as J
import "./gensym.arr" as G
import "./compile-structs.arr" as CS
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

Loc = error.Location

js-id-of = block:
  var js-ids = D.string-dict()
  fun(id :: String):
    if js-ids.has-key(id):
      js-ids.get(id)
    else: no-hyphens = builtins.string-replace(id, "-", "_DASH_")
      safe-id = G.make-name(no-hyphens)
      js-ids.set(id, safe-id)
      safe-id
    end
  end
end

fun obj-of-loc(l):
  j-obj([
      j-field("src", j-str(l.file)),
      j-field("line", j-num(l.line)),
      j-field("column", j-num(l.column))
    ])
end

fun add-stack-frame(l, exn-id):
  j-method(j-dot(j-id(exn-id), "pyretStack"), "push", [obj-of-loc(l)])
end

fun rt-field(name): j-dot(j-id("RUNTIME"), name);
fun rt-method(name, args): j-method(j-id("RUNTIME"), name, args);

fun app(f, args):
#  j-app(compile-v(f), args.map(compile-v))
  j-ternary(j-binop(j-dot(f, "arity"), j-eq, j-num(args.length())),
    j-method(f, "app", args),
    thunk-app-stmt(
      j-throw(j-method(j-id("RUNTIME"), "makeMessageException", [
          j-str("Arity mismatch")
        ]))))
end

fun thunk-app(block):
  j-app(j-parens(j-fun([], block)), [])
end

fun thunk-app-stmt(stmt):
  thunk-app(j-block([stmt]))
end

fun helper-name(s :: String): "$HELPER_" + js-id-of(s);

fun compile-helper(compiler, h :: S.Helper) -> J.JStmt:
  cases(S.Helper) h:
    | helper(name, args, body) =>
      j-var(helper-name(name), j-fun(args.map(js-id-of), body.visit(compiler)))
  end
end

fun compile-split-app(compiler, l, is-var, f, args, name, helper-args):
  when is-var: raise("Can't handle splitting on a var yet");
  e = js-id-of(G.make-name("e"))
  z = js-id-of(G.make-name("z"))
  ss = js-id-of(G.make-name("ss"))
  ret = js-id-of(G.make-name("ret"))
  compiled-f = f.visit(compiler)
  compiled-args = args.map(_.visit(compiler))
  compiled-helper-args = helper-args.map(_.visit(compiler))
  body =
    j-if(j-binop(j-unop(j-dot(j-id("RUNTIME"), "GAS"), j-decr), J.j-gt, j-num(0)),
      j-block([j-expr(j-assign(z, app(compiled-f, compiled-args)))]),
      j-block([
#              j-method(j-id("RUNTIME"), "log", [j-str("Starting, "), j-dot(j-id("RUNTIME"), "EXN_STACKHEIGHT"), obj-of-loc(l), j-str(e)]),
          j-expr(j-dot-assign(j-id("RUNTIME"), "EXN_STACKHEIGHT", j-num(0))),
          j-throw(j-method(j-id("RUNTIME"), "makeCont", 
              [j-obj([j-field("go",
                      j-fun([js-id-of("ignored")], j-block([j-return(app(compiled-f, compiled-args))]))),
                      j-field("from", obj-of-loc(l)),
                      j-field("near", j-str(e))])]))]))
  helper-ids = helper-args.rest.map(_.id).map(js-id-of)
  catch =
    j-block([
#          j-method(j-id("RUNTIME"), "log", [j-str("Catching, "), obj-of-loc(l), j-str(e), j-dot(j-id("RUNTIME"), "EXN_STACKHEIGHT")]),
      j-if(j-method(j-id("RUNTIME"), "isCont", [j-id(e)]),
        j-block([
            j-var(ss,
              j-obj([
                j-field("from", obj-of-loc(l)),
                j-field("near", j-str(e)),
                j-field("captureExn", j-fun(["exn"],
                  j-block([
                      j-return(add-stack-frame(l, "exn"))
                    ]))),
                j-field("go", j-fun([js-id-of(helper-args.first.id)],
                  j-block([
                    j-return(j-app(j-id(helper-name(name)),
                        link(
                            j-id(js-id-of(helper-args.first.id)),
                            helper-ids.map(fun(a): j-id(a);)
                            #helper-ids.map(fun(a): j-dot(j-id("this"), a) end)
                            )))])))]
                #+ helper-ids.map(fun(a): j-field(a, j-id(a)) end)
                )),
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
          ]))])
  j-block([
      j-var(z, j-undefined),
      j-try-catch(body, e, catch),
      j-var(ret, j-app(j-id(helper-name(name)), [j-id(z)] + compiled-helper-args.rest)),
      j-expr(j-unop(j-dot(j-id("RUNTIME"), "GAS"), j-incr)),
      j-return(j-id(ret))
    ])
end


compiler-visitor = {
  a-let(self, l :: Loc, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    compiled-body = body.visit(self)
    j-block(
        link(
            j-var(js-id-of(b.id), e.visit(self)),
            compiled-body.stmts
          )
      )
  end,
  a-var(self, l :: Loc, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    compiled-body = body.visit(self)
    j-block(link(
              j-var(js-id-of(b.id), j-obj([j-field("$var", e.visit(self)), j-field("$name", j-str(js-id-of(b.id)))])),
              compiled-body.stmts))
  end,
  a-split-app(self, l :: Loc, is-var :: Boolean, f :: N.AVal, args :: List<N.AVal>, name :: String, helper-args :: List<N.AVal>):
    compile-split-app(self, l, is-var, f, args, name, helper-args)
  end,
  a-if(self, l :: Loc, cond :: N.AVal, consq :: N.AExpr, alt :: N.AExpr):
    compiled-consq = consq.visit(self)
    compiled-alt = alt.visit(self)
    j-block([
        j-if(j-method(j-id("RUNTIME"), "isPyretTrue", [cond.visit(self)]), compiled-consq, compiled-alt)
      ])
  end,
  a-lettable(self, e :: N.ALettable):
    j-block([j-return(e.visit(self))])
  end,
  a-assign(self, l :: Loc, id :: String, value :: N.AVal):
    j-dot-assign(j-id(js-id-of(id)), "$var", value.visit(self))
  end,
  a-app(self, l :: Loc, f :: N.AVal, args :: List<N.AVal>):
    app(f.visit(self), args.map(_.visit(self)))
  end,

  a-obj(self, l :: Loc, fields :: List<N.AField>):
    j-method(j-id("RUNTIME"), "makeObject", [j-obj(fields.map(fun(f): j-field(f.name, f.value.visit(self));))])
  end,
  a-extend(self, l :: Loc, obj :: N.AVal, fields :: List<N.AField>):
    j-method(obj.visit(self), "extendWith", [j-obj(fields.map(_.visit(self)))])
  end,
  a-dot(self, l :: Loc, obj :: N.AVal, field :: String):
    j-method(j-id("RUNTIME"), "getField", [obj.visit(self), j-str(field)])
  end,
  a-colon(self, l :: Loc, obj :: N.AVal, field :: String):
    j-method(j-id("RUNTIME"), "getColonField", [obj.visit(self), j-str(field)])
  end,
  a-lam(self, l :: Loc, args :: List<N.ABind>, body :: N.AExpr):
    j-method(j-id("RUNTIME"), "makeFunction", [j-fun(args.map(_.id).map(js-id-of), body.visit(self))])
  end,
  a-method(self, l :: Loc, args :: List<N.ABind>, body :: N.AExpr):
    compiled-body = body.visit(self)
    j-method(j-id("RUNTIME"), "makeMethod", [j-fun([js-id-of(args.first.id)],
      j-block([
        j-return(j-fun(args.rest.map(_.id).map(js-id-of), compiled-body))])),
       
      j-fun(args.map(_.id).map(js-id-of), compiled-body)])
  end,
  a-val(self, v :: N.AVal):
    v.visit(self)
  end,
  a-field(self, l :: Loc, name :: String, value :: N.AVal):
    j-field(name, value.visit(self))
  end,
  a-num(self, l :: Loc, n :: Number):
    j-method(j-id("RUNTIME"), "makeNumber", [j-num(n)])
  end,
  a-str(self, l :: Loc, s :: String):
    j-method(j-id("RUNTIME"), "makeString", [j-str(s)])
  end,
  a-bool(self, l :: Loc, b :: Bool):
    str = if b: "pyretTrue" else: "pyretFalse";
    j-dot(j-id("RUNTIME"), str)
  end,
  a-undefined(self, l :: Loc):
    j-undefined
  end,
  a-id(self, l :: Loc, id :: String):
    j-id(js-id-of(id))
  end,
  a-id-var(self, l :: Loc, id :: String):
    j-dot(j-id(js-id-of(id)), "$var")
  end,
  a-id-letrec(self, l :: Loc, id :: String):
    j-dot(j-id(js-id-of(id)), "$var")
  end,

  a-data-expr(self, l, name, variants, shared):
    fun brand-name(base):
      G.make-name("$brand-" + base)
    end

    shared-fields = shared.map(_.visit(self))
    base-brand = brand-name(name)

    fun make-brand-predicate(b :: String, pred-name :: String):
      j-field(
          pred-name,
          rt-method("makeFunction", [
              j-fun(
                  ["val"],
                  j-block([
                    j-return(rt-method("makeBoolean", [rt-method("hasBrand", [j-id("val"), j-str(b)])]))
                  ])
                )
            ])
        )
    end

    fun make-variant-constructor(base-id, brands-id, vname, members):
      member-names = members.map(fun(m): m.bind.id;)
      j-field(
          vname,
          rt-method("makeFunction", [
            j-fun(
              member-names.map(js-id-of),
              j-block([
                  j-var("dict", j-method(j-id("Object"), "create", [j-id(base-id)]))
                ] +
                for map2(n from member-names, m from members):
                    cases(N.AMemberType) m.member-type:
                      | a-normal => j-bracket-assign(j-id("dict"), j-str(n), j-id(js-id-of(n)))
                      | a-cyclic => raise("Cannot handle cyclic fields yet")
                      | a-mutable => raise("Cannot handle mutable fields yet")
                    end
                  end +
                [
                  j-return(rt-method("makeBrandedObject", [j-id("dict"), j-id(brands-id)]))
                ])
              )
          ])
        )
    end

    fun compile-variant(v :: N.AVariant):
      vname = v.name
      variant-base-id = js-id-of(G.make-name(vname + "-base"))
      variant-brand = brand-name(vname)
      variant-brand-obj-id = js-id-of(G.make-name(vname + "-brands"))
      variant-brands = j-obj([
          j-field(base-brand, j-true),
          j-field(variant-brand, j-true)
        ])
      stmts = [
          j-var(variant-base-id, j-obj(shared-fields + v.with-members.map(_.visit(self)))),
          j-var(variant-brand-obj-id, variant-brands)
        ]
      predicate = make-brand-predicate(variant-brand, A.make-checker-name(vname))

      cases(N.AVariant) v:
        | a-variant(_, _, members, with-members) =>
          {
            stmts: stmts,
            constructor: make-variant-constructor(variant-base-id, variant-brand-obj-id, vname, members),
            predicate: predicate
          }
        | a-singleton-variant(_, _, with-members) =>
          {
            stmts: stmts,
            constructor: j-field(vname, rt-method("makeBrandedObject", [j-id(variant-base-id), j-id(variant-brand-obj-id)])),
            predicate: predicate
          }
      end
    end

    variant-pieces = variants.map(compile-variant)

    header-stmts = for fold(acc from [], piece from variant-pieces):
      piece.stmts.reverse() + acc
    end.reverse()
    obj-fields = for fold(acc from [], piece from variant-pieces):
      [piece.constructor] + [piece.predicate] + acc
    end.reverse()

    data-predicate = make-brand-predicate(base-brand, name)

    data-object = rt-method("makeObject", [j-obj([data-predicate] + obj-fields)])

    thunk-app(j-block(header-stmts + [j-return(data-object)]))
  end
}

fun splitting-compiler(env):
  fun inst(id): j-app(j-id(id), [j-id("RUNTIME"), j-id("NAMESPACE")]);
  namespace-ids = env.bindings.filter(CS.is-builtin-id)
  namespace-binds = for map(n from namespace-ids):
    j-var(js-id-of(n.id), j-method(j-id("NAMESPACE"), "get", [j-str(n.id)]))
  end
  compiler-visitor.{
    a-program(self, l, headers, body):
      split = S.ast-split(body)
      ids = headers.map(_.name).map(js-id-of)
      filenames = headers.map(fun(h):
          cases(N.AHeader) h:
            | a-import-builtin(_, name, _) => "trove/" + name
            | a-import-file(_, file, _) => file
          end
        end)
      module-id = G.make-name(l.file)
      module-ref = fun(name): j-bracket(rt-field("modules"), j-str(name));
      input-ids = ids.map(fun(f): G.make-name(f) end)
      j-app(j-id("define"), [j-list(filenames.map(j-str)), j-fun(input-ids, j-block([
                j-return(j-fun(["RUNTIME", "NAMESPACE"],
                    j-block([
                        j-if(module-ref(module-id),
                          j-block([j-return(module-ref(module-id))]),
                          j-block([
                              j-bracket-assign(rt-field("modules"), j-str(module-id), thunk-app(
                                  j-block(
                                    [ j-dot-assign(j-id("RUNTIME"), "EXN_STACKHEIGHT", j-num(0)) ] +
                                    namespace-binds +
                                    for map2(id from ids, in-id from input-ids):
                                      j-var(id, j-method(j-id("RUNTIME"), "getField", [inst(in-id), j-str("provide")]))
                                    end +
                                    split.helpers.map(compile-helper(self, _)) +
                                    [split.body.visit(self)]))),
                              j-return(module-ref(module-id))
                            ]))
                      ])))
            ]))])
    end
  }
end

