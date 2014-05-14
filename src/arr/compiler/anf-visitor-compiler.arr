#lang pyret

provide *
import ast as A
import "compiler/ast-anf.arr" as N
import "compiler/ast-split.arr" as S
import "compiler/js-ast.arr" as J
import "compiler/gensym.arr" as G
import "compiler/compile-structs.arr" as CS
import string-dict as D
import srcloc as SL

j-fun = J.j-fun
j-var = J.j-var
j-id = J.j-id
j-method = J.j-method
j-block = J.j-block
j-true = J.j-true
j-false = J.j-false
j-num = J.j-num
j-str = J.j-str
j-return = J.j-return
j-assign = J.j-assign
j-if = J.j-if
j-if1 = J.j-if1
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
j-neq = J.j-neq
j-unop = J.j-unop
j-decr = J.j-decr
j-incr = J.j-incr
j-ternary = J.j-ternary
j-null = J.j-null
j-parens = J.j-parens

get-field-loc = j-id("G")
throw-uninitialized = j-id("U")
source-name = j-id("M")
undefined = j-id("D")

Loc = SL.Srcloc

js-id-of = block:
  var js-ids = D.string-dict()
  fun(id :: String):
    when not(is-string(id)): raise("js-id-of got non-string: " + torepr(id));
    if js-ids.has-key(id):
      js-ids.get(id)
    else:
      no-hyphens = string-replace(id, "-", "_DASH_")
      safe-id = G.make-name(no-hyphens)
      js-ids.set(id, safe-id)
      safe-id
    end
  end
end

fun compiler-name(id):
  G.make-name("$" + id)
end

fun obj-of-loc(l):
  j-list(false, [list: 
    j-id("M"),
    j-num(l.start-line),
    j-num(l.start-column),
    j-num(l.start-char),
    j-num(l.end-line),
    j-num(l.end-column),
    j-num(l.end-char)
  ])
#  j-obj([list: 
#      j-field("src", j-str(l.source)),
#      j-field("start-line", j-num(l.start-line)),
#      j-field("start-column", j-num(l.start-column)),
#      j-field("start-char", j-num(l.start-char)),
#      j-field("end-line", j-num(l.end-line)),
#      j-field("end-column", j-num(l.end-column)),
#      j-field("end-char", j-num(l.end-char))
#    ])
end

fun get-field(obj, field, loc):
  j-app(get-field-loc, [list: obj, field, loc])
end

fun raise-id-exn(loc, name):
  j-app(throw-uninitialized, [list: loc, j-str(name)])
end

fun add-stack-frame(exn-id, loc):
  j-method(j-dot(j-id(exn-id), "pyretStack"), "push", [list: loc])
end

fun rt-field(name): j-dot(j-id("R"), name);
fun rt-method(name, args): j-method(j-id("R"), name, args);

fun app(l, f, args):
  j-ternary(rt-method("isFunction", [list: f]),
    j-method(f, "app", args),
    j-method(rt-field("ffi"), "throwNonFunApp", [list: obj-of-loc(l), f, j-list(false, args)]))
end

fun thunk-app(block):
  j-app(j-parens(j-fun([list: ], block)), [list: ])
end

fun thunk-app-stmt(stmt):
  thunk-app(j-block([list: stmt]))
end

fun helper-name(s :: String): "$H" + js-id-of(s.tostring());

fun compile-helper(compiler, h :: S.Helper) -> J.JStmt:
  cases(S.Helper) h:
    | helper(name, args, body) =>
      j-var(helper-name(name), j-fun(args.map(_.tostring()).map(js-id-of), body.visit(compiler)))
  end
end

fun compile-tail-app(compiler, l, f, args):
  z = js-id-of(compiler-name("z"))
  compiled-f = f.visit(compiler)
  compiled-args = args.map(_.visit(compiler))
  body =
    j-if(j-binop(j-unop(rt-field("GAS"), j-decr), J.j-gt, j-num(0)),
      j-block([list: j-expr(j-assign(z, app(l, compiled-f, compiled-args)))]),
      j-block([list: 
          j-expr(j-dot-assign(j-id("R"), "EXN_STACKHEIGHT", j-num(0))),
          j-throw(rt-method("makeCont", 
              [list: j-obj([list: j-field("go",
                      j-fun([list: js-id-of("ignored")], j-block([list: j-return(app(l, compiled-f, compiled-args))]))),
                      j-field("from", obj-of-loc(l))])]))]))
  j-block([list: 
      body,
      j-expr(j-unop(rt-field("GAS"), j-incr)),
      j-return(j-id(z))
    ])
end

fun compile-split-app(
    compiler,
    l :: Loc,
    is-var :: Boolean,
    f :: N.AVal,
    args :: List<A.Name>,
    name :: A.Name,
    helper-args :: List<A.AVal>):
  when is-var: raise("Can't handle splitting on a var yet");
  e = js-id-of(compiler-name("e"))
  z = js-id-of(compiler-name("z"))
  ss = js-id-of(compiler-name("ss"))
  ret = js-id-of(compiler-name("ret"))
  f-app = js-id-of(compiler-name("f"))
  compiled-f = f.visit(compiler)
  compiled-args = args.map(_.visit(compiler))
  compiled-helper-args = helper-args.map(_.visit(compiler))
  body =
    j-if(j-binop(j-unop(rt-field("GAS"), j-decr), J.j-gt, j-num(0)),
      j-block([list: j-expr(j-assign(z, j-app(j-id(f-app), [list: ])))]),
      j-block([list: 
#              rt-method("log", [list: j-str("Starting, "), rt-field("EXN_STACKHEIGHT"), obj-of-loc(l), j-str(e)]),
          j-expr(j-dot-assign(j-id("R"), "EXN_STACKHEIGHT", j-num(0))),
          j-throw(rt-method("makeCont", 
              [list: j-obj([list: j-field("go", j-id(f-app)),
                      j-field("from", obj-of-loc(l))])]))]))
  helper-ids = helper-args.rest.map(_.id).map(_.tostring()).map(js-id-of)
  catch =
    j-block([list: 
#          rt-method("log", [list: j-str("Catching, "), obj-of-loc(l), j-str(e), rt-field("EXN_STACKHEIGHT")]),
      j-var("from", obj-of-loc(l)),
      j-if1(rt-method("isCont", [list: j-id(e)]),
        j-block([list: 
            j-var(ss,
              j-obj([list: 
                j-field("from", j-id("from")),
                j-field("go", j-fun([list: js-id-of(helper-args.first.id.tostring())],
                  j-block([list: 
                    j-return(j-app(j-id(helper-name(name)),
                        link(
                            j-id(js-id-of(helper-args.first.id.tostring())),
                            helper-ids.map(fun(a): j-id(a);)
                            #helper-ids.map(fun(a): j-dot(j-id("this"), a) end)
                            )))])))]
                #+ helper-ids.map(fun(a): j-field(a, j-id(a)) end)
                )),
            j-expr(j-bracket-assign(j-dot(j-id(e), "stack"),
                j-unop(rt-field("EXN_STACKHEIGHT"), J.j-postincr), j-id(ss)))
            #j-expr(j-method(j-dot(j-id(e), "stack"), "push", [list: j-id(ss)])),
            ])),
      j-if1(rt-method("isPyretException", [list: j-id(e)]),
          j-block([list: 
              j-expr(add-stack-frame(e, j-id("from")))
            ])),
      j-throw(j-id(e))])
  j-block([list: 
      j-var(z, undefined),
      j-var(f-app, j-fun([list: "_"], j-block([list: j-return(app(l, compiled-f, compiled-args))]))),
      j-try-catch(body, e, catch),
      j-var(ret, j-app(j-id(helper-name(name)), [list: j-id(z)] + compiled-helper-args.rest)),
      j-expr(j-unop(rt-field("GAS"), j-incr)),
      j-return(j-id(ret))
    ])
end


fun arity-check(loc, body-stmts, arity):
  j-block(
    link(
      j-if1(j-binop(j-dot(j-id("arguments"), "length"), j-neq, j-num(arity)),
        j-method(rt-field("ffi"), "throwArityErrorC", [list: obj-of-loc(loc), j-num(arity), j-id("arguments")])),
      body-stmts))
end

compiler-visitor = {
  a-let(self, l :: Loc, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    compiled-body = body.visit(self)
    j-block(
        link(
            j-var(js-id-of(b.id.tostring()), e.visit(self)),
            compiled-body.stmts
          )
      )
  end,
  a-var(self, l :: Loc, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    compiled-body = body.visit(self)
    j-block(link(
              j-var(js-id-of(b.id.tostring()), j-obj([list: j-field("$var", e.visit(self)), j-field("$name", j-str(b.id.toname()))])),
              compiled-body.stmts))
  end,
  a-tail-app(self, l :: Loc, f :: N.AVal, args :: List<N.AVal>):
    compile-tail-app(self, l, f, args)
  end,
  a-split-app(self, l :: Loc, is-var :: Boolean, f :: N.AVal, args :: List<N.AVal>, name :: String, helper-args :: List<N.AVal>):
    compile-split-app(self, l, is-var, f, args, name, helper-args)
  end,
  a-seq(self, l, e1, e2):
    j-block(link(e1.visit(self), e2.visit(self).stmts))
  end,
  a-if(self, l :: Loc, cond :: N.AVal, consq :: N.AExpr, alt :: N.AExpr):
    compiled-consq = consq.visit(self)
    compiled-alt = alt.visit(self)
    j-block([list: 
        j-if(rt-method("isPyretTrue", [list: cond.visit(self)]), compiled-consq, compiled-alt)
      ])
  end,
  a-lettable(self, e :: N.ALettable):
    j-block([list: j-return(e.visit(self))])
  end,
  a-assign(self, l :: Loc, id :: String, value :: N.AVal):
    j-dot-assign(j-id(js-id-of(id.tostring())), "$var", value.visit(self))
  end,
  a-app(self, l :: Loc, f :: N.AVal, args :: List<N.AVal>):
    app(l, f.visit(self), args.map(_.visit(self)))
  end,
  a-prim-app(self, l :: Loc, f :: String, args :: List<N.AVal>):
    rt-method(f, args.map(_.visit(self)))
  end,
  
  a-obj(self, l :: Loc, fields :: List<N.AField>):
    rt-method("makeObject", [list: j-obj(fields.map(fun(f): j-field(f.name, f.value.visit(self));))])
  end,
  a-extend(self, l :: Loc, obj :: N.AVal, fields :: List<N.AField>):
    j-method(obj.visit(self), "extendWith", [list: j-obj(fields.map(_.visit(self)))])
  end,
  a-dot(self, l :: Loc, obj :: N.AVal, field :: String):
    get-field(obj.visit(self), j-str(field), obj-of-loc(l))
  end,
  a-colon(self, l :: Loc, obj :: N.AVal, field :: String):
    rt-method("getColonField", [list: obj.visit(self), j-str(field)])
  end,
  a-lam(self, l :: Loc, args :: List<N.ABind>, body :: N.AExpr):
    rt-method("makeFunction", [list: j-fun(args.map(_.id).map(_.tostring()).map(js-id-of), arity-check(l, body.visit(self).stmts, args.length()))])
  end,
  a-method(self, l :: Loc, args :: List<N.ABind>, body :: N.AExpr):
    compiled-body-stmts = body.visit(self).stmts
    rt-method("makeMethod", [list: j-fun([list: js-id-of(args.first.id.tostring())],
      j-block([list: 
        j-return(j-fun(args.rest.map(_.id).map(_.tostring()).map(js-id-of), arity-check(l, compiled-body-stmts, args.length() - 1)))])),
      #undefined])
      j-fun(args.map(_.id).map(_.tostring()).map(js-id-of), arity-check(l, compiled-body-stmts, args.length()))])
  end,
  a-val(self, v :: N.AVal):
    v.visit(self)
  end,
  a-field(self, l :: Loc, name :: String, value :: N.AVal):
    j-field(name, value.visit(self))
  end,
  a-array(self, l, values):
    j-list(false, values.map(_.visit(self)))
  end,
  a-num(self, l :: Loc, n :: Number):
    if num-is-fixnum(n):
      j-parens(j-num(n))
    else:
      rt-method("makeNumberFromString", [list: j-str(tostring(n))])
    end
  end,
  a-str(self, l :: Loc, s :: String):
    j-parens(j-str(s))
  end,
  a-bool(self, l :: Loc, b :: Bool):
    j-parens(if b: j-true else: j-false end)
  end,
  a-undefined(self, l :: Loc):
    undefined
  end,
  a-id(self, l :: Loc, id :: String):
    j-id(js-id-of(id.tostring()))
  end,
  a-id-var(self, l :: Loc, id :: String):
    j-dot(j-id(js-id-of(id.tostring())), "$var")
  end,
  a-id-letrec(self, l :: Loc, id :: String, safe :: Boolean):
    s = id.tostring()
    if safe:
      j-dot(j-id(js-id-of(s)), "$var")
    else:
      j-ternary(
        j-binop(j-dot(j-id(js-id-of(s)), "$var"), j-eq, undefined),
        raise-id-exn(obj-of-loc(l), id.toname()),
        j-dot(j-id(js-id-of(s)), "$var"))
    end
  end,

  a-data-expr(self, l, name, variants, shared):
    fun brand-name(base):
      compiler-name("brand-" + base)
    end

    shared-fields = shared.map(_.visit(self))
    base-brand = brand-name(name)

    fun make-brand-predicate(b :: String, pred-name :: String):
      j-field(
          pred-name,
          rt-method("makeFunction", [list: 
              j-fun(
                  [list: "val"],
                  j-block([list: 
                    j-return(rt-method("makeBoolean", [list: rt-method("hasBrand", [list: j-id("val"), j-str(b)])]))
                  ])
                )
            ])
        )
    end

    fun make-variant-constructor(l2, base-id, brands-id, vname, members):
      member-names = members.map(fun(m): m.bind.id.toname();)
      j-field(
          vname,
          rt-method("makeFunction", [list: 
            j-fun(
              member-names.map(js-id-of),
              arity-check(
                l2,
                [list: 
                  j-var("dict", rt-method("create", [list: j-id(base-id)]))
                ] +
                for map2(n from member-names, m from members):
                  cases(N.AMemberType) m.member-type:
                    | a-normal => j-bracket-assign(j-id("dict"), j-str(n), j-id(js-id-of(n)))
                    | a-cyclic => raise("Cannot handle cyclic fields yet")
                    | a-mutable => raise("Cannot handle mutable fields yet")
                  end
                end +
                [list: 
                  j-return(rt-method("makeBrandedObject", [list: j-id("dict"), j-id(brands-id)]))
                ],
                member-names.length())
              )
          ])
        )
    end

    fun compile-variant(v :: N.AVariant):
      vname = v.name
      variant-base-id = js-id-of(compiler-name(vname + "-base"))
      variant-brand = brand-name(vname)
      variant-brand-obj-id = js-id-of(compiler-name(vname + "-brands"))
      variant-brands = j-obj([list: 
          j-field(base-brand, j-true),
          j-field(variant-brand, j-true)
        ])
      stmts = [list: 
          j-var(variant-base-id, j-obj(shared-fields + v.with-members.map(_.visit(self)))),
          j-var(variant-brand-obj-id, variant-brands)
        ]
      predicate = make-brand-predicate(variant-brand, A.make-checker-name(vname))

      cases(N.AVariant) v:
        | a-variant(l2, constr-loc, _, members, with-members) =>
          {
            stmts: stmts,
            constructor: make-variant-constructor(constr-loc, variant-base-id, variant-brand-obj-id, vname, members),
            predicate: predicate
          }
        | a-singleton-variant(_, _, with-members) =>
          {
            stmts: stmts,
            constructor: j-field(vname, rt-method("makeBrandedObject", [list: j-id(variant-base-id), j-id(variant-brand-obj-id)])),
            predicate: predicate
          }
      end
    end

    variant-pieces = variants.map(compile-variant)

    header-stmts = for fold(acc from [list: ], piece from variant-pieces):
      piece.stmts.reverse() + acc
    end.reverse()
    obj-fields = for fold(acc from [list: ], piece from variant-pieces):
      [list: piece.constructor] + [list: piece.predicate] + acc
    end.reverse()

    data-predicate = make-brand-predicate(base-brand, name)

    data-object = rt-method("makeObject", [list: j-obj([list: data-predicate] + obj-fields)])

    thunk-app(j-block(header-stmts + [list: j-return(data-object)]))
  end
}

remove-useless-if-visitor = N.default-map-visitor.{
  a-if(self, l, c, t, e):
    cases(N.AVal) c:
      | a-bool(_, test) =>
        if test: t.visit(self) else: e.visit(self) end
      | else => N.a-if(l, c.visit(self), t.visit(self), e.visit(self))
    end
  end
}

check:
  d = N.dummy-loc
  true1 = N.a-if(d, N.a-bool(d, true), N.a-num(d, 1), N.a-num(d, 2))
  true1.visit(remove-useless-if-visitor) is N.a-num(d, 1)

  false4 = N.a-if(d, N.a-bool(d, false), N.a-num(d, 3), N.a-num(d, 4))
  false4.visit(remove-useless-if-visitor) is N.a-num(d, 4)

  N.a-if(d, N.a-id(d, "x"), true1, false4).visit(remove-useless-if-visitor) is
    N.a-if(d, N.a-id(d, "x"), N.a-num(d, 1), N.a-num(d, 4))

end

fun mk-abbrevs(l):
  [list: 
    j-var("G", rt-field("getFieldLoc")),
    j-var("U", j-fun([list: "loc", "name"],
        j-block([list: j-method(rt-field("ffi"), "throwUninitializedIdMkLoc",
                          [list: j-id("loc"), j-id("name")])]))),
    j-var("M", j-str(l.source)),
    j-var("D", rt-field("undefined"))
  ]
end

fun compile-program(self, l, headers, split, env):
  fun inst(id): j-app(j-id(id), [list: j-id("R"), j-id("NAMESPACE")]);
  free-ids = S.freevars-split-result(split).difference(set(headers.map(_.name)))
  namespace-binds = for map(n from free-ids.to-list()):
    j-var(js-id-of(n.tostring()), j-method(j-id("NAMESPACE"), "get", [list: j-str(n.toname())]))
  end
  ids = headers.map(_.name).map(_.tostring()).map(js-id-of)
  filenames = headers.map(fun(h):
      cases(N.AHeader) h:
        | a-import-builtin(_, name, _) => "trove/" + name
        | a-import-file(_, file, _) => file
      end
    end)
  module-id = compiler-name(l.source)
  module-ref = fun(name): j-bracket(rt-field("modules"), j-str(name));
  input-ids = ids.map(fun(f): compiler-name(f) end)
  fun wrap-modules(modules, body):
    mod-input-ids = modules.map(fun(f): j-id(f.input-id) end)
    mod-ids = modules.map(_.id)
    j-return(rt-method("loadModules",
        [list: j-id("NAMESPACE"), j-list(false, mod-input-ids),
          j-fun(mod-ids,
            j-block([list: 
                j-return(rt-method(
                    "safeCall", [list: 
                      j-fun([list: ], j-block([list: body])),
                      j-fun([list: "moduleVal"],
                        j-block([list: 
                            j-bracket-assign(rt-field("modules"), j-str(module-id), j-id("moduleVal")),
                            j-return(j-id("moduleVal"))
                    ]))]))]))]))
  end
  module-specs = for map2(id from ids, in-id from input-ids):
    { id: id, input-id: in-id }
  end
  j-app(j-id("define"), [list: j-list(true, filenames.map(j-str)), j-fun(input-ids, j-block([list: 
            j-return(j-fun([list: "R", "NAMESPACE"],
                j-block([list: 
                    j-if(module-ref(module-id),
                      j-block([list: j-return(module-ref(module-id))]),
                      j-block(mk-abbrevs(l) +
                              namespace-binds +
                              split.helpers.map(compile-helper(self, _)) +
                              [list: wrap-modules(module-specs, split.body.visit(self))]))])))]))])
end

fun splitting-compiler(env):
  compiler-visitor.{
    a-program(self, l, headers, body):
      simplified = body.visit(remove-useless-if-visitor)
      split = S.ast-split(simplified)
      compile-program(self, l, headers, split, env)
    end
  }
end

fun non-splitting-compiler(env):
  compiler-visitor.{
    a-program(self, l, headers, body):
      simplified = body.visit(remove-useless-if-visitor)
      split = S.split-result([list: ], simplified, N.freevars-e(simplified))
      compile-program(self, l, headers, split, env)
    end
  }
end

