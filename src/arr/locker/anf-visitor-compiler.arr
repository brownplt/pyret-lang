#lang pyret

provide *
provide-types *

import ast as A
import "compiler/ast-anf.arr" as N
import "compiler/ast-split.arr" as S
import "compiler/js-ast.arr" as J
import "compiler/gensym.arr" as G
import "compiler/compile-structs.arr" as CS
import "compiler/concat-lists.arr" as CL
import string-dict as D
import srcloc as SL

type Loc = SL.Srcloc

type ConcatList = CL.ConcatList

concat-empty = CL.concat-empty
concat-snoc = CL.concat-snoc

fun type-name(str):
  "$type$" + str
end


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


js-id-of = block:
  var js-ids = D.string-dict()
  lam(id :: String):
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
    j-method(rt-field("ffi"), "throwNonFunApp", [list: l, f, j-list(false, args)]))
end

fun thunk-app(block):
  j-app(j-parens(j-fun([list: ], block)), [list: ])
end

fun thunk-app-stmt(stmt):
  thunk-app(j-block([list: stmt]))
end

fun helper-name(s :: A.Name): "$H" + js-id-of(s.tostring());

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
      j-block([list: j-expr(j-assign(z, app(compiler.get-loc(l), compiled-f, compiled-args)))]),
      j-block([list: 
          j-expr(j-dot-assign(j-id("R"), "EXN_STACKHEIGHT", j-num(0))),
          j-throw(rt-method("makeCont", 
              [list: j-obj([list: j-field("go",
                      j-fun([list: js-id-of("ignored")], j-block([list: j-return(app(compiler.get-loc(l), compiled-f, compiled-args))]))),
                      j-field("from", compiler.get-loc(l))])]))]))
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
#              rt-method("log", [list: j-str("Starting, "), rt-field("EXN_STACKHEIGHT"), compiler.get-loc(l), j-str(e)]),
          j-expr(j-dot-assign(j-id("R"), "EXN_STACKHEIGHT", j-num(0))),
          j-throw(rt-method("makeCont", 
              [list: j-obj([list: j-field("go", j-id(f-app)),
                      j-field("from", compiler.get-loc(l))])]))]))
  helper-ids = helper-args.rest.map(_.id).map(_.tostring()).map(js-id-of)
  catch =
    j-block([list: 
#          rt-method("log", [list: j-str("Catching, "), compiler.get-loc(l), j-str(e), rt-field("EXN_STACKHEIGHT")]),
      j-if1(rt-method("isCont", [list: j-id(e)]),
        j-block([list: 
            j-var(ss,
              j-obj([list: 
                j-field("from", compiler.get-loc(l)),
                j-field("go", j-fun([list: js-id-of(helper-args.first.id.tostring())],
                  j-block([list: 
                    j-return(j-app(j-id(helper-name(name)),
                        link(
                            j-id(js-id-of(helper-args.first.id.tostring())),
                            helper-ids.map(lam(a): j-id(a);)
                            #helper-ids.map(lam(a): j-dot(j-id("this"), a) end)
                            )))])))]
                #+ helper-ids.map(lam(a): j-field(a, j-id(a)) end)
                )),
            j-expr(j-bracket-assign(j-dot(j-id(e), "stack"),
                j-unop(rt-field("EXN_STACKHEIGHT"), J.j-postincr), j-id(ss)))
            #j-expr(j-method(j-dot(j-id(e), "stack"), "push", [list: j-id(ss)])),
            ])),
      j-if1(rt-method("isPyretException", [list: j-id(e)]),
          j-block([list: 
              j-expr(add-stack-frame(e, compiler.get-loc(l)))
            ])),
      j-throw(j-id(e))])
  j-block([list: 
      j-var(z, undefined),
      j-var(f-app, j-fun([list: "_"], j-block([list: j-return(app(compiler.get-loc(l), compiled-f, compiled-args))]))),
      j-try-catch(body, e, catch),
      j-var(ret, j-app(j-id(helper-name(name)), [list: j-id(z)] + compiled-helper-args.rest)),
      j-expr(j-unop(rt-field("GAS"), j-incr)),
      j-return(j-id(ret))
    ])
end


fun compile-ann(ann, visitor):
  cases(A.Ann) ann:
    | a-name(_, n) => j-id(js-id-of(n.tostring()))
    | a-arrow(_, _, _, _) => rt-field("Function")
    | a-method(_, _, _) => rt-field("Method")
    | a-app(l, base, _) => compile-ann(base, visitor)
    | a-record(l, fields) =>
      names = j-list(false, fields.map(_.name).map(j-str))
      locs = j-list(false, fields.map(_.l).map(visitor.get-loc))
      anns = for map(f from fields):
        j-field(f.name, compile-ann(f.ann, visitor))
      end
      rt-method("makeRecordAnn", [list:
          names,
          locs,
          j-obj(anns)
        ])
    | a-pred(l, base, exp) =>
      name = cases(A.AExpr) exp:
        | s-id(_, id) => id.toname()
        | s-id-letrec(_, id, _) => id.toname()
      end
      expr-to-compile = cases(A.Expr) exp:
        | s-id(l2, id) => N.a-id(l2, id)
        | s-id-letrec(l2, id, ok) => N.a-id-letrec(l2, id, ok)
      end
      rt-method("makePredAnn", [list: compile-ann(base, visitor), expr-to-compile.visit(visitor), j-str(name)])
    | a-dot(l, m, field) =>
      rt-method("getDotAnn", [list: visitor.get-loc(l), j-str(m.toname()), j-id(js-id-of(m.tostring())), j-str(field)])
    | a-blank => rt-field("Any")
    | a-any => rt-field("Any")
  end
end

fun arity-check(loc-expr, body-stmts, arity):
  j-block(
    link(
      j-if1(j-binop(j-dot(j-id("arguments"), "length"), j-neq, j-num(arity)),
        j-block([list:
          j-throw(j-method(rt-field("ffi"), "throwArityErrorC", [list: loc-expr, j-num(arity), j-id("arguments")]))
        ])),
      body-stmts))
end

fun contract-checks(args, ret, stmts, visitor):
  nonblanks = for filter(a from args): not(A.is-a-blank(a.ann)) and not(A.is-a-any(a.ann)) end
  if is-empty(nonblanks):
    stmts
  else:
    cont = j-fun([list:], j-block(stmts))
    anns = for map(a from nonblanks): compile-ann(a.ann, visitor) end
    locs = for map(a from nonblanks): visitor.get-loc(a.ann.l) end
    vals = for map(a from nonblanks): j-id(js-id-of(a.id.tostring())) end
    [list: j-return(rt-method("checkAnnArgs", [list: # FILL
      j-list(false, anns),
      j-list(false, vals),
      j-list(false, locs),
      cont
    ]))]
  end
end

compiler-visitor = {
  a-module(self, l, answer, provides, types, checks):
    types-obj-fields = for map(ann from types):
      j-field(ann.name, compile-ann(ann.ann, self))
    end
    rt-method("makeObject", [list:
        j-obj([list:
            j-field("answer", answer.visit(self)),
            j-field("provide-plus-types",
              rt-method("makeObject", [list: j-obj([list:
                      j-field("values", provides.visit(self)),
                      j-field("types", j-obj(types-obj-fields))
                    ])])),
            j-field("checks", checks.visit(self))])])
  end,
  a-type-let(self, l, bind, body):
    cases(N.ATypeBind) bind:
      | a-type-bind(l2, name, ann) =>
        j-block(
          [list: j-var(js-id-of(name.tostring()), compile-ann(ann, self))] +
          body.visit(self).stmts
        )
      | a-newtype-bind(l2, name, nameb) =>
        brander-id = js-id-of(nameb.tostring())
        j-block(
          [list:
            j-var(brander-id, rt-method("namedBrander", [list: j-str(name.toname())])),
            j-var(js-id-of(name.tostring()), rt-method("makeBranderAnn", [list: j-id(brander-id), j-str(name.toname())]))
          ] +
          body.visit(self).stmts)
    end
  end,
  a-let(self, l :: Loc, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    compiled-body = body.visit(self)
    if A.is-a-blank(b.ann) or A.is-a-any(b.ann):
      j-block(
        [list: j-var(js-id-of(b.id.tostring()), e.visit(self))] +
        compiled-body.stmts)
    else:
      cont = j-fun([list:], compiled-body)
      j-block([list:
        j-var(js-id-of(b.id.tostring()), e.visit(self)),
        j-return(rt-method("checkAnn", [list: # FILL
          self.get-loc(b.ann.l),
          compile-ann(b.ann, self),
          j-id(js-id-of(b.id.tostring())),
          cont
        ]))
      ])
    end
      
#     ann-id = js-id-of("ann")
#     existing-ann = j-var(ann-id, rt-method("getAnnIfCached", [list: M, "$ann_" + b.id.tostring()]))
#     
#     j-if1(not(j-id(ann-id)),
#       ann-id = rt-method("makeAndSaveAnn", [list: b.ann])
#       )
#     j-var(js-id-of(b.id.tostring()), rt-method("checkAnn", [list: ann-id, e.visit(self)]))

#       ])
#     j-var(js-id-of(make-name("ann")),
#       if ann exists and ann is not refinement:
#         use ann
#       elseboth:
#         makeAnn
#       end
#   end
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
  a-split-app(self, l :: Loc, is-var :: Boolean, f :: N.AVal, args :: List<N.AVal>, name :: A.Name, helper-args :: List<N.AVal>):
    compile-split-app(self, l, is-var, f, args, name, helper-args)
  end,
  a-seq(self, l, e1, e2):
    e1-visit = e1.visit(self)
    if J.JStmt(e1-visit):
      j-block(link(e1-visit, e2.visit(self).stmts))
    else:
      j-block(link(j-expr(e1-visit), e2.visit(self).stmts))
    end
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
  a-assign(self, l :: Loc, id :: A.Name, value :: N.AVal):
    j-dot-assign(j-id(js-id-of(id.tostring())), "$var", value.visit(self))
  end,
  a-app(self, l :: Loc, f :: N.AVal, args :: List<N.AVal>):
    app(self.get-loc(l), f.visit(self), args.map(_.visit(self)))
  end,
  a-prim-app(self, l :: Loc, f :: String, args :: List<N.AVal>):
    rt-method(f, args.map(_.visit(self)))
  end,
  
  a-obj(self, l :: Loc, fields :: List<N.AField>):
    rt-method("makeObject", [list: j-obj(fields.map(lam(f): j-field(f.name, f.value.visit(self));))])
  end,
  a-extend(self, l :: Loc, obj :: N.AVal, fields :: List<N.AField>):
    j-method(obj.visit(self), "extendWith", [list: j-obj(fields.map(_.visit(self)))])
  end,
  a-dot(self, l :: Loc, obj :: N.AVal, field :: String):
    get-field(obj.visit(self), j-str(field), self.get-loc(l))
  end,
  a-colon(self, l :: Loc, obj :: N.AVal, field :: String):
    rt-method("getColonField", [list: obj.visit(self), j-str(field)])
  end,
  a-lam(self, l :: Loc, args :: List<N.ABind>, ret, body :: N.AExpr):
    rt-method("makeFunction", [list: j-fun(args.map(_.id).map(_.tostring()).map(js-id-of),
          arity-check(self.get-loc(l), contract-checks(args, ret, body.visit(self).stmts, self), args.length()))])
  end,
  a-method(self, l :: Loc, args :: List<N.ABind>, ret, body :: N.AExpr):
    compiled-body-stmts = contract-checks(args, ret, body.visit(self).stmts, self)
    rt-method("makeMethod", [list: j-fun([list: js-id-of(args.first.id.tostring())],
      j-block([list: 
        j-return(j-fun(args.rest.map(_.id).map(_.tostring()).map(js-id-of), arity-check(
                    self.get-loc(l), compiled-body-stmts, args.length() - 1)))])),
      j-obj([list: j-field("length", j-num(args.length()))])])
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
  a-srcloc(self, l, loc):
    self.get-loc(loc)
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
  a-bool(self, l :: Loc, b :: Boolean):
    j-parens(if b: j-true else: j-false end)
  end,
  a-undefined(self, l :: Loc):
    undefined
  end,
  a-id(self, l :: Loc, id :: A.Name):
    j-id(js-id-of(id.tostring()))
  end,
  a-id-var(self, l :: Loc, id :: A.Name):
    j-dot(j-id(js-id-of(id.tostring())), "$var")
  end,
  a-id-letrec(self, l :: Loc, id :: A.Name, safe :: Boolean):
    s = id.tostring()
    if safe:
      j-dot(j-id(js-id-of(s)), "$var")
    else:
      j-ternary(
        j-binop(j-dot(j-id(js-id-of(s)), "$var"), j-eq, undefined),
        raise-id-exn(self.get-loc(l), id.toname()),
        j-dot(j-id(js-id-of(s)), "$var"))
    end
  end,

  a-data-expr(self, l, name, namet, variants, shared):
    fun brand-name(base):
      compiler-name("brand-" + base)
    end

    shared-fields = shared.map(_.visit(self))
    external-brand = j-id(js-id-of(namet.tostring()))

    fun make-brand-predicate(loc :: Loc, b :: J.JExpr, pred-name :: String):
      j-field(
          pred-name,
          rt-method("makeFunction", [list: 
              j-fun(
                  [list: "val"],
                  arity-check(self.get-loc(loc), [list:
                      j-return(rt-method("makeBoolean", [list: rt-method("hasBrand", [list: j-id("val"), b])]))
                    ], 1)
                )
            ])
        )
    end

    fun make-variant-constructor(l2, base-id, brands-id, vname, members):
      member-names = members.map(lam(m): m.bind.id.toname();)
      member-ids = members.map(lam(m): m.bind.id.tostring();)
      j-field(
          vname,
          rt-method("makeFunction", [list: 
            j-fun(
              member-ids.map(js-id-of),
              arity-check(
                self.get-loc(l2),
                contract-checks(
                  members.map(_.bind),
                  A.a-blank,
                  [list: 
                    j-var("dict", rt-method("create", [list: j-id(base-id)]))
                  ] +
                  for map3(n from member-names, m from members, id from member-ids):
                    cases(N.AMemberType) m.member-type:
                      | a-normal => j-bracket-assign(j-id("dict"), j-str(n), j-id(js-id-of(id)))
                      | a-mutable => raise("Cannot handle mutable fields yet")
                    end
                  end +
                  [list: 
                    j-return(rt-method("makeBrandedObject", [list: j-id("dict"), j-id(brands-id)]))
                  ],
                  self
                ),
                member-names.length()
              )
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
          j-field(variant-brand, j-true)
        ])
      stmts = [list: 
          j-var(variant-base-id, j-obj(shared-fields + v.with-members.map(_.visit(self)))),
          j-var(variant-brand-obj-id, variant-brands),
          j-bracket-assign(
            j-id(variant-brand-obj-id),
            j-dot(external-brand, "_brand"),
            j-true)
        ]
      predicate = make-brand-predicate(v.l, j-str(variant-brand), A.make-checker-name(vname))

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

    data-predicate = make-brand-predicate(l, j-dot(external-brand, "_brand"), name)

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
  e = lam(v): N.a-lettable(N.a-val(v));
  true1 = N.a-if(d, N.a-bool(d, true), e(N.a-num(d, 1)), e(N.a-num(d, 2)))
  true1.visit(remove-useless-if-visitor) is e(N.a-num(d, 1))

  false4 = N.a-if(d, N.a-bool(d, false), e(N.a-num(d, 3)), e(N.a-num(d, 4)))
  false4.visit(remove-useless-if-visitor) is e(N.a-num(d, 4))


  n = A.global-names.make-atom

  x = n("x")
  N.a-if(d, N.a-id(d, x), true1, false4).visit(remove-useless-if-visitor) is
    N.a-if(d, N.a-id(d, x), e(N.a-num(d, 1)), e(N.a-num(d, 4)))

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
  free-ids = S.freevars-split-result(split).difference(sets.list-to-tree-set(headers.map(_.name))).difference(sets.list-to-tree-set(headers.map(_.types)))
  namespace-binds = for map(n from free-ids.to-list()):
    bind-name = 
      cases(A.Name) n:
        | s-global(s) => n.toname()
        | s-type-global(s) => type-name(n.toname())
      end
    j-var(js-id-of(n.tostring()), j-method(j-id("NAMESPACE"), "get", [list: j-str(bind-name)]))
  end
  ids = headers.map(_.name).map(_.tostring()).map(js-id-of)
  type-imports = headers.filter(N.is-a-import-types)
  type-ids = type-imports.map(_.types).map(_.tostring()).map(js-id-of)
  filenames = headers.map(lam(h):
      cases(N.AHeader) h.import-type:
        | a-import-builtin(_, name) => "trove/" + name
        | a-import-file(_, file) => file
      end
    end)
  module-id = compiler-name(l.source)
  module-ref = lam(name): j-bracket(rt-field("modules"), j-str(name));
  input-ids = ids.map(lam(f): compiler-name(f) end)
  fun wrap-modules(modules, body):
    mod-input-names = modules.map(_.input-id)
    mod-input-ids = mod-input-names.map(j-id)
    mod-val-ids = modules.map(_.id)
    j-return(rt-method("loadModulesNew",
        [list: j-id("NAMESPACE"), j-list(false, mod-input-ids),
          j-fun(mod-input-names,
            j-block(
              for map2(m from mod-val-ids, in from mod-input-ids):
                j-var(m, rt-method("getField", [list: in, j-str("values")]))
              end +
              for map2(mt from type-ids, in from mod-input-ids):
                j-var(mt, rt-method("getField", [list: in, j-str("types")]))
              end +
              [list: 
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
  var locations = concat-empty
  var loc-count = 0
  var loc-cache = D.string-dict()
  locs = "L"
  fun get-loc(shadow l :: Loc):
    as-str = torepr(l)
    if loc-cache.has-key(as-str):
      loc-cache.get(as-str)
    else:
      ans = j-bracket(j-id(locs), j-num(loc-count))
      loc-cache.set(as-str, ans)
      loc-count := loc-count + 1
      locations := concat-snoc(locations, obj-of-loc(l))
      ans
    end
  end
  visited-helpers = split.helpers.map(compile-helper(self.{get-loc: get-loc}, _))
  visited-body = split.body.visit(self.{get-loc: get-loc})
  define-locations = j-var(locs, j-list(true, locations.to-list()))
  j-app(j-id("define"), [list: j-list(true, filenames.map(j-str)), j-fun(input-ids, j-block([list: 
            j-return(j-fun([list: "R", "NAMESPACE"],
                j-block([list: 
                    j-if(module-ref(module-id),
                      j-block([list: j-return(module-ref(module-id))]),
                      j-block(mk-abbrevs(l) +
                              [list: define-locations] + 
                              namespace-binds +
                              visited-helpers +
                              [list: wrap-modules(module-specs, visited-body)]))])))]))])
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

