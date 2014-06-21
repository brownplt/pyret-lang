#lang pyret

provide *
provide-types *
import ast as A
import srcloc as S
import parse-pyret as PP
import string-dict as SD
import "compiler/compile-structs.arr" as C
import "compiler/ast-util.arr" as U
import "compiler/gensym.arr" as G

data NameResolution:
  | resolved(ast :: A.Program, errors :: List<C.CompileError>,
      bindings :: SD.StringDict, type-bindings :: SD.StringDict)
end

fun mk-bind(l, id) -> A.Expr: A.s-bind(l, false, id, A.a-blank);

fun mk-id(loc, base) -> { id :: A.Expr, id-b :: A.Expr, id-e :: A.Expr }:
  t = A.s-name(loc, base)
  { id: t, id-b: mk-bind(loc, t), id-e: A.s-id(loc, t) }
end

fun resolve-provide(p :: A.Provide, b :: A.Expr) -> A.Provide:
  cases(A.Provide) p:
    | s-provide-all(l) =>
      ids = A.block-ids(b)
      obj = A.s-obj(l, for map(id from ids): A.s-data-field(l, A.s-str(l, tostring(id)), A.s-id(l, id)) end)
      A.s-provide(l, obj)
    | else => p
  end
end

fun resolve-type-provide(p :: A.ProvideTypes, b :: A.Expr) -> A.ProvideTypes:
  cases(A.ProvideTypes) p:
    | s-provide-types-all(l) =>
      ids = A.block-type-ids(b)
      type-fields = for map(id from ids):
        if id.bind-type == "data":
          A.a-field(l, id.name.toname(), A.a-name(l, id.name))
        else:
          A.a-field(l, id.name.toname(), A.a-name(l, id.name))
        end
      end
      A.s-provide-types(l, type-fields)
    | else => p
  end
end

fun resolve-imports(imports :: List<A.Import>):
  fun resolve-import-type(imp :: A.ImportType):
    cases(A.ImportType) imp:
      | s-file-import(l, file) =>
        if string-contains(file, "/"): imp
        else: A.s-file-import(l, "./" + file)
        end
      | s-const-import(_, _) => imp
    end
  end
  ret = for fold(acc from {imports: [list: ], lets: [list: ]}, i from imports):
    cases(A.Import) i:
      | s-import(l, imp, name) =>
        new-i = A.s-import(l, resolve-import-type(imp), name)
        acc.{imports: link(new-i, acc.imports)}
      | s-import-fields(l, fields, imp) =>
        imp-str = if A.is-s-const-import(imp): imp.mod else: "mod-import" end
        imp-name = A.s-name(imp.l, G.make-name(imp-str))
        new-i = A.s-import(l, resolve-import-type(imp), imp-name)
        new-lets = for map(f from fields.reverse()):
          A.s-let(f.l, A.s-bind(l, false, f, A.a-blank), A.s-dot(l, A.s-id(l, imp-name), f.tostring()), false)
        end
        acc.{imports: link(new-i, acc.imports), lets: new-lets + acc.lets}
    end
  end
  { imports: ret.imports.reverse(), lets: ret.lets.reverse() }
end

fun desugar-toplevel-types(stmts :: List<A.Expr>) -> List<A.Expr>:
  doc: ```
       Treating stmts as a toplevel block, hoist any type-lets or newtype declarations
       to the top, turning them into a type-let-expression, and generate newtypes for all
       data expressions.
       ```
  var rev-type-binds = empty
  var rev-stmts = empty
  for lists.each(s from stmts):
    cases(A.Expr) s:
      | s-type(l, name, ann) =>
        rev-stmts := link(s, rev-stmts)
      | s-newtype(l, name, namet) =>
        rev-type-binds := link(A.s-newtype-bind(l, name, namet), rev-type-binds)
      | s-data(l, name, params, mixins, variants, shared, _check) =>
        namet = G.make-name(name + "T")
        rev-type-binds := link(A.s-newtype-bind(l, A.s-name(l, name), A.s-name(l, namet)), rev-type-binds)
        rev-stmts := link(A.s-data-expr(l, name, A.s-name(l, namet), params, mixins, variants, shared, _check), rev-stmts)
      | else =>
        rev-stmts := link(s, rev-stmts)
    end
  end
  if is-empty(rev-type-binds):
    stmts
  else:
    type-binds = rev-type-binds.reverse()
    new-stmts = rev-stmts.reverse()
    [list: A.s-type-let-expr(type-binds.first.l, type-binds, A.s-block(type-binds.first.l, new-stmts))]
  end
end


fun desugar-scope-block(stmts, let-binds, letrec-binds, type-let-binds) -> List<A.Expr>:
  doc: ```
       Treating stmts as a block, resolve scope.
       There should be no blocks left after this stage of the compiler pipeline.
       ```
  cases(List) stmts:
    | empty => empty
    | link(f, rest-stmts) =>
      fun wrap-letrecs(expr):
        A.s-letrec(letrec-binds.first.l, letrec-binds.reverse(), expr)
      end
      fun wrap-lets(expr):
        A.s-let-expr(let-binds.first.l, let-binds.reverse(), expr)
      end
      fun wrap-type-lets(expr):
        A.s-type-let-expr(type-let-binds.first.l, type-let-binds.reverse(), expr)
      end
      fun handle-let-bind(l, new-bind):
        new-binds = link(new-bind, let-binds)
        resolved-inner = desugar-scope-block(rest-stmts, new-binds, [list: ], [list: ])
        if not(is-empty(letrec-binds)):
          [list: wrap-letrecs(A.s-block(l, resolved-inner))]
        else if not(is-empty(type-let-binds)):
          [list: wrap-type-lets(A.s-block(l, resolved-inner))]
        else:
          resolved-inner
        end
      end
      wrapper = 
        if is-link(let-binds): wrap-lets
        else if is-link(letrec-binds): wrap-letrecs
        else if is-link(type-let-binds): wrap-type-lets
        else: lam(e): e;
        end
      cases(A.Expr) f:
        | s-type(l, name, ann) =>
          new-type-lets = link(A.s-type-bind(l, name, ann), type-let-binds)
          resolved-inner = desugar-scope-block(rest-stmts, [list: ], [list: ], new-type-lets)
          if not(is-empty(let-binds)):
            [list: wrap-lets(A.s-block(l, resolved-inner))]
          else if not(is-empty(letrec-binds)):
            [list: wrap-letrecs(A.s-block(l, resolved-inner))]
          else:
            resolved-inner
          end
        | s-let(l, bind, expr, _) =>
          handle-let-bind(l, A.s-let-bind(l, bind, expr))
        | s-var(l, bind, expr) =>
          handle-let-bind(l, A.s-var-bind(l, bind, expr))
        | s-fun(l, name, params, args, ann, doc, body, _check) =>
          new-letrecs = link(A.s-letrec-bind(
              l,
              A.s-bind(l, false, A.s-name(l, name), A.a-blank),
              A.s-lam(l, params, args, ann, doc, body, _check)
            ), letrec-binds)
          resolved-inner = desugar-scope-block(rest-stmts, [list: ], new-letrecs, [list: ])
          if not(is-empty(let-binds)):
            [list: wrap-lets(A.s-block(l, resolved-inner))]
          else if not(is-empty(type-let-binds)):
            [list: wrap-type-lets(A.s-block(l, resolved-inner))]
          else:
            resolved-inner
          end
        | s-data-expr(l, name, namet, params, mixins, variants, shared, _check) =>
          fun b(loc, id :: String): A.s-bind(loc, false, A.s-name(l, id), A.a-blank);
          fun variant-binds(data-blob-id, v):
            vname = v.name
            checker-name = A.make-checker-name(vname)
            get-part = A.s-dot(v.l, data-blob-id, _)
            [list:
              A.s-letrec-bind(v.l, b(v.l, vname), get-part(vname)),
              A.s-letrec-bind(v.l, b(v.l, checker-name), get-part(checker-name))
            ]
          end
          blob-id = G.make-name(name)
          data-expr = A.s-data-expr(l, name, namet, params, mixins, variants, shared, _check)
          bind-data = A.s-letrec-bind(l, b(l, blob-id), data-expr)
          bind-data-pred = A.s-letrec-bind(l, b(l, A.make-checker-name(name)), A.s-dot(l, A.s-id(l, A.s-name(l, blob-id)), name))
          bind-data-pred2 = A.s-letrec-bind(l, b(l, name), A.s-dot(l, A.s-id(l, A.s-name(l, blob-id)), name))
          all-binds = for fold(acc from [list: bind-data-pred, bind-data-pred2, bind-data], v from variants):
            variant-binds(A.s-id(l, A.s-name(l, blob-id)), v) + acc
          end

          if is-empty(letrec-binds):
            [list: wrapper(A.s-block(l, desugar-scope-block(rest-stmts, [list: ], all-binds, [list: ])))]
          else:
            desugar-scope-block(rest-stmts, [list: ], all-binds + letrec-binds, [list: ])
          end
        | s-contract(l, name, ann) =>
          desugar-scope-block(rest-stmts, let-binds, letrec-binds, type-let-binds)
        | else =>
          cases(List) rest-stmts:
            | empty => [list: wrapper(f)]
            | link(_, _) =>
              if not(is-link(let-binds) or is-link(letrec-binds) or is-link(type-let-binds)):
                link(f, desugar-scope-block(rest-stmts, [list: ], [list: ], [list: ]))
              else:
                [list: wrapper(A.s-block(f.l,
                  link(f, desugar-scope-block(rest-stmts, [list: ], [list: ], [list: ]))))]
              end
          end
      end
  end
where:
  p = lam(str): PP.surface-parse(str, "test").block;
  d = A.dummy-loc
  b = lam(s): A.s-bind(d, false, A.s-name(d, s), A.a-blank);
  id = lam(s): A.s-id(d, A.s-name(d, s));
  bk = lam(e): A.s-block(d, [list: e]) end
  bs = lam(str):
    A.s-block(d, desugar-scope-block(p(str).stmts, [list: ], [list: ], [list: ])).visit(A.dummy-loc-visitor)
  end
  n = none
  thunk = lam(e): A.s-lam(d, [list: ], [list: ], A.a-blank, "", bk(e), n) end


  compare1 = A.s-let-expr(d, [list: A.s-let-bind(d, b("x"), A.s-num(d, 15)),
                                      A.s-let-bind(d, b("y"), A.s-num(d, 10))],
                        id("y"))
  desugar-scope-block(p("x = 15 y = 10 y").stmts, empty, empty, empty).first.visit(A.dummy-loc-visitor)
    is compare1

  desugar-scope-block(p("x = 55 var y = 10 y").stmts, empty, empty, empty).first.visit(A.dummy-loc-visitor)
    is A.s-let-expr(d, [list: A.s-let-bind(d, b("x"), A.s-num(d, 55)),
      A.s-var-bind(d, b("y"), A.s-num(d, 10))], id("y"))

  bs("x = 7 print(2) var y = 10 y") is A.s-block(d,
    [list: A.s-let-expr(d, [list:A.s-let-bind(d, b("x"), A.s-num(d, 7))],
        A.s-block(d, [list:
            A.s-app(d, id("print"), [list:A.s-num(d, 2)]),
            A.s-let-expr(d, [list:A.s-var-bind(d, b("y"), A.s-num(d, 10))],
              id("y"))
        ]))])

  prog = bs("fun f(): 4 end fun g(): 5 end f()")
  prog is A.s-block(d,
    [list: A.s-letrec(d, [list:
          A.s-letrec-bind(d, b("f"), thunk(A.s-num(d, 4))),
          A.s-letrec-bind(d, b("g"), thunk(A.s-num(d, 5)))
        ],
        A.s-app(d, id("f"), [list: ]))
    ])

  p-s = lam(e): A.s-app(d, id("print"), [list: e]);
  pretty = lam(e): e.tosource().pretty(80).join-str("\n");

  prog2 = bs("print(1) fun f(): 4 end fun g(): 5 end fun h(): 6 end x = 3 print(x)")
  prog2 is A.s-block(d,
    [list: p-s(A.s-num(d, 1)),
      A.s-letrec(d, [list:
          A.s-letrec-bind(d, b("f"), thunk(A.s-num(d, 4))),
          A.s-letrec-bind(d, b("g"), thunk(A.s-num(d, 5))),
          A.s-letrec-bind(d, b("h"), thunk(A.s-num(d, 6)))
        ],
        A.s-block(d, [list:
            A.s-let-expr(d, [list: A.s-let-bind(d, b("x"), A.s-num(d, 3))], p-s(id("x")))
        ]))])

  desugar-scope-block([list: prog2], [list: ], [list: ], [list: ]).first is prog2
  for each2(p1 from desugar-scope-block(prog2.stmts, [list: ], [list: ], [list: ]), p2 from prog2.stmts):
    p1.visit(A.dummy-loc-visitor) is p2
  end

  prog3 = bs("print(x) x := 3 print(x)")
  prog3 is A.s-block(d,
    [list:
      p-s(id("x")),
      A.s-assign(d, A.s-name(d, "x"), A.s-num(d, 3)),
      p-s(id("x"))
    ])
  
  prog4 = bs("var x = 10 fun f(): 4 end f()")
  prog4 is A.s-block(d, [list:
      A.s-let-expr(d, [list:
          A.s-var-bind(d, b("x"), A.s-num(d, 10))
        ],
        A.s-block(d, [list:
            A.s-letrec(d, [list:
                A.s-letrec-bind(d, b("f"), thunk(A.s-num(d, 4)))
              ],
              A.s-app(d, id("f"), [list: ]))
        ]))])

  #prog5 = bs("data List: empty | link(f, r) end empty")
  #prog5.stmts.length() is 1
  #the-let = prog5.stmts.first
  #the-let satisfies A.is-s-let-expr
  #the-let.binds.length() is 6 # ListB, emptyB, linkB, List, is-empty, is-link
  #the-let.binds.take(3).map(_.value) satisfies lists.all(lam(e): A.is-s-app(e) and (e._fun.id == "brander");, _)
  #the-let.binds.drop(3).map(_.value) satisfies lists.all(lam(e): A.is-s-dot(e) and (e.field == "test");, _)
  #the-letrec = the-let.body
  #the-letrec satisfies A.is-s-letrec
  #the-letrec.binds.length() is 4 # emptyDict, linkDict, empty, link

end

desugar-scope-visitor = A.default-map-visitor.{
  s-block(self, l, stmts):
    A.s-block(l, desugar-scope-block(stmts.map(_.visit(self)), [list: ], [list: ], [list: ]))
  end
}

fun wrap-env-imports(l, expr :: A.Expr, env :: C.CompileEnvironment):
  cases(C.CompileEnvironment) env:
    | compile-env(compile-bindings, type-env) =>
      let-binds = for fold(lst from [list: ], b from compile-bindings):
          cases(C.CompileBinding) b:
            | module-bindings(mname, bindings) =>
              lst + 
                for map(name from bindings):
                  A.s-let(l, A.s-bind(l, false, A.s-name(l, name), A.a-blank), A.s-dot(l, A.s-id(l, A.s-name(l, mname)), name), false)
                end
            | else => lst
          end
        end
      type-binds = for fold(lst from [list: ], t from type-env):
        cases(C.CompileTypeBinding) t:
          | type-id(id) => lst
          | type-module-bindings(name, bindings) =>
            lst +
              for map(tname from bindings):
                A.s-type-bind(l, A.s-name(l, tname), A.a-dot(l, A.s-name(l, name), tname))
              end
        end
      end
      A.s-type-let-expr(l, type-binds, A.s-block(l, let-binds + [list: expr]))
  end
end

fun desugar-scope(prog :: A.Program, compile-env:: C.CompileEnvironment):
  doc: ```
       Remove x = e, var x = e, and fun f(): e end
       and turn them into explicit let and letrec expressions.
       Do this recursively through the whole program.
       Preconditions on prog:
         - well-formed
       Postconditions on prog:
         - contains no s-provide in headers
         - contains no s-let, s-var, s-data
       ```
  cases(A.Program) prog:
    | s-program(l, _provide-raw, provide-types-raw, imports-raw, body) =>
      imports-and-lets = resolve-imports(imports-raw)
      imports = imports-and-lets.imports
      extra-lets = imports-and-lets.lets
      str = A.s-str(l, _)
      prov = cases(A.Provide) resolve-provide(_provide-raw, body):
        | s-provide-none(_) => A.s-obj(l, [list: ])
        | s-provide(_, block) => block
        | else => raise("Should have been resolved away")
      end
      provides = resolve-type-provide(provide-types-raw, body)
      provt = cases(A.Provide) provides:
        | s-provide-types-none(_) => [list: ]
        | s-provide-types(_, anns) => anns
        | else => raise("Should have been resolve-typed away" + torepr(provides))
      end
      # TODO: Need to resolve provide-types here
      with-imports = cases(A.Expr) body:
        | s-block(l2, stmts) =>
          A.s-block(l2, extra-lets + desugar-toplevel-types(stmts))
        | else => A.s-block(l, extra-lets + desugar-toplevel-types([list: body]))
      end
      fun transform-toplevel-last(l2, last):
        A.s-module(l2, last, prov, provt, A.s-app(l2, A.s-dot(l2, U.checkers(l2), "results"), empty))
      end
      with-provides = cases(A.Expr) with-imports:
        | s-block(l2, stmts) =>
          last = stmts.last()
          cases(A.Expr) last:
            | s-type-let-expr(l3, binds, body2) =>
              inner-last = body2.stmts.last()
              A.s-block(l2,
                stmts.take(stmts.length() - 1) + [list:
                  A.s-type-let-expr(l3, binds,
                    A.s-block(body2.l, body2.stmts.take(body2.stmts.length() - 1)
                        + [list: transform-toplevel-last(l3, inner-last)]))])
            | else =>
              A.s-block(l2, stmts.take(stmts.length() - 1) + [list: transform-toplevel-last(l2, last)])
          end
        | else => raise("Impossible")
      end
      wrapped = wrap-env-imports(l, with-provides, compile-env)
      full-imports = imports + for map(k from compile-env.bindings.filter(C.is-module-bindings).map(_.name)):
          A.s-import(l, A.s-const-import(l, k), A.s-name(l, k))
        end

      A.s-program(l, A.s-provide-none(l), A.s-provide-types-none(l),
        full-imports, wrapped.visit(desugar-scope-visitor))
  end
  
where:
  d = A.dummy-loc
  b = lam(s): A.s-bind(d, false, A.s-name(d, s), A.a-blank);
  id = lam(s): A.s-id(d, A.s-name(d, s));
  checks = A.s-app(d, A.s-dot(d, U.checkers(d), "results"), [list: ])
  str = A.s-str(d, _)
  ds = lam(prog): desugar-scope(prog, C.minimal-builtins).visit(A.dummy-loc-visitor) end
  compare1 = A.s-program(d, A.s-provide-none(d), A.s-provide-types-none(d), [list: ],
      A.s-type-let-expr(d, [list:],
        A.s-block(d, [list:
          A.s-block(d, [list:
            A.s-let-expr(d, [list:
                A.s-let-bind(d, b("x"), A.s-num(d, 10))
              ],
              A.s-module(d, id("nothing"), id("x"), [list:], checks))
          ])
        ])
      ))
  # NOTE(joe): Explicit nothing here because we expect to have
  # had append-nothing-if-necessary called
  ds(PP.surface-parse("provide x end x = 10 nothing", "test")) is compare1

  compare2 = A.s-program(d, A.s-provide-none(d), A.s-provide-types-none(d), [list:
        A.s-import(d, A.s-file-import(d, "./foo.arr"), A.s-name(d, "F"))
      ],
      A.s-type-let-expr(d, [list:],
        A.s-block(d, [list:
          A.s-block(d, [list:
            A.s-let-expr(d, [list: 
                A.s-let-bind(d, b("x"), A.s-num(d, 10))
              ],
              A.s-module(d, A.s-app(d, id("F"), [list: id("x")]), id("x"), [list:], checks))
          ])
        ])
      ))
  ds(PP.surface-parse("provide x end import 'foo.arr' as F x = 10 F(x)", "test")) is compare2
end


names = A.MakeName(0)

data ScopeBinding:
  | letrec-bind(loc, atom :: A.Name, expr :: Option<A.Expr>)
  | let-bind(loc, atom :: A.Name, expr :: Option<A.Expr>)
  | var-bind(loc, atom :: A.Name, expr :: Option<A.Expr>)
  | global-bind(loc, atom :: A.Name, expr :: Option<A.Expr>)
end

data TypeBinding:
  | global-type-bind(loc, atom :: A.Name, ann :: Option<A.Ann>)
  | let-type-bind(loc, atom :: A.Name, ann :: Option<A.Ann>)
  | type-var-bind(loc, atom :: A.Name, ann :: Option<A.Ann>)
end

fun scope-env-from-env(initial :: C.CompileEnvironment):
  for fold(acc from SD.immutable-string-dict(), b from initial.bindings):
    cases(C.CompileBinding) b:
      | module-bindings(name, ids) => acc
      | builtin-id(name) =>
        acc.set(name, let-bind(S.builtin("pyret-builtin"), names.s-global(name), none))
    end
  end
where:
  scope-env-from-env(C.compile-env([list:
      C.builtin-id("x")
    ], [list: ])).get("x") is let-bind(S.builtin("pyret-builtin"), names.s-global("x"), none)
end

fun type-env-from-env(initial :: C.CompileEnvironment):
  for fold(acc from SD.immutable-string-dict(), b from initial.types):
    cases(C.CompileBinding) b:
      | type-module-bindings(name, ids) => acc
      | type-id(name) =>
        acc.set(name, global-type-bind(S.builtin("pyret-builtin-type"), names.s-type-global(name), none))
    end
  end
end

fun resolve-names(p :: A.Program, initial-env :: C.CompileEnvironment):
  doc: ```
       Turn all s-names into s-atom or s-global
       Requires:
        1. desugar-scope
       Preconditions on p:
        -  Contains no s-block, s-let, s-var, s-data
       Postconditions on p (in addition to preconditions):
        -  Contains no s-name in names
       ```
  var name-errors = [list: ]
  bindings = SD.string-dict()
  type-bindings = SD.string-dict()

  fun make-atom-for(name, is-shadowing, env, shadow bindings, typ):
    cases(A.Name) name:
      | s-name(l, s) =>
        when env.has-key(s) and not(is-shadowing):
          old-loc = env.get(s).loc
          name-errors := link(C.shadow-id(s, l, old-loc), name-errors)
        end
        atom = names.make-atom(s)
        binding = typ(l, atom, none)
        bindings.set(atom.key(), binding)
        { atom: atom, env: env.set(s, binding) }
      | s-underscore(l) =>
        atom = names.make-atom("$underscore")
        bindings.set(atom.key(), typ(l, atom, none))
        { atom: atom, env: env }
      | else => raise("Unexpected atom type: " + torepr(name))
    end
  end
  fun update-type-binding-ann(atom, ann):
    if type-bindings.has-key(atom.key()):
      cases(TypeBinding) type-bindings.get(atom.key()):
        | let-type-bind(l, _, _) =>
          type-bindings.set(atom.key(), let-type-bind(l, atom, ann))
        | global-type-bind(l, _, _) =>
          type-bindings.set(atom.key(), global-type-bind(l, atom, ann))
        | type-var-bind(l, _, _) =>
          type-bindings.set(atom.key(), type-var-bind(l, atom, ann))
      end
    else:
      print("No binding for " + torepr(atom))
    end
  end
  fun update-binding-expr(atom, expr):
    cases(ScopeBinding) bindings.get(atom.key()):
      | letrec-bind(loc, _, _) => bindings.set(atom.key(), letrec-bind(loc, atom, expr))
      | let-bind(loc, _, _) => bindings.set(atom.key(), let-bind(loc, atom, expr))
      | var-bind(loc, _, _) => bindings.set(atom.key(), var-bind(loc, atom, expr))
      | global-bind(loc, _, _) => bindings.set(atom.key(), global-bind(loc, atom, expr))
    end
  end
  fun handle-id(env, l, id):
    cases(A.Name) id:
      | s-name(l2, s) =>
        if env.has-key(s):
          cases (ScopeBinding) env.get(s):
            | let-bind(_, atom, _) => A.s-id(l, atom)
            | letrec-bind(_, atom, _) => A.s-id-letrec(l, atom, false)
            | var-bind(_, atom, _) => A.s-id-var(l, atom)
            | global-bind(_, atom, _) => A.s-id(l, atom)
          end
        else:
          A.s-id(l, names.s-global(s))
        end
      | s-underscore(_) => A.s-id(l, id)
      | else => raise("Wasn't expecting a non-s-name in resolve-names id: " + torepr(id))
    end
  end
  fun handle-ann(l, type-env, id):
    cases(A.Name) id:
      | s-name(_, s) =>
        if type-env.has-key(s):
          cases(TypeBinding) type-env.get(s):
            | global-type-bind(_, name, _) => A.a-name(l, name)
            | let-type-bind(_, name, _) => A.a-name(l, name)
            | type-var-bind(_, name, _) => A.a-blank # TODO: Turn this into a A.a-type-var(l, name) instead
          end
        else: A.a-name(l, names.s-type-global(s))
        end
      | else => A.a-name(l, id)
    end
  end
  names-visitor = A.default-map-visitor.{
    env: scope-env-from-env(initial-env),
    type-env: type-env-from-env(initial-env),
    s-program(self, l, _provide, _provide-types, imports, body):
      imports-and-env = for fold(acc from { e: self.env, te: self.type-env, imps: [list: ] }, i from imports):
        cases(A.Import) i:
          | s-import(l2, file, name) =>
            atom-env = make-atom-for(name, false, acc.e, bindings, let-bind)
            atom-env-t = make-atom-for(name, false, acc.te, type-bindings, let-type-bind)
            new-header = A.s-import-types(l2, file, atom-env.atom, atom-env-t.atom)
            update-binding-expr(atom-env.atom, some(new-header))
            update-type-binding-ann(atom-env-t.atom, some(new-header))
            { e: atom-env.env, te: atom-env-t.env, imps: link(new-header, acc.imps) }
          | else => acc
        end
      end
      visit-body = body.visit(self.{env: imports-and-env.e, type-env: imports-and-env.te})
      A.s-program(l, _provide, _provide-types, imports-and-env.imps.reverse(), visit-body)
    end,
    s-type-let-expr(self, l, binds, body):
      bound-env = for fold(acc from { e: self.env, te: self.type-env, bs: [list: ] }, b from binds):
        cases(A.TypeLetBind) b:
          | s-type-bind(l2, name, ann) =>
            atom-env = make-atom-for(name, false, acc.te, type-bindings, let-type-bind)
            new-bind = A.s-type-bind(l2, atom-env.atom, ann.visit(self.{env: acc.e, type-env: acc.te}))
            update-type-binding-ann(atom-env.atom, some(new-bind.ann))
            { e: acc.e, te: atom-env.env, bs: link(new-bind, acc.bs) }
          | s-newtype-bind(l2, name, tname) =>
            atom-env-t = make-atom-for(name, false, acc.te, type-bindings, let-type-bind)
            atom-env = make-atom-for(tname, false, acc.e, bindings, let-bind)
            new-bind = A.s-newtype-bind(l2, atom-env-t.atom, atom-env.atom)
            update-binding-expr(atom-env.atom, none)
            update-type-binding-ann(atom-env-t.atom, none)
            { e: atom-env.env, te: atom-env-t.env, bs: link(new-bind, acc.bs) }
        end
      end
      visit-body = body.visit(self.{env: bound-env.e, type-env: bound-env.te})
      A.s-type-let-expr(l, bound-env.bs, visit-body)
    end,
    s-let-expr(self, l, binds, body):
      bound-env = for fold(acc from { e: self.env, bs : [list: ] }, b from binds):
        cases(A.LetBind) b:
          | s-let-bind(l2, bind, expr) =>
            atom-env = make-atom-for(bind.id, bind.shadows, acc.e, bindings, let-bind)
            visit-expr = expr.visit(self.{env: acc.e})
            update-binding-expr(atom-env.atom, some(visit-expr))
            new-bind = A.s-let-bind(l2, A.s-bind(l2, bind.shadows, atom-env.atom, bind.ann.visit(self.{env: acc.e})), visit-expr)
            {
              e: atom-env.env,
              bs: link(new-bind, acc.bs)
            }
          | s-var-bind(l2, bind, expr) =>
            atom-env = make-atom-for(bind.id, bind.shadows, acc.e, bindings, var-bind)
            visit-expr = expr.visit(self.{env: acc.e})
            update-binding-expr(atom-env.atom, some(visit-expr))
            new-bind = A.s-var-bind(l2, A.s-bind(l2, bind.shadows, atom-env.atom, bind.ann.visit(self.{env: acc.e})), visit-expr)
            {
              e: atom-env.env,
              bs: link(new-bind, acc.bs)
            }
        end
      end
      visit-binds = bound-env.bs.reverse()
      visit-body = body.visit(self.{env: bound-env.e})
      A.s-let-expr(l, visit-binds, visit-body)
    end,
    s-letrec(self, l, binds, body):
      bind-env-and-atoms = for fold(acc from { env: self.env, atoms: [list: ] }, b from binds):
        atom-env = make-atom-for(b.b.id, b.b.shadows, acc.env,  bindings, letrec-bind)
        { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
      end
      new-visitor = self.{env: bind-env-and-atoms.env}
      visit-binds = for map2(b from binds, a from bind-env-and-atoms.atoms.reverse()):
        cases(A.LetrecBind) b:
          | s-letrec-bind(l2, bind, expr) =>
            new-bind = A.s-bind(l2, false, a, bind.ann.visit(self.{env: bind-env-and-atoms.env}))
            visit-expr = expr.visit(new-visitor)
            update-binding-expr(a, some(visit-expr))
            A.s-letrec-bind(l2, new-bind, visit-expr)
        end
      end
      visit-body = body.visit(new-visitor)
      A.s-letrec(l, visit-binds, visit-body)
    end,
    s-for(self, l, iter, binds, ann, body):
      env-and-binds = for fold(acc from { env: self.env, fbs: [list: ] }, fb from binds):
        cases(ForBind) fb:
          | s-for-bind(l2, bind, val) => 
            atom-env = make-atom-for(bind.id, bind.shadows, acc.env, bindings, let-bind)
            new-bind = A.s-bind(bind.l, bind.shadows, atom-env.atom, bind.ann.visit(self.{env: acc.env}))
            visit-val = val.visit(self.{env: acc.env})
            update-binding-expr(atom-env.atom, some(visit-val))
            new-fb = A.s-for-bind(l2, new-bind, visit-val)
            { env: atom-env.env, fbs: link(new-fb, acc.fbs) }
        end
      end
      A.s-for(l, iter.visit(self), env-and-binds.fbs.reverse(), ann.visit(self), body.visit(self.{env: env-and-binds.env}))
    end,
    s-cases-branch(self, l, name, args, body):
      env-and-atoms = for fold(acc from { env: self.env, atoms: [list: ] }, a from args):
        atom-env = make-atom-for(a.id, a.shadows, acc.env, bindings, let-bind)
        { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
      end
      new-args = for map2(a from args, at from env-and-atoms.atoms.reverse()):
        cases(A.Bind) a:
          | s-bind(l2, shadows, id, ann) => A.s-bind(l2, false, at, ann.visit(self.{env: env-and-atoms.env}))
        end
      end
      new-body = body.visit(self.{env: env-and-atoms.env})
      A.s-cases-branch(l, name, new-args, new-body)
    end,
    s-data-expr(self, l, name, namet, params, mixins, variants, shared-members, _check):
      new-types = for fold(acc from { env: self.type-env, atoms: empty }, param from params):
        atom-env = make-atom-for(param, false, acc.env, type-bindings, type-var-bind)
        { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
      end
      with-params = self.{type-env: new-types.env}
      new-namet = handle-id(self.env, l, namet).id
      A.s-data-expr(l, name, new-namet, new-types.atoms.reverse(),
        mixins.map(_.visit(with-params)), variants.map(_.visit(with-params)),
        shared-members.map(_.visit(with-params)), with-params.option(_check))
    end,
    s-lam(self, l, params, args, ann, doc, body, _check):
      new-types = for fold(acc from {env: self.type-env, atoms: empty }, param from params):
        atom-env = make-atom-for(param, false, acc.env, type-bindings, type-var-bind)
        { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
      end
      with-params = self.{type-env: new-types.env}
      env-and-atoms = for fold(acc from { env: with-params.env, atoms: [list: ] }, a from args):
        atom-env = make-atom-for(a.id, a.shadows, acc.env, bindings, let-bind)
        { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
      end
      new-args = for map2(a from args, at from env-and-atoms.atoms.reverse()):
        cases(A.Bind) a:
          | s-bind(l2, shadows, id, ann2) => A.s-bind(l2, false, at, ann2.visit(with-params))
        end
      end
      with-params-and-args = with-params.{env: env-and-atoms.env}
      new-body = body.visit(with-params-and-args)
      new-check = with-params.option(_check) # Maybe should be self?  Are any type params visible here?
      A.s-lam(l, new-types.atoms.reverse(), new-args, ann.visit(with-params), doc, new-body, new-check)
    end,
    s-method(self, l, args, ann, doc, body, _check):
      env-and-atoms = for fold(acc from { env: self.env, atoms: [list: ] }, a from args):
        atom-env = make-atom-for(a.id, a.shadows, acc.env, bindings, let-bind)
        { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
      end
      new-args = for map2(a from args, at from env-and-atoms.atoms.reverse()):
        cases(A.Bind) a:
          | s-bind(l2, shadows, id, ann2) => A.s-bind(l2, shadows, at, ann2.visit(self.{env: env-and-atoms.env}))
        end
      end
      new-body = body.visit(self.{env: env-and-atoms.env})
      new-check = self.option(_check)
      A.s-method(l, new-args, ann.visit(self.{env: env-and-atoms.env}), doc, new-body, new-check)
    end,
    s-method-field(self, l, name, args, ann, doc, body, _check):
      env-and-atoms = for fold(acc from { env: self.env, atoms: [list: ] }, a from args):
        atom-env = make-atom-for(a.id, a.shadows, acc.env, bindings, let-bind)
        { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
      end
      new-args = for map2(a from args, at from env-and-atoms.atoms.reverse()):
        cases(A.Bind) a:
          | s-bind(l2, shadows, id, ann2) => A.s-bind(l2, shadows, at, ann2.visit(self.{env: env-and-atoms.env}))
        end
      end
      new-body = body.visit(self.{env: env-and-atoms.env})
      new-check = self.option(_check)
      A.s-method-field(l, name, new-args, ann.visit(self.{env: env-and-atoms.env}), doc, new-body, new-check)
    end,
    s-assign(self, l, id, expr):
      cases(A.Name) id:
        | s-name(l2, s) =>
          if self.env.has-key(s):
            bind = self.env.get(s)
            A.s-assign(l, bind.atom, expr.visit(self))
            # This used to examine bind in more detail, and raise an error if it wasn't a var-bind
            # but that's better suited for a later pass
          else:
            A.s-assign(l, id, expr.visit(self)) # TODO: Should this be a s-global after all?
          end
        | s-underscore(_) =>
          A.s-assign(l, id, expr.visit(self))
        | else => raise("Wasn't expecting a non-s-name in resolve-names for assignment: " + torepr(id))
      end
    end,
    s-id(self, l, id): handle-id(self.env, l, id) end,
    s-id-letrec(self, l, id, _): handle-id(self.env, l, id) end,
    s-id-var(self, l, id): handle-id(self.env, l, id) end,
    s-variant-member(self, l, typ, bind):
      new-bind = cases(A.Bind) bind:
        | s-bind(l2, shadows, name, ann) =>
          atom-env = make-atom-for(name, true, self.env, bindings, let-bind)
          A.s-bind(l2, shadows, atom-env.atom, ann.visit(self))
      end
      A.s-variant-member(l, typ, new-bind)
    end,
    s-bind(self, l, shadows, id, ann):
      cases(A.Name) id:
        | s-underscore(_) => A.s-bind(l, shadows, id, ann)
        | else => 
          raise("Should not reach non-underscore bindings in resolve-names" + torepr(l) + torepr(id))
      end
    end,
    a-blank(self): A.a-blank end,
    a-any(self): A.a-any end,
    a-name(self, l, id): handle-ann(l, self.type-env, id) end,
    a-arrow(self, l, args, ret, parens): A.a-arrow(l, args.map(_.visit(self)), ret.visit(self), parens) end,
    a-method(self, l, args, ret): A.a-method(l, args.map(_.visit(self)), ret.visit(self)) end,
    a-record(self, l, fields): A.a-record(l, fields.map(_.visit(self))) end,
    a-app(self, l, ann, args): A.a-app(l, ann.visit(self), args.map(_.visit(self))) end,
    a-pred(self, l, ann, exp): A.a-pred(l, ann.visit(self), exp.visit(self)) end,
    a-dot(self, l, obj, field):
      obj-ann = handle-ann(l, self.type-env, obj)
      cases(A.Ann) obj-ann:
        | a-name(_, name) => A.a-dot(l, name, field)
        | else =>
          name-errors := link(C.unexpected-type-var(l, obj), name-errors)
          A.a-blank
      end
    end,
    a-field(self, l, name, ann): A.a-field(l, name, ann.visit(self)) end
  }
  resolved(p.visit(names-visitor), name-errors, bindings, type-bindings)
end

