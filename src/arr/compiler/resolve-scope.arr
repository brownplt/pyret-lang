#lang pyret

provide *
import ast as A
import parse-pyret as PP
import "./compile-structs.arr" as C
import "./ast-util.arr" as U
import "./gensym.arr" as G

fun mk-bind(l, id): A.s_bind(l, false, id, A.a_blank);

fun mk-id(loc, base):
  t = G.make-name(base)
  { id: t, id-b: mk-bind(loc, t), id-e: A.s_id(loc, t) }
end

fun resolve-header(h :: A.Header, b :: A.Expr):
  cases(A.Header) h:
    | s_provide_all(l) =>
      ids = A.block-ids(b)
      obj = A.s_obj(l, for map(id from ids): A.s_data_field(l, A.s_str(l, id), A.s_id(l, id)) end)
      A.s_provide(l, obj)
    | s_import(l, imp, name) =>
      cases(A.ImportType) imp:
        | s_file_import(file) =>
          if file.contains("/"): h
          else: A.s_import(l, A.s_file_import("./" + file), name)
          end
        | else => h
      end
    | else => h
  end
end


fun resolve-scope-block(stmts, let-binds, letrec-binds) -> List<Expr>:
  doc: "Treating stmts as a block, resolve scope."
  cases(List) stmts:
    | empty => empty
    | link(f, rest-stmts) =>
      fun wrap-letrecs(expr):
        A.s_letrec(letrec-binds.first.l, letrec-binds.reverse(), expr)
      end
      fun wrap-lets(expr):
        A.s_let_expr(let-binds.first.l, let-binds.reverse(), expr)
      end
      fun handle-let-bind(l, new-bind):
        new-binds = link(new-bind, let-binds)
        resolved-inner = resolve-scope-block(rest-stmts, new-binds, [])
        if is-empty(letrec-binds):
          resolved-inner
        else:
          [wrap-letrecs(A.s_block(l, resolved-inner))]
        end
      end
      wrapper = 
        if is-link(let-binds): wrap-lets
        else if is-link(letrec-binds): wrap-letrecs
        else: fun(e): e;
        end
      cases(A.Expr) f:
        | s_let(l, bind, expr) =>
          handle-let-bind(l, A.s_let_bind(l, bind, expr))
        | s_var(l, bind, expr) =>
          handle-let-bind(l, A.s_var_bind(l, bind, expr))
        | s_fun(l, name, params, args, ann, doc, body, _check) =>
          new-letrecs = link(A.s_letrec_bind(
              l,
              A.s_bind(l, false, name, A.a_blank),
              A.s_lam(l, params, args, ann, doc, body, _check)
            ), letrec-binds)
          resolved-inner = resolve-scope-block(rest-stmts, [], new-letrecs)
          if is-empty(let-binds):
            resolved-inner
          else:
            [wrap-lets(A.s_block(l, resolved-inner))]
          end
        | s_data(l, name, params, mixins, variants, shared, _check) =>
          fun b(loc, id): A.s_bind(loc, false, id, A.a_blank);
          fun variant-binds(data-blob-id, v):
            vname = v.name
            checker-name = A.make-checker-name(vname)
            get-part = A.s_dot(v.l, data-blob-id, _)
            [
              A.s_letrec_bind(v.l, b(v.l, vname), get-part(vname)),
              A.s_letrec_bind(v.l, b(v.l, checker-name), get-part(checker-name))
            ]
          end
          blob-id = G.make-name(name)
          data-expr = A.s_data_expr(l, name, params, mixins, variants, shared, _check)
          bind-data = A.s_letrec_bind(l, b(l, blob-id), data-expr)
          bind-data-pred = A.s_letrec_bind(l, b(l, name), A.s_dot(l, A.s_id(l, blob-id), name))
          all-binds = for fold(acc from [bind-data-pred, bind-data], v from variants):
            variant-binds(A.s_id(l, blob-id), v) + acc
          end

          if is-empty(letrec-binds):
            [wrapper(A.s_block(l, resolve-scope-block(rest-stmts, [], all-binds)))]
          else:
            resolve-scope-block(rest-stmts, [], all-binds + letrec-binds)
          end

        | else =>
          cases(List) rest-stmts:
            | empty => [wrapper(f)]
            | link(_, _) =>
              if not (is-link(let-binds) or is-link(letrec-binds)):
                link(f, resolve-scope-block(rest-stmts, [], []))
              else:
                [wrapper(A.s_block(f.l,
                  link(f, resolve-scope-block(rest-stmts, [], []))))]
              end
          end
      end
  end
where:
  p = fun(str): PP.surface-parse(str, "test").block;
  d = A.dummy-loc
  b = A.s_bind(d, false, _, A.a_blank)
  bk = fun(e): A.s_block(d, [e]) end
  bs = fun(str):
    A.s_block(d, resolve-scope-block(p(str).stmts, [], []))
  end
  n = none
  thunk = fun(e): A.s_lam(d, [], [], A.a_blank, "", bk(e), n) end


  resolve-scope-block(p("x = 15 y = 10 y").stmts, [], []).first
    satisfies 
      A.equiv-ast(_, A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 15)),
                                      A.s_let_bind(d, b("y"), A.s_num(d, 10))],
                        A.s_id(d, "y")))

  resolve-scope-block(p("x = 55 var y = 10 y").stmts, [], []).first
    satisfies 
      A.equiv-ast(_, A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 55)),
                                      A.s_var_bind(d, b("y"), A.s_num(d, 10))],
                        A.s_id(d, "y")))

  bs("x = 7 print(2) var y = 10 y")
    satisfies 
      A.equiv-ast(_,
                  A.s_block(d,
                    [ A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 7))],
                        A.s_block(d, [
                            A.s_app(d, A.s_id(d, "print"), [A.s_num(d, 2)]),
                            A.s_let_expr(d, [A.s_var_bind(d, b("y"), A.s_num(d, 10))],
                              A.s_id(d, "y"))
                          ]))]))

  prog = bs("fun f(): 4 end fun g(): 5 end f()")
  prog
    satisfies
      A.equiv-ast(_,
        A.s_block(d,
          [ A.s_letrec(d, [
              A.s_letrec_bind(d, b("f"), thunk(A.s_num(d, 4))),
              A.s_letrec_bind(d, b("g"), thunk(A.s_num(d, 5)))
            ],
            A.s_app(d, A.s_id(d, "f"), []))
          ]))

  p-s = fun(e): A.s_app(d, A.s_id(d, "print"), [e]);
  pretty = fun(e): e.tosource().pretty(80).join-str("\n");

  prog2 = bs("print(1) fun f(): 4 end fun g(): 5 end fun h(): 6 end x = 3 print(x)")
  prog2
    satisfies
      A.equiv-ast(_,
          A.s_block(d,
            [ p-s(A.s_num(d, 1)),
              A.s_letrec(d, [
                  A.s_letrec_bind(d, b("f"), thunk(A.s_num(d, 4))),
                  A.s_letrec_bind(d, b("g"), thunk(A.s_num(d, 5))),
                  A.s_letrec_bind(d, b("h"), thunk(A.s_num(d, 6)))
                ],
                A.s_block(d, [
                    A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 3))], p-s(A.s_id(d, "x")))
                  ]))]))

  resolve-scope-block([prog2], [], []).first satisfies A.equiv-ast(_, prog2)
  for each2(p1 from resolve-scope-block(prog2.stmts, [], []), p2 from prog2.stmts):
    p1 satisfies A.equiv-ast(_, p2)
  end

  prog3 = bs("print(x) x := 3 print(x)")
  prog3 satisfies
    A.equiv-ast(_,
      A.s_block(d,
          [
            p-s(A.s_id(d, "x")),
            A.s_assign(d, "x", A.s_num(d, 3)),
            p-s(A.s_id(d, "x"))
          ]
        )
      )

  prog4 = bs("var x = 10 fun f(): 4 end f()")
  prog4 satisfies
    A.equiv-ast(_,
      A.s_block(d, [
        A.s_let_expr(d, [
              A.s_var_bind(d, b("x"), A.s_num(d, 10))
            ],
            A.s_block(d, [
                A.s_letrec(d, [
                    A.s_letrec_bind(d, b("f"), thunk(A.s_num(d, 4)))
                  ],
                  A.s_app(d, A.s_id(d, "f"), []))
              ]))]))

  #prog5 = bs("data List: empty | link(f, r) end empty")
  #prog5.stmts.length() is 1
  #the-let = prog5.stmts.first
  #the-let satisfies A.is-s_let_expr
  #the-let.binds.length() is 6 # ListB, emptyB, linkB, List, is-empty, is-link
  #the-let.binds.take(3).map(_.value) satisfies list.all(fun(e): A.is-s_app(e) and (e._fun.id == "brander");, _)
  #the-let.binds.drop(3).map(_.value) satisfies list.all(fun(e): A.is-s_dot(e) and (e.field == "test");, _)
  #the-letrec = the-let.body
  #the-letrec satisfies A.is-s_letrec
  #the-letrec.binds.length() is 4 # emptyDict, linkDict, empty, link

end

resolve-scope-visitor = A.default-map-visitor.{
  s_block(self, l, stmts):
    A.s_block(l, resolve-scope-block(stmts.map(_.visit(self)), [], []))
  end
}

fun wrap-env-imports(l, expr :: A.Expr, env :: C.CompileEnvironment):
  cases(C.CompileEnvironment) env:
    | compile-env(compile-bindings) =>
      let-binds = for fold(lst from [], b from compile-bindings):
          cases(C.CompileBinding) b:
            | module-bindings(mname, bindings) =>
              lst + 
                for map(name from bindings):
                  A.s_let(l, A.s_bind(l, false, name, A.a_blank), A.s_dot(l, A.s_id(l, mname), name))
                end
            | else => lst
          end
        end
      A.s_block(l, let-binds + [expr])
  end
end


fun resolve-scope(prog :: A.Program, compile-env:: C.CompileEnvironment):
  doc: "Remove x = e, var x = e, and fun f(): e end
        and turn them into explicit let and letrec expressions.
        Do this recursively through the whole program."
  cases(A.Program) prog:
    | s_program(l, headers-raw, body) =>
      headers = headers-raw.map(resolve-header(_, body))
      str = A.s_str(l, _)
      provides = headers.filter(A.is-s_provide)
      len = provides.length()
      prov = if len == 0:
        A.s_obj(l, [])
      else if len == 1:
        provides.first.block
      else:
        raise("More than one provide")
      end
      with-provides = cases(A.Expr) body:
        | s_block(l2, stmts) =>
          last = stmts.last()
          new-stmts = stmts.take(stmts.length() - 1) + [A.s_obj(l2, [
              A.s_data_field(l2, str("answer"), last),
              A.s_data_field(l2, str("provide"), prov),
              A.s_data_field(
                  l2,
                  str("checks"),
                  A.s_app(l2, A.s_dot(l2, U.checkers(l2), "results"), [])
                )
            ])]
          A.s_block(l2, new-stmts)
        | else => body
      end
      wrapped = wrap-env-imports(l, with-provides, compile-env)
      imports = headers.filter(fun(h): not A.is-s_provide(h) end)
      full-imports = imports + for map(k from compile-env.bindings.filter(C.is-module-bindings).map(_.name)):
          A.s_import(l, A.s_const_import(k), k)
        end

      A.s_program(l, full-imports, wrapped.visit(resolve-scope-visitor))
  end
  
where:
  d = A.dummy-loc
  checks = A.s_data_field(
                  d,
                  A.s_str(d, "checks"),
                  A.s_app(d, A.s_dot(d, U.checkers(d), "results"), [])
                )
  str = A.s_str(d, _)
  ds = resolve-scope(_, C.minimal-builtins)
  compare1 = A.s_program(d, [],
      A.s_block(d, [
        A.s_block(d, [
          A.s_let_expr(d, [
              A.s_let_bind(d, mk-bind(d, "x"), A.s_num(d, 10))
            ],
            A.s_obj(d, [
                A.s_data_field(d, str("answer"), A.s_id(d, "nothing")),
                A.s_data_field(d, str("provide"), A.s_id(d, "x")),
                checks
              ]))
        ])
      ]))
  # NOTE(joe): Explicit nothing here because we expect to have
  # had append-nothing-if-necessary called
  ds(PP.surface-parse("provide x end x = 10 nothing", "test")) satisfies
    A.equiv-ast-prog(_, compare1)

  compare2 = A.s_program(d, [
        A.s_import(d, A.s_file_import("./foo.arr"), "F") 
      ],
      A.s_block(d, [
        A.s_block(d, [
          A.s_let_expr(d, [
              A.s_let_bind(d, mk-bind(d, "x"), A.s_num(d, 10))
            ],
            A.s_obj(d, [
                A.s_data_field(d, str("answer"), A.s_app(d, A.s_id(d, "F"), [A.s_id(d, "x")])),
                A.s_data_field(d, str("provide"), A.s_id(d, "x")),
                checks
              ]))
        ])
      ]))
  ds(PP.surface-parse("provide x end import 'foo.arr' as F x = 10 F(x)", "test")) satisfies
    A.equiv-ast-prog(_, compare2)
    
end


names = A.MakeName(0)

