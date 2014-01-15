#lang pyret

provide *
import ast as A

data DesugarEnv:
  | d-env(ids :: Set<String>, vars :: Set<String>)
end

mt-d-env = d-env(set([]), set([]))

fun extend-id(nv :: DesugarEnv, id :: String):
  d-env(nv.ids.add(id), nv.vars)
end

fun extend-var(nv :: DesugarEnv, id :: String):
  d-env(nv.ids, nv.vars.add(id))
end

fun desugar(program :: A.Program):
  cases(A.Program) program:
    | s_program(l, headers, body) =>
      A.s_program(l, headers, desugar-expr(mt-d-env, body))
  end
end

fun resolve-scope(stmts, let-binds, letrec-binds) -> List<Expr>:
  doc: "Treating stmts as a block, remove x = e, var x = e, and fun f(): e end
        and turn them into explicit let and letrec expressions."
  cases(List) stmts:
    | empty => raise("Empty block in resolve-scope")
    | link(f, rest-stmts) =>
      fun wrap-letrecs(expr):
        A.s_letrec(letrec-binds.first.l, letrec-binds.reverse(), expr)
      end
      fun wrap-lets(expr):
        A.s_let_expr(let-binds.first.l, let-binds.reverse(), expr)
      end
      fun handle-let-bind(l, new-bind):
        new-binds = link(new-bind, let-binds)
        resolved-inner = resolve-scope(rest-stmts, new-binds, [])
        if is-empty(letrec-binds):
          resolved-inner
        else:
          [wrap-letrecs(A.s_block(l, resolved-inner))]
        end
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
          resolved-inner = resolve-scope(rest-stmts, [], new-letrecs)
          if is-empty(let-binds):
            resolved-inner
          else:
            [wrap-lets(A.s_block(l, resolved-inner))]
          end
        | else =>
          wrapper = 
            if is-link(let-binds): wrap-lets
            else if is-link(letrec-binds): wrap-letrecs
            else: fun(e): e;
            end
          cases(List) rest-stmts:
            | empty => [wrapper(f)]
            | link(_, _) =>
              if not (is-link(let-binds) or is-link(letrec-binds)):
                link(f, resolve-scope(rest-stmts, [], []))
              else:
                [wrapper(A.s_block(f.l,
                  link(f, resolve-scope(rest-stmts, [], []))))]
              end
          end
      end
  end
where:
  p = fun(str): A.surface-parse(str, "test").block;
  d = A.dummy-loc
  b = A.s_bind(d, false, _, A.a_blank)
  bk = fun(e): A.s_block(d, [e]) end
  bs = fun(str):
    A.s_block(d, resolve-scope(p(str).stmts, [], []))
  end
  n = A.s_block(d, [])
  thunk = fun(e): A.s_lam(d, [], [], A.a_blank, "", bk(e), n) end

  resolve-scope(p("x = 5 y = 10 y").stmts, [], []).first
    satisfies 
      A.equiv-ast(_, A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 5)),
                                      A.s_let_bind(d, b("y"), A.s_num(d, 10))],
                        A.s_id(d, "y")))

  resolve-scope(p("x = 5 var y = 10 y").stmts, [], []).first
    satisfies 
      A.equiv-ast(_, A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 5)),
                                      A.s_var_bind(d, b("y"), A.s_num(d, 10))],
                        A.s_id(d, "y")))

  bs("x = 5 print(2) var y = 10 y")
    satisfies 
      A.equiv-ast(_,
                  A.s_block(d,
                    [ A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 5))],
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

  resolve-scope([prog2], [], []).first satisfies A.equiv-ast(_, prog2)
  for each2(p1 from resolve-scope(prog2.stmts, [], []), p2 from prog2.stmts):
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


end

fun get-arith-op(str):
  if str == "op+": some("_plus")
  else if str == "op-": some("_minus")
  else if str == "op*": some("_times")
  else if str == "op<": some("_lessthan")
  else: none
  end
end

fun desugar-if-branch(nv :: DesugarEnv, expr :: A.IfBranch):
  cases(A.IfBranch) expr:
    | s_if_branch(l, test, body) =>
      A.s_if_branch(l, desugar-expr(nv, test), desugar-expr(nv, body))
  end
end

fun desugar-expr(nv :: DesugarEnv, expr :: A.Expr):
  cases(A.Expr) expr:
    | s_block(l, stmts) =>
      cases(List) stmts:
        | empty => expr
        | link(_, _) =>
          new-stmts = resolve-scope(stmts, [], [])
          A.s_block(l, resolve-scope(stmts, [], []).map(desugar-expr(nv, _)))
      end
    | s_app(l, f, args) =>
      A.s_app(l, desugar-expr(nv, f), args.map(desugar-expr(nv, _)))
    | s_lam(l, params, args, ann, doc, body, _check) =>
      new-env = for fold(nv2 from nv, arg from args): extend-id(nv2, arg.id) end
      A.s_lam(l, params, args, ann, doc, desugar-expr(new-env, body), desugar-expr(nv, _check))
    | s_method(l, args, ann, doc, body, _check) =>
      new-env = for fold(nv2 from nv, arg from args): extend-id(nv2, arg.id) end
      A.s_method(l, args, ann, doc, desugar-expr(new-env, body), desugar-expr(nv, _check))
    | s_let_expr(l, binds, body) =>
      new-binds = for fold(b-e from { b: [], e: nv }, bind from binds):
        cases(A.LetBind) bind:
          | s_let_bind(l2, b, val) =>
            new-env = b-e.e^extend-id(b.id)
            new-val = desugar-expr(b-e.e, val)
            { b: link(A.s_let_bind(l2, b, new-val), b-e.b),
              e: new-env }
          | s_var_bind(l2, b, val) =>
            new-env = b-e.e^extend-var(b.id)
            new-val = desugar-expr(b-e.e, val)
            { b: link(A.s_var_bind(l2, b, new-val), b-e.b),
              e: new-env }
        end
      end
      A.s_let_expr(l, new-binds.b.reverse(), desugar-expr(new-binds.e, body))
    | s_if(l, branches) =>
      raise("If must have else for now")
    | s_if_else(l, branches, _else) =>
      A.s_if_else(l, branches.map(desugar-if-branch(nv, _)), desugar-expr(nv, _else))
    | s_assign(l, id, val) => A.s_assign(l, id, desugar-expr(nv, val))
    | s_dot(l, obj, field) => A.s_dot(l, desugar-expr(nv, obj), field)
    | s_colon(l, obj, field) => A.s_colon(l, desugar-expr(nv, obj), field)
    | s_op(l, op, left, right) =>
      cases(Option) get-arith-op(op):
        | some(field) => A.s_app(l, A.s_dot(l, desugar-expr(nv, left), field), [desugar-expr(nv, right)])
        | none => raise("Only arith ops so far, " + op + " did not match")
      end
    | s_id(l, x) =>
      print("checking membership of " + x)
      print(nv.vars)
      if nv.vars.member(x): A.s_id_var(l, x)
      else: expr
      end
    | s_num(_, _) => expr
    | s_str(_, _) => expr
    | s_bool(_, _) => expr
    | s_obj(l, fields) => A.s_obj(l, fields.map(fun(f): 
            cases(A.Member) f:
               | s_method_field(_ , _, _, _, _, body, _) => f.{body : desugar-expr(nv, body)} 
               | else => f.{value : desugar-expr(nv, f.value)}
            end
        end))
    | else => raise("NYI: " + torepr(expr))
  end
where:
  p = fun(str): A.surface-parse(str, "test").block;
  prog = p("var x = 10 x := 5 test-print(x)")
  d = A.dummy-loc
  b = A.s_bind(d, false, _, A.a_blank)

  desugar-expr(mt-d-env, prog) satisfies
    A.equiv-ast(_, A.s_block(d, [
        A.s_let_expr(d, [
            A.s_var_bind(d, b("x"), A.s_num(d, 10))
          ],
          A.s_block(d, [
              A.s_assign(d, "x", A.s_num(d, 5)),
              A.s_app(d, A.s_id(d, "test-print"), [A.s_id_var(d, "x")])
            ]))
        ]))
end

