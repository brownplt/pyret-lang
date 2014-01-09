#lang pyret

import ast as A

data DesugarEnv:
  | d-env(ids :: Set<String>, vars :: Set<String>)
end

mt-d-env = d-env(set([]), set([]))

fun desugar(program :: A.Program):
  cases(A.Program) program:
    | s_program(l, headers, body) =>
      A.s_program(l, headers, desugar-expr(mt-d-env, body))
  end
end

fun resolve-scope(stmts, let-binds, letrec-binds) -> List<Expr>:
  cases(List) stmts:
    | empty => raise("Empty block in resolve-scope")
    | link(f, rest-stmts) =>
      fun wrap-letrecs(expr):
        A.s_let_expr(letrec-binds.first.l, letrec-binds.reverse(), expr)
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
          handle-let-bind(A.s_var_bind(l, bind, expr))
        | s_fun(l, name, params, args, ann, doc, body, _check) =>
          new-letrecs = link(A.s_letrec_bind(
              l,
              A.s_bind(name, A.a_blank),
              A.s_lam(l, params, args, ann, doc, body, _check)
            ), letrec-binds)
          resolved-inner = resolve-scope(rest-stmts, [], new-letrecs)
          if is-empty(let-binds):
            resolved-inner
          else:
            [wrap-lets(A.s_block(l, resolved-inner))]
          end
          raise("nyi fun")
        | else =>
          cases(List) rest-stmts:
            | empty =>
              if is-link(let-binds): [wrap-lets(f)]
              else if is-link(letrec-binds): [wrap-letrecs(f)]
              else: [f]
              end
            | link(_, _) =>
              link(f, resolve-scope(rest-stmts))
          end
      end
  end
where:
  p = fun(str): A.surface-parse(str, "test").block;
  d = A.dummy-loc
  b = A.s_bind(d, false, _, A.a_blank)

  resolve-scope(p("x = 5 y = 10 y").stmts, [], []).first
    satisfies 
      A.equiv-ast(_, A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 5)),
                                      A.s_let_bind(d, b("y"), A.s_num(d, 10))],
                        A.s_id(d, "y")))
end

fun desugar-expr(nv :: DesugarEnv, expr :: A.Program):
  cases(A.Expr) expr:
    | s_block(l, stmts) =>
      resolve-scope(stmts).map(desugar-expr(nv, _))
    | s_app(l, f, params, args) =>
      A.s_app(desugar-expr(f), params, args.map(desugar-expr))
    | s_num(_, _) => expr
    | s_str(_, _) => expr
    | s_bool(_, _) => expr
  end
end

