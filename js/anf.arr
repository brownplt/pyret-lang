#lang pyret

provide *

import ast as A
import "ast-anf.arr" as N
import "gensym.arr" as G

fun anf-term(e :: A.Expr) -> N.AExpr:
  anf(e, fun(x): N.a-lettable(x);)
end

fun bind(l, id): N.a-bind(l, id, A.a_blank);

fun mk-id(loc, base):
  t = G.make-name(base)
  { id: t, id-b: bind(loc, t), id-e: N.a-id(loc, t) }
end

fun anf-name(expr :: A.Expr, name-hint :: String, k :: (N.AVal -> N.AExpr)) -> N.AExpr:
  anf(expr, fun(lettable):
      cases(N.ALettable) lettable:
        | a-val(v) => k(v)
        | else =>
          t = mk-id(expr.l, name-hint)
          N.a-let(expr.l, t.id-b, lettable, k(t.id-e))
      end
    end)
end

fun anf-name-rec(
    exprs :: List<A.Expr>,
    name-hint :: String,
    k :: (List<N.AVal> -> N.AExpr)
  ) -> N.AExpr:
  cases(List) exprs:
    | empty => k([])
    | link(f, r) =>
      anf-name(f, name-hint, fun(v):
          anf-name-rec(r, name-hint, fun(vs): k([v] + vs);)
        end)
  end
end

fun anf-program(e :: A.Program):
  cases(A.Program) e:
    | s_program(l, imports, block) =>
      N.a-program(l, imports.map(anf-import), anf-term(block))
  end
end

fun anf-import(i :: A.Header):
  cases(A.Header) i:
    | s_import(l, f, name) =>
      cases(A.ImportType) f:
        | s_file_import(fname) => N.a-import-file(l, fname, name)
        | s_const_import(module) => N.a-import-builtin(l, module, name)
      end
    | s_provide(l, block) => N.a-provide(l, anf-term(block))
    | else => raise("Unhandled header in anf-import: " + torepr(i))
  end
end

fun anf-block(es-init :: List<A.Expr>, k :: (N.ALettable -> N.AExpr)):
  fun anf-block-help(es):
    cases (List<A.Expr>) es:
      | empty => raise("Empty block")
      | link(f, r) =>
        # Note: assuming blocks don't end in let/var here
        if r.length() == 0:
          anf(f, k)
        else:
          cases(A.Expr) f:
            | else => anf(f, fun(lettable):
                  t = mk-id(f.l, "anf_begin_dropped")
                  N.a-let(f.l, t.id-b, lettable, anf-block-help(r))
                end)
          end
        end
    end
  end
  anf-block-help(es-init)
end

fun anf(e :: A.Expr, k :: (N.ALettable -> N.AExpr)) -> N.AExpr:
  cases(A.Expr) e:
    | s_num(l, n) => k(N.a-val(N.a-num(l, n)))
    | s_str(l, s) => k(N.a-val(N.a-str(l, s)))
    | s_undefined(l) => k(N.a-val(N.a-undefined(l)))
    | s_bool(l, b) => k(N.a-val(N.a-bool(l, b)))
    | s_id(l, id) => k(N.a-val(N.a-id(l, id)))
    | s_id_var(l, id) => k(N.a-val(N.a-id-var(l, id)))
    | s_id_letrec(l, id) => k(N.a-val(N.a-id-letrec(l, id)))

    | s_let_expr(l, binds, body) =>
      cases(List) binds:
        | empty => anf(body, k)
        | link(f, r) =>
          cases(A.LetBind) f:
            | s_var_bind(l2, b, val) => anf(val, fun(lettable):
              N.a-var(l2, N.a-bind(l2, b.id, b.ann), lettable,
                anf(A.s_let_expr(l, r, body), k)) end)
            | s_let_bind(l2, b, val) => anf(val, fun(lettable):
              N.a-let(l2, N.a-bind(l2, b.id, b.ann), lettable,
                anf(A.s_let_expr(l, r, body), k)) end)
          end
      end

    | s_letrec(l, binds, body) =>
      let-binds = for map(b from binds):
        A.s_var_bind(b.l, b.b, A.s_undefined(l))
      end
      assigns = for map(b from binds):
        A.s_assign(b.l, b.b.id, b.value)
      end
      anf(A.s_let_expr(l, let-binds, A.s_block(l, assigns + [body])), k)
    | s_if_else(l, branches, _else) =>
      if not is-empty(branches):
        s-if = for fold(acc from _else, branch from branches.reverse()):
          A.s_if_else(l, [branch], acc)
        end
        cond = s-if.branches.first.test
        consq = s-if.branches.first.body
        altern = s-if._else
        anf-name(cond, "anf_if", fun(t):
            k(N.a-if(l, t, anf-term(consq), anf-term(altern)))
          end)
      else:
        anf(_else, k)
      end

    | s_try(l, body, id, _except) =>
      N.a-try(l, anf-term(body), id, anf-term(_except))

    | s_block(l, stmts) => anf-block(stmts, k)
    | s_user_block(l, body) => anf(body, k)

    | s_lam(l, params, args, ret, doc, body, check) =>
      k(N.a-lam(l, args.map(fun(b): bind(b.l, b.id) end), anf-term(body)))
    | s_method(l, args, ret, doc, body, check) =>
      k(N.a-method(l, args.map(fun(b): bind(b.l, b.id) end), anf-term(body)))

    | s_app(l, f, args) =>
      anf-name(f, "anf_fun", fun(v):
          anf-name-rec(args, "anf_arg", fun(vs):
              k(N.a-app(l, v, vs))
            end)
        end)

    | s_dot(l, obj, field) =>
      anf-name(obj, "anf_bracket", fun(t-obj): k(N.a-dot(l, t-obj, field)) end)

    | s_colon(l, obj, field) =>
      anf-name(obj, "anf_colon", fun(t-obj): k(N.a-colon(l, t-obj, field)) end)

    | s_bracket(l, obj, field) =>
      fname = cases(A.Expr) field:
          | s_str(_, s) => s
          | else => raise("Non-string field: " + torepr(field))
        end
      anf-name(obj, "anf_bracket", fun(t-obj): k(N.a-dot(l, t-obj, fname)) end)

    | s_colon_bracket(l, obj, field) =>
      fname = cases(A.Expr) field:
          | s_str(_, s) => s
          | else => raise("Non-string field: " + torepr(field))
        end
      anf-name(obj, "anf_colon", fun(t-obj): k(N.a-colon(l, t-obj, fname)) end)

    | s_get_bang(l, obj, field) =>
      anf-name(obj, "anf_get_bang", fun(t): k(N.a-get-bang(l, t, field)) end)

    | s_assign(l, id, value) =>
      anf-name(value, "anf_assign", fun(v): k(N.a-assign(l, id, v)) end)

    | s_obj(l, fields) =>
      names = fields.map(fun(f): f.name.s;)
      exprs = fields.map(_.value)

      anf-name-rec(exprs, "anf_obj", fun(ts):
          new-fields = for map2(f from fields, t from ts):
              N.a-field(f.l, f.name.s, t)
            end
          k(N.a-obj(l, new-fields))
        end)

    | s_extend(l, obj, fields) =>
      names = fields.map(_.name)
      exprs = fields.map(_.value)

      anf-name(obj, "anf_extend", fun(o):
          anf-name-rec(exprs, "anf_extend", fun(ts):
              new-fields = for map2(f from fields, t from ts):
                  N.a-field(f.l, f.name.s, t)
                end
              k(N.a-extend(l, o, new-fields))
            end)
        end)

    | s_let(_, _, _) => raise("s_let should be handled by anf-block: " + torepr(e))
    | s_var(_, _, _) => raise("s_var should be handled by anf-block: " + torepr(e))
    | else => raise("Missed case in anf: " + torepr(e))
  end
end

