#lang pyret

import ast as A
import "ast-anf.arr" as N

provide *

fun anf-term(e :: A.Expr):
  normalize(e, fun(x): N.a-lettable(x);)
end

fun is-value(e :: A.Expr):
  cases(A.Expr) e:
    | a-bool(_, _) => true
    | a-str(_, _) => true
    | a-num(_, _) => true
    | a-id(_, _) => true
  end
end

fun bind(l, id): N.s-bind(l, id, A.a_blank);

fun mk-id(loc, base):
  t = gensym(base)
  { id: t, id-b: bind(loc, t), id-e: N.s-id(loc, t) }
end

fun anf-name(expr :: A.Expr, name-hint :: String, k :: (N.AVal -> N.AExpr)):
  t = mk-id(l, name-hint)
  anf(expr, fun(lettable):
      N.a-let(l, t.id-b, lettable, k(t.id-e))
    end)
end

fun anf-name-rec(exprs :: A.Expr, name-hint :: String, k :: (List<N.AVal> -> N.AExpr)):
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
      N.a-program(l, 
      anf-term(block)
  end
end

fun anf-import(i :: A.Header):
  cases(A.Header) i:
    | s_import(l, f, name) =>
      cases(A.ImportType) f:
        | s_file_import(fname) => N.a-import-file(l, fname, name)
        | s_const_import(module) => N.a-import-file(l, module, name)
      end
    | s_provide(l, block) => N.a-provide(l, ast-term(block))
  end
end

fun anf-block(es :: List<A.Expr>, k :: (N.ALettable -> N.AExpr)):
  ids = A.block-ids(A.s_block(N.dummy-loc, es))
  shell = for fold(s from fun(e): e; id from ids):
      fun(e):
        N.a-let(N.dummy-loc, N.bind(N.dummy-loc, id), N.undefined(N.dummy-loc), e)
      end
    end
  fun handle-id(l, bind, e, rest):
    anf(e, fun(lettable):
      t = mk-id(b.loc, "anf-let")
      N.a-let(f.loc, t.id-b, lettable,
        N.a-begin(f.loc, N.a-assign(f.loc, b.id, N.a-val(t.id-e)),
          anf-block-help(r)))
    end)
  end
  fun anf-block-help(es):
    cases (List<A.Expr>) M:
      | empty => empty
      | link(f, r) =>
        # Note: assuming blocks don't end in let/var here
        if r.length() == 0:
          anf(f, k)
        else:
          cases(A.Expr) f:
            | s_let(l, b, e) => handle-id(l, b, e, r)
            | s_var(l, b, e) => handle-id(l, b, e, r)
            | else => anf(f, fun(lettable):
                  N.a-begin(f.loc, lettable, anf-block-help(r))
                end)
          end
        end
    end
  end
  shell(anf-block-help(es))
end

fun anf(e :: A.Expr, k :: (N.ALettable -> N.AExpr)):
  cases(A.Expr) e:
    | s_num(l, n) => k(N.a-val(N.a-num(l, n)))
    | s_str(l, s) => k(N.a-val(N.a-str(l, s)))
    | s_bool(l, b) => k(N.a-val(N.a-bool(l, b)))
    | s_id(l, id) => k(N.a-val(N.a-id(l, id)))

    | s_if_else(l, branches, _else) =>
      if not(is-empty(branches)):
        s-if = for fold(acc from _else, branch from branches):
          A.s_if_else(l, [branch], acc)
        end
        cond = s-if.branches.first.test
        consq = s-if.branches.first.body
        altern = s-if._else
        anf-name(cond, fun(t):
            N.a-if(l, t, anf-term(consq), anf-term(altern))
          end)
      else:
        anf(_else, k)
      end

    | s_try(l, body, id, _except) =>
      N.a-try(l, anf-term(body), id, anf-term(_except))

    | s_block(l, stmts) => anf-block(stmts, k)
    | s_user_block(l, body) => anf(body, k)

    | s_lam(l, params, args, ret, doc, body, check)
      k(N.a-lam(l, args.map(fun(b): bind(b.loc, b.id)), anf-term(body)))
    | s_method(l, args, ret, doc, body, check)
      k(N.a-method(l, args.map(fun(b): bind(b.loc, b.id)), anf-term(body)))

    | s_bracket(l, obj, field) =>
      fname = cases(A.Expr) field:
          | s_str(l, s) => s
          | else => raise("Non-string field: " + s)
        end
      anf-name(obj, fun(t-obj): k(N.s-bracket(l, t-obj, fname)))

    | s_colon_bracket(l, obj, field) =>
      fname = cases(A.Expr) field:
          | s_str(l, s) => s
          | else => raise("Non-string field: " + s)
        end
      anf-name(obj, fun(t-obj): k(N.s-colon(l, t-obj, fname)) end)

    | s_get_bang(l, obj, field) =>
      normalize-name(obj, fun(t): k(A.s_get_bang(l, t, field)) end)

    | s_assign(l, id, value) =>
      anf-name(value, "anf-assign", fun(v): k(N.a-assign(l, id, v)) end)

    | s_obj(l, fields) =>
      names = fields.map(_.name)
      exprs = fields.map(_.value)

      anf-name-rec(exprs, fun(ts):
          new-fields = for map2(f from fields, t from ts):
              N.a-field(f.loc, f.name, t)
            end
          k(N.a-obj(new-fields))
        end)

    | s_obj(l, obj, fields) =>
      names = fields.map(_.name)
      exprs = fields.map(_.value)

      anf-name(obj, fun(o):
          anf-name-rec(exprs, fun(ts):
              new-fields = for map2(f from fields, t from ts):
                  N.a-field(f.loc, f.name, t)
                end
              k(N.a-update(obj, new-fields))
            end)
        end)

    | s_let(_, _, _) => raise("s_let should be handled by anf-block: " + torepr(e))
    | s_var(_, _, _) => raise("s_var should be handled by anf-block: " + torepr(e))
  end
end

