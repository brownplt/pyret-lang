#lang pyret

provide *

import ast as A
import "./ast-anf.arr" as N
import "./gensym.arr" as G

fun mk-id(loc, base):
  t = G.make-name(base)
  { id: t, id-b: bind(loc, t), id-e: N.a-id(loc, t) }
end

data ANFCont:
  | cont(k :: (N.ALettable -> N.AExpr)) with:
    apply(self, l :: Loc, expr :: N.ALettable): self.k(expr) end
  | id(name :: String) with:
    apply(self, l :: Loc, expr :: N.ALettable):
      cases(N.ALettable) expr:
        | a-val(v) =>
          name = mk-id(l, "cont_tail_app")
          N.a-let(l, name.id-b, N.a-app(l, N.a-id(l, self.name), [v]),
            N.a-lettable(N.a-val(name.id-e)))
        | else =>
          e-name = mk-id(l, "cont_tail_arg")
          name = mk-id(l, "cont_tail_app")
          N.a-let(l, e-name.id-b, expr,
            N.a-let(l, name.id-b, N.a-app(l, N.a-id(l, self.name), [e-name.id-e]),
              N.a-lettable(N.a-val(name.id-e))))
      end
    end
end

fun anf-term(e :: A.Expr) -> N.AExpr:
  anf(e, cont(fun(x):
        cases(N.ALettable) x:
            # tail call
          | a-app(l, f, args) =>
            name = mk-id(l, "anf_tail_app")
            N.a-let(l, name.id-b, x, N.a-lettable(N.a-val(name.id-e)))
          | else => N.a-lettable(x)
        end
      end)
    )
end

fun bind(l, id): N.a-bind(l, id, A.a_blank);

fun anf-bind(b):
  cases(A.Bind) b:
    | s_bind(l, shadows, id, ann) => N.a-bind(l, tostring(id), ann)
  end
end

fun anf-name(expr :: A.Expr, name-hint :: String, k :: (N.AVal -> N.AExpr)) -> N.AExpr:
  anf(expr, cont(fun(lettable):
        cases(N.ALettable) lettable:
          | a-val(v) => k(v)
          | else =>
            t = mk-id(expr.l, name-hint)
            N.a-let(expr.l, t.id-b, lettable, k(t.id-e))
        end
      end))
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
        | s_file_import(fname) => N.a-import-file(l, fname, tostring(name))
        | s_const_import(module) => N.a-import-builtin(l, module, tostring(name))
      end
    | s_provide(l, block) => N.a-provide(l, anf-term(block))
    | else => raise("Unhandled header in anf-import: " + torepr(i))
  end
end

fun anf-block(es-init :: List<A.Expr>, k :: ANFCont):
  fun anf-block-help(es):
    cases (List<A.Expr>) es:
      | empty => raise("Empty block")
      | link(f, r) =>
        # Note: assuming blocks don't end in let/var here
        if r.length() == 0:
          anf(f, k)
        else:
          cases(A.Expr) f:
            | else => anf(f, cont(fun(lettable):
                    t = mk-id(f.l, "anf_begin_dropped")
                    N.a-let(f.l, t.id-b, lettable, anf-block-help(r))
                  end))
          end
        end
    end
  end
  anf-block-help(es-init)
end

fun anf(e :: A.Expr, k :: ANFCont) -> N.AExpr:
  cases(A.Expr) e:
    | s_num(l, n) => k.apply(l, N.a-val(N.a-num(l, n)))
    | s_frac(l, num, den) => k.apply(l, N.a-val(N.a-num(l, num / den))) # Possibly unneeded if removed by desugar?
    | s_str(l, s) => k.apply(l, N.a-val(N.a-str(l, s)))
    | s_undefined(l) => k.apply(l, N.a-val(N.a-undefined(l)))
    | s_bool(l, b) => k.apply(l, N.a-val(N.a-bool(l, b)))
    | s_id(l, id) => k.apply(l, N.a-val(N.a-id(l, tostring(id))))
    | s_id_var(l, id) => k.apply(l, N.a-val(N.a-id-var(l, tostring(id))))
    | s_id_letrec(l, id) => k.apply(l, N.a-val(N.a-id-letrec(l, tostring(id))))

    | s_let_expr(l, binds, body) =>
      cases(List) binds:
        | empty => anf(body, k)
        | link(f, r) =>
          cases(A.LetBind) f:
            | s_var_bind(l2, b, val) => anf(val, cont(fun(lettable):
                    N.a-var(l2, N.a-bind(l2, tostring(b.id), b.ann), lettable,
                      anf(A.s_let_expr(l, r, body), k))
                  end))
            | s_let_bind(l2, b, val) => anf(val, cont(fun(lettable):
                    N.a-let(l2, N.a-bind(l2, tostring(b.id), b.ann), lettable,
                      anf(A.s_let_expr(l, r, body), k))
                  end))
          end
      end

    | s_letrec(l, binds, body) =>
      let-binds = for map(b from binds):
        A.s_var_bind(b.l, b.b, A.s_undefined(l))
      end
      assigns = for map(b from binds):
        A.s_assign(b.l, tostring(b.b.id), b.value)
      end
      anf(A.s_let_expr(l, let-binds, A.s_block(l, assigns + [body])), k)

    | s_data_expr(l, name, params, mixins, variants, shared, _check) =>
      fun anf-member(member :: A.VariantMember):
        cases(A.VariantMember) member:
          | s_variant_member(l2, type, b) =>
            a-type = cases(A.VariantMemberType) type:
              | s_normal => N.a-normal
              | s_cyclic => N.a-cyclic
              | s_mutable => N.a-mutable
            end
            new-bind = cases(A.Bind) b:
              | s_bind(l, shadows, name, ann) =>
                cases(A.Name) name:
                  | s_atom(base, serial) => N.a-bind(l, base, ann)
                end
            end
            N.a-variant-member(l2, a-type, new-bind)
        end
      end
      fun anf-variant(v :: A.Variant, kv :: (N.AVariant -> N.AExpr)):
        cases(A.Variant) v:
          | s_variant(l2, vname, members, with-members) =>
            with-exprs = with-members.map(_.value)
            anf-name-rec(with-exprs, "anf_variant_member", fun(ts):
                new-fields = for map2(f from with-members, t from ts):
                    N.a-field(f.l, f.name.s, t)
                  end
                kv(N.a-variant(l, vname, members.map(anf-member), new-fields))
              end)
          | s_singleton_variant(l2, vname, with-members) =>
            with-exprs = with-members.map(_.value)
            anf-name-rec(with-exprs, "anf_singleton_variant_member", fun(ts):
                new-fields = for map2(f from with-members, t from ts):
                    N.a-field(f.l, f.name.s, t)
                  end
                kv(N.a-singleton-variant(l, vname, new-fields))
              end)
        end
      end
      fun anf-variants(vs :: List<A.Variant>, ks :: (List<N.AVariant> -> N.AExpr)):
        cases(List) vs:
          | empty => ks([])
          | link(f, r) =>
            anf-variant(f, fun(v): anf-variants(r, fun(rest-vs): ks([v] + rest-vs););)
        end
      end
      exprs = shared.map(_.value)

      anf-name-rec(exprs, "anf_shared", fun(ts):
          new-shared = for map2(f from shared, t from ts):
              N.a-field(f.l, f.name.s, t)
            end
          anf-variants(variants, fun(new-variants):
              k.apply(l, N.a-data-expr(l, name, new-variants, new-shared))
            end)
        end)

    | s_if_else(l, branches, _else) =>
      cases(ANFCont) k:
        | id(_) =>
          for fold(acc from anf(_else, k), branch from branches.reverse()):
            anf-name(branch.test, "anf_if",
              fun(test): N.a-if(l, test, anf(branch.body, k), acc) end)
          end
        | cont(_) =>
          helper = mk-id(l, "if_helper")
          arg = mk-id(l, "if_helper_arg")
          N.a-let(l, helper.id-b, N.a-lam(l, [arg.id-b], k.apply(l, N.a-val(arg.id-e))),
            for fold(acc from anf(_else, id(helper.id)), branch from branches.reverse()):
              anf-name(branch.test, "anf_if",
                fun(test): N.a-if(l, test, anf(branch.body, id(helper.id)), acc) end)
            end)
      end
    | s_try(l, body, id, _except) =>
      N.a-try(l, anf-term(body), id, anf-term(_except))

    | s_block(l, stmts) => anf-block(stmts, k)
    | s_user_block(l, body) => anf(body, k)

    | s_lam(l, params, args, ret, doc, body, _) =>
      k.apply(l, N.a-lam(l, args.map(fun(b): bind(b.l, tostring(b.id)) end), anf-term(body)))
    | s_method(l, args, ret, doc, body, _) =>
      k.apply(l, N.a-method(l, args.map(fun(b): bind(b.l, tostring(b.id)) end), anf-term(body)))

    | s_app(l, f, args) =>
      anf-name(f, "anf_fun", fun(v):
          anf-name-rec(args, "anf_arg", fun(vs):
              k.apply(l, N.a-app(l, v, vs))
            end)
        end)

    | s_prim_app(l, f, args) =>
      anf-name-rec(args, "anf_arg", fun(vs):
          k.apply(l, N.a-prim-app(l, f, vs))
        end)

    | s_dot(l, obj, field) =>
      anf-name(obj, "anf_bracket", fun(t-obj): k.apply(l, N.a-dot(l, t-obj, field)) end)

    | s_colon(l, obj, field) =>
      anf-name(obj, "anf_colon", fun(t-obj): k.apply(l, N.a-colon(l, t-obj, field)) end)

    | s_bracket(l, obj, field) =>
      fname = cases(A.Expr) field:
          | s_str(_, s) => s
          | else => raise("Non-string field: " + torepr(field))
        end
      anf-name(obj, "anf_bracket", fun(t-obj): k.apply(l, N.a-dot(l, t-obj, fname)) end)

    | s_colon_bracket(l, obj, field) =>
      fname = cases(A.Expr) field:
          | s_str(_, s) => s
          | else => raise("Non-string field: " + torepr(field))
        end
      anf-name(obj, "anf_colon", fun(t-obj): k.apply(l, N.a-colon(l, t-obj, fname)) end)

    | s_get_bang(l, obj, field) =>
      anf-name(obj, "anf_get_bang", fun(t): k.apply(l, N.a-get-bang(l, t, field)) end)

    | s_assign(l, id, value) =>
      anf-name(value, "anf_assign", fun(v): k.apply(l, N.a-assign(l, tostring(id), v)) end)

    | s_obj(l, fields) =>
      exprs = fields.map(_.value)

      anf-name-rec(exprs, "anf_obj", fun(ts):
          new-fields = for map2(f from fields, t from ts):
              N.a-field(f.l, f.name.s, t)
            end
          k.apply(l, N.a-obj(l, new-fields))
        end)

    | s_extend(l, obj, fields) =>
      exprs = fields.map(_.value)

      anf-name(obj, "anf_extend", fun(o):
          anf-name-rec(exprs, "anf_extend", fun(ts):
              new-fields = for map2(f from fields, t from ts):
                  N.a-field(f.l, f.name.s, t)
                end
              k.apply(l, N.a-extend(l, o, new-fields))
            end)
        end)

    | s_let(_, _, _) => raise("s_let should be handled by anf-block: " + torepr(e))
    | s_var(_, _, _) => raise("s_var should be handled by anf-block: " + torepr(e))
    | else => raise("Missed case in anf: " + torepr(e))
  end
end

