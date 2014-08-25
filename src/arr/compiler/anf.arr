#lang pyret

provide *
provide-types *

import ast as A
import srcloc as SL
import "compiler/ast-anf.arr" as N

type Loc = SL.Srcloc

names = A.global-names

fun mk-id(loc, base):
  t = names.make-atom(base)
  { id: t, id-b: bind(loc, t), id-e: N.a-id(loc, t) }
end

data ANFCont:
  | k-cont(k :: (N.ALettable -> N.AExpr)) with:
    apply(self, l :: Loc, expr :: N.ALettable): self.k(expr) end
  | k-id(name :: A.Name) with:
    apply(self, l :: Loc, expr :: N.ALettable):
      cases(N.ALettable) expr:
        | a-val(l2, v) =>
          name = mk-id(l2, "cont_tail_app")
          N.a-let(l, name.id-b, N.a-app(l, N.a-id(l, self.name), [list: v]),
            N.a-lettable(l2, N.a-val(l2, name.id-e)))
        | else =>
          e-name = mk-id(l, "cont_tail_arg")
          name = mk-id(l, "cont_tail_app")
          N.a-let(l, e-name.id-b, expr,
            N.a-lettable(l, N.a-app(l, N.a-id(l, self.name), [list: e-name.id-e])))
      end
    end
end

fun anf-term(e :: A.Expr) -> N.AExpr:
  anf(e, k-cont(lam(x):
        cases(N.ALettable) x:
            # tail call
          | a-app(l, f, args) =>
            name = mk-id(l, "anf_tail_app")
            N.a-let(l, name.id-b, x, N.a-lettable(l, N.a-val(l, name.id-e)))
          | else => N.a-lettable(x.l, x)
        end
      end)
    )
end

fun bind(l, id): N.a-bind(l, id, A.a-blank);

fun anf-bind(b):
  cases(A.Bind) b:
    | s-bind(l, shadows, id, ann) => N.a-bind(l, id, ann)
  end
end

fun anf-cases-bind(cb :: A.CasesBind):
  cases(A.CasesBind) cb:
    | s-cases-bind(l, typ, b) => N.a-cases-bind(l, typ, anf-bind(b))
  end
end

fun anf-cases-branch(branch):
  cases(A.CasesBranch) branch:
    | s-cases-branch(l, pat-loc, name, args, body) =>
      N.a-cases-branch(l, pat-loc, name, args.map(anf-cases-bind), anf-term(body))
    | s-singleton-cases-branch(l, pat-loc, name, body) =>
      N.a-singleton-cases-branch(l, pat-loc, name, anf-term(body))
  end
end

fun anf-name(expr :: A.Expr, name-hint :: String, k :: (N.AVal -> N.AExpr)) -> N.AExpr:
  anf(expr, k-cont(lam(lettable):
        cases(N.ALettable) lettable:
          | a-val(_, v) => k(v)
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
    | empty => k([list: ])
    | link(f, r) =>
      anf-name(f, name-hint, lam(v):
          anf-name-rec(r, name-hint, lam(vs): k([list: v] + vs);)
        end)
  end
end

fun anf-program(e :: A.Program):
  cases(A.Program) e:
    | s-program(l, _, _, imports, block) =>
      # Note: provides have been desugared away; if this changes, revise this line
      N.a-program(l, imports.map(anf-import), anf-term(block))
  end
end

fun anf-import(i :: A.Import):
  cases(A.Import) i:
    | s-import(l, f, name) =>
      itype = cases(A.ImportType) f:
        | s-file-import(_, fname) => N.a-import-file(l, fname)
        | s-const-import(_, mod) => N.a-import-builtin(l, mod)
        | s-special-import(_, kind, args) => N.a-import-special(l, kind, args)
      end
      N.a-import(l, itype, name)
    | s-import-types(l, f, name, types) =>
      itype = cases(A.ImportType) f:
        | s-file-import(_, fname) => N.a-import-file(l, fname)
        | s-const-import(_, mod) => N.a-import-builtin(l, mod)
        | s-special-import(_, kind, args) => N.a-import-special(l, kind, args)
      end
      N.a-import-types(l, itype, name, types)
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
            | else => anf(f, k-cont(lam(lettable):
                    N.a-seq(f.l, lettable, anf-block-help(r))
                  end))
          end
        end
    end
  end
  anf-block-help(es-init)
end

fun anf(e :: A.Expr, k :: ANFCont) -> N.AExpr:
  cases(A.Expr) e:
    | s-module(l, answer, provides, types, checks) =>
      anf-name(answer, "answer", lam(ans):
          anf-name(provides, "provides", lam(provs):
              anf-name(checks, "checks", lam(chks):
                  k.apply(l, N.a-module(l, ans, provs, types, chks))
                end)
            end)
        end)
    | s-num(l, n) => k.apply(l, N.a-val(l, N.a-num(l, n)))
    | s-frac(l, num, den) => k.apply(l, N.a-val(l, N.a-num(l, num / den))) # Possibly unneeded if removed by desugar?
    | s-str(l, s) => k.apply(l, N.a-val(l, N.a-str(l, s)))
    | s-undefined(l) => k.apply(l, N.a-val(l, N.a-undefined(l)))
    | s-bool(l, b) => k.apply(l, N.a-val(l, N.a-bool(l, b)))
    | s-id(l, id) => k.apply(l, N.a-val(l, N.a-id(l, id)))
    | s-id-var(l, id) => k.apply(l, N.a-val(l, N.a-id-var(l, id)))
    | s-id-letrec(l, id, safe) =>
      k.apply(l, N.a-val(l, N.a-id-letrec(l, id, safe)))
    | s-srcloc(l, loc) => k.apply(l, N.a-val(l, N.a-srcloc(l, loc)))
    | s-type-let-expr(l, binds, body) =>
      cases(List) binds:
        | empty => anf(body, k)
        | link(f, r) =>
          new-bind = cases(A.TypeLetBind) f:
            | s-type-bind(l2, name, ann) =>
              N.a-type-bind(l2, name, ann)
            | s-newtype-bind(l2, name, namet) =>
              N.a-newtype-bind(l2, name, namet)
          end
          N.a-type-let(l, new-bind, anf(A.s-type-let-expr(l, r, body), k))
      end
    | s-let-expr(l, binds, body) =>
      cases(List) binds:
        | empty => anf(body, k)
        | link(f, r) =>
          cases(A.LetBind) f:
            | s-var-bind(l2, b, val) =>
              if A.is-a-blank(b.ann) or A.is-a-any(b.ann):
                anf-name(val, "var", lam(new-val):
                      N.a-var(l2, N.a-bind(l2, b.id, b.ann), N.a-val(new-val.l, new-val),
                        anf(A.s-let-expr(l, r, body), k))
                    end)
              else:
                var-name = mk-id(l2, "var")
                anf(val, k-cont(lam(lettable):
                      N.a-let(l2, var-name.id-b, lettable,
                        N.a-var(l2, N.a-bind(l2, b.id, b.ann), N.a-val(l2, var-name.id-e),
                          anf(A.s-let-expr(l, r, body), k)))
                    end))
              end
            | s-let-bind(l2, b, val) => anf(val, k-cont(lam(lettable):
                    N.a-let(l2, N.a-bind(l2, b.id, b.ann), lettable,
                      anf(A.s-let-expr(l, r, body), k))
                  end))
          end
      end

    | s-letrec(l, binds, body) =>
      let-binds = for map(b from binds):
        A.s-var-bind(b.l, b.b, A.s-undefined(l))
      end
      assigns = for map(b from binds):
        A.s-assign(b.l, b.b.id, b.value)
      end
      anf(A.s-let-expr(l, let-binds, A.s-block(l, assigns + [list: body])), k)

    | s-data-expr(l, data-name, data-name-t, params, mixins, variants, shared, _check) =>
      fun anf-member(member :: A.VariantMember):
        cases(A.VariantMember) member:
          | s-variant-member(l2, typ, b) =>
            a-type = cases(A.VariantMemberType) typ:
              | s-normal => N.a-normal
              | s-mutable => N.a-mutable
            end
            new-bind = cases(A.Bind) b:
              | s-bind(l3, shadows, name, ann) =>
                N.a-bind(l3, name, ann)
            end
            N.a-variant-member(l2, a-type, new-bind)
        end
      end
      fun anf-variant(v :: A.Variant, kv :: (N.AVariant -> N.AExpr)):
        cases(A.Variant) v:
          | s-variant(l2, constr-loc, vname, members, with-members) =>
            with-exprs = with-members.map(_.value)
            anf-name-rec(with-exprs, "anf_variant_member", lam(ts):
                new-fields = for map2(f from with-members, t from ts):
                    N.a-field(f.l, f.name, t)
                  end
                kv(N.a-variant(l2, constr-loc, vname, members.map(anf-member), new-fields))
              end)
          | s-singleton-variant(l2, vname, with-members) =>
            with-exprs = with-members.map(_.value)
            anf-name-rec(with-exprs, "anf_singleton_variant_member", lam(ts):
                new-fields = for map2(f from with-members, t from ts):
                    N.a-field(f.l, f.name, t)
                  end
                kv(N.a-singleton-variant(l2, vname, new-fields))
              end)
        end
      end
      fun anf-variants(vs :: List<A.Variant>, ks :: (List<N.AVariant> -> N.AExpr)):
        cases(List) vs:
          | empty => ks([list: ])
          | link(f, r) =>
            anf-variant(f, lam(v): anf-variants(r, lam(rest-vs): ks([list: v] + rest-vs););)
        end
      end
      exprs = shared.map(_.value)

      anf-name-rec(exprs, "anf_shared", lam(ts):
          new-shared = for map2(f from shared, t from ts):
              N.a-field(f.l, f.name, t)
            end
          anf-variants(variants, lam(new-variants):
              k.apply(l, N.a-data-expr(l, data-name, data-name-t, new-variants, new-shared))
            end)
        end)

    | s-if-else(l, branches, _else) =>
      fun anf-if-branches(shadow k, shadow branches):
        cases(List) branches:
          | empty => raise("Empty branches")
          | link(f, r) =>
            cases(List) r:
              | empty =>
                anf-name(
                  f.test,
                  "anf_if",
                  lam(test): k.apply(l, N.a-if(l, test, anf-term(f.body), anf-term(_else))) end
                  )
              | link(f2, r2) =>
                anf-name(
                  f.test,
                  "anf_if",
                  lam(test):
                    k.apply(l, N.a-if(l, test, anf-term(f.body),
                        anf-if-branches(k-cont(lam(if-expr): N.a-lettable(l, if-expr) end), r)))
                  end
                  )
            end
        end
      end
      anf-if-branches(k, branches)
    | s-cases-else(l, typ, val, branches, _else) =>
      anf-name(val, "cases_val",
        lam(v): k.apply(l, N.a-cases(l, typ, v, branches.map(anf-cases-branch), anf-term(_else))) end)
    | s-try(l, body, id, _except) =>
      N.a-try(l, anf-term(body), id, anf-term(_except))

    | s-block(l, stmts) => anf-block(stmts, k)
    | s-user-block(l, body) => anf(body, k)

    | s-lam(l, params, args, ret, doc, body, _) =>
      name = mk-id(l, "ann_check_temp")
      k.apply(l, N.a-lam(l, args.map(lam(a): N.a-bind(a.l, a.id, a.ann);), ret,
                  anf-term(A.s-let-expr(l,
                    [list: A.s-let-bind(l, A.s-bind(l, false, name.id, ret), body)],
                    A.s-id(l, name.id)))))
    | s-method(l, args, ret, doc, body, _) =>
      name = mk-id(l, "ann_check_temp")
      k.apply(l, N.a-method(l, args.map(lam(a): N.a-bind(a.l, a.id, a.ann);), ret,
                  anf-term(A.s-let-expr(l,
                    [list: A.s-let-bind(l, A.s-bind(l, false, name.id, ret), body)],
                    A.s-id(l, name.id)))))

    | s-array(l, values) =>
      anf-name-rec(values, "anf_array_val", lam(vs):
        k.apply(l, N.a-array(l, vs))
      end)

    | s-app(l, f, args) =>
      anf-name(f, "anf_fun", lam(v):
          anf-name-rec(args, "anf_arg", lam(vs):
              k.apply(l, N.a-app(l, v, vs))
            end)
        end)

    | s-prim-app(l, f, args) =>
      anf-name-rec(args, "anf_arg", lam(vs):
          k.apply(l, N.a-prim-app(l, f, vs))
        end)

    | s-instantiate(_, body, _) =>
      anf(body, k)

    | s-dot(l, obj, field) =>
      anf-name(obj, "anf_bracket", lam(t-obj): k.apply(l, N.a-dot(l, t-obj, field)) end)

    | s-bracket(l, obj, field) =>
      fname = cases(A.Expr) field:
          | s-str(_, s) => s
          | else => raise("Non-string field: " + torepr(field))
        end
      anf-name(obj, "anf_bracket", lam(t-obj): k.apply(l, N.a-dot(l, t-obj, fname)) end)

    | s-ref(l, ann) =>
      k.apply(l, N.a-ref(l, ann))

    | s-get-bang(l, obj, field) =>
      anf-name(obj, "anf_get_bang", lam(t): k.apply(l, N.a-get-bang(l, t, field)) end)

    | s-assign(l, id, value) =>
      anf-name(value, "anf_assign", lam(v): k.apply(l, N.a-assign(l, id, v)) end)

    | s-obj(l, fields) =>
      exprs = fields.map(_.value)

      anf-name-rec(exprs, "anf_obj", lam(ts):
          new-fields = for map2(f from fields, t from ts):
              N.a-field(f.l, f.name, t)
            end
          k.apply(l, N.a-obj(l, new-fields))
        end)

    | s-update(l, obj, fields) =>
      exprs = fields.map(_.value)

      anf-name(obj, "anf_update", lam(o):
          anf-name-rec(exprs, "anf_update", lam(ts):
              new-fields = for map2(f from fields, t from ts):
                  N.a-field(f.l, f.name, t)
                end
              k.apply(l, N.a-update(l, o, new-fields))
            end)
        end)

    | s-extend(l, obj, fields) =>
      exprs = fields.map(_.value)

      anf-name(obj, "anf_extend", lam(o):
          anf-name-rec(exprs, "anf_extend", lam(ts):
              new-fields = for map2(f from fields, t from ts):
                  N.a-field(f.l, f.name, t)
                end
              k.apply(l, N.a-extend(l, o, new-fields))
            end)
        end)

    | s-let(_, _, _) => raise("s-let should be handled by anf-block: " + torepr(e))
    | s-var(_, _, _) => raise("s-var should be handled by anf-block: " + torepr(e))
    | else => raise("Missed case in anf: " + torepr(e))
  end
end

