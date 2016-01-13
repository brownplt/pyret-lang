#lang pyret

provide *
provide-types *

import ast as A
import srcloc as SL
import "compiler/ast-anf.arr" as N

type Loc = SL.Srcloc

fun get-value(o): o.value end

names = A.global-names

fun mk-id(loc, base):
  t = names.make-atom(base)
  { id: t, id-b: bind(loc, t), id-e: N.a-id(loc, t) }
end

data ANFCont:
  | k-cont(k :: (N.ALettable -> N.AExpr)) with:
    apply(self, l :: Loc, expr :: N.ALettable): self.k(expr) end
end

data Scope:
  | scope-c(bindings :: List<A.LetrecBind>, current :: Option<A.Name>) with:
    setup-bindings(self, bindings :: List<A.LetrecBind>) -> Scope:
      scope-c(bindings + self.bindings.filter(lam(b :: A.LetrecBind): A.is-s-lam(b.value) end), self.current)
    end,
    activate(self, lambda :: A.Expr) -> Scope:
      maybe-bind = lists.find(lam(b :: A.LetrecBind): identical(b.value, lambda) end, self.bindings)
      scope-c(self.bindings, if is-some(maybe-bind): some(maybe-bind.value.b.id) else: none end)
    end,
    is-matching(self, name :: A.Name) -> Boolean:
      is-some(self.current) and (self.current.value == name)
    end
end

fun anf-term(e :: A.Expr, scope :: Scope) -> N.AExpr:
  anf(e, k-cont(lam(x): N.a-lettable(x.l, x) end), scope)
end

fun bind(l, id): N.a-bind(l, id, A.a-blank);

fun anf-bind(b :: A.Bind):
  cases(A.Bind) b:
    | s-bind(l, shadows, id, ann) => N.a-bind(l, id, ann)
  end
end

fun anf-cases-bind(cb :: A.CasesBind):
  cases(A.CasesBind) cb:
    | s-cases-bind(l, typ, b) => N.a-cases-bind(l, typ, anf-bind(b))
  end
end

fun anf-cases-branch(branch, scope :: Scope):
  cases(A.CasesBranch) branch:
    | s-cases-branch(l, pat-loc, name, args, body) =>
      N.a-cases-branch(l, pat-loc, name, args.map(anf-cases-bind), anf-term(body, scope))
    | s-singleton-cases-branch(l, pat-loc, name, body) =>
      N.a-singleton-cases-branch(l, pat-loc, name, anf-term(body, scope))
  end
end

fun anf-name(expr :: A.Expr, name-hint :: String, k :: (N.AVal -> N.AExpr), scope :: Scope) -> N.AExpr:
  anf(expr, k-cont(lam(lettable):
        cases(N.ALettable) lettable:
          | a-val(_, v) => k(v)
          | else =>
            t = mk-id(expr.l, name-hint)
            N.a-let(expr.l, t.id-b, lettable, k(t.id-e))
        end
      end), scope)
end

fun anf-name-rec(
    exprs :: List<A.Expr>,
    name-hint :: String,
    k :: (List<N.AVal> -> N.AExpr),
    scope :: Scope) -> N.AExpr:
  cases(List) exprs:
    | empty => k([list: ])
    | link(f, r) =>
      anf-name(f, name-hint, lam(v):
          anf-name-rec(r, name-hint, lam(vs): k([list: v] + vs) end, scope)
        end, scope)
  end
end

fun anf-program(e :: A.Program):
  cases(A.Program) e:
    | s-program(l, _, _, imports, block) =>
      # Note: provides have been desugared away; if this changes, revise this line
      N.a-program(l, imports.map(anf-import), anf-term(block, scope-c(empty, none)))
  end
end

fun anf-import-type(it :: A.ImportType):
  cases(A.ImportType) it:
    | s-file-import(l, fname) => N.a-import-file(l, fname)
    | s-const-import(l, mod) => N.a-import-builtin(l, mod)
    | s-special-import(l, kind, args) => N.a-import-special(l, kind, args)
  end
end

fun anf-import(i :: A.Import):
  cases(A.Import) i:
    | s-import-complete(l, vals, types, f, val-name, types-name) =>
      itype = cases(A.ImportType) f:
        | s-file-import(_, fname) => N.a-import-file(l, fname)
        | s-const-import(_, mod) => N.a-import-builtin(l, mod)
        | s-special-import(_, kind, args) => N.a-import-special(l, kind, args)
      end
      N.a-import-complete(l, vals, types, itype, val-name, types-name)
  end
end

fun anf-block(es-init :: List<A.Expr>, k :: ANFCont, scope :: Scope):
  fun anf-block-help(es):
    cases (List<A.Expr>) es:
      | empty => raise("Empty block")
      | link(f, r) =>
        # Note: assuming blocks don't end in let/var here
        if r.length() == 0:
          anf(f, k, scope)
        else:
          cases(A.Expr) f:
            | else => anf(f, k-cont(lam(lettable):
                    N.a-seq(f.l, lettable, anf-block-help(r))
                  end), scope)
          end
        end
    end
  end
  anf-block-help(es-init)
end

fun anf(e :: A.Expr, k :: ANFCont, scope :: Scope) -> N.AExpr:
  cases(A.Expr) e:
    | s-module(l, answer, dvs, dts, provides, types, checks) =>
      advs = for map(dv from dvs):
        aval = cases(A.Expr) dv.value:
          | s-id(shadow l, id) => N.a-id(l, id)
          | s-id-var(shadow l, id) => N.a-id-var(l, id)
          | s-id-letrec(shadow l, id, safe) => N.a-id-letrec(l, id, safe)
          | else => raise("Got non-id in defined-value list " + torepr(dv))
        end
        N.a-defined-value(dv.name, aval)
      end
      adts = for map(dt from dts):
        N.a-defined-type(dt.name, dt.typ)
      end
      anf-name(answer, "answer", lam(ans):
          anf-name(provides, "provides", lam(provs):
              anf-name(checks, "checks", lam(chks):
                  k.apply(l, N.a-module(l, ans, advs, adts, provs, types, chks))
                end, scope)
            end, scope)
        end, scope)
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
        | empty => anf(body, k, scope)
        | link(f, r) =>
          new-bind = cases(A.TypeLetBind) f:
            | s-type-bind(l2, name, ann) =>
              N.a-type-bind(l2, name, ann)
            | s-newtype-bind(l2, name, namet) =>
              N.a-newtype-bind(l2, name, namet)
          end
          N.a-type-let(l, new-bind, anf(A.s-type-let-expr(l, r, body), k, scope))
      end
    | s-let-expr(l, binds, body) =>
      cases(List) binds:
        | empty => anf(body, k, scope)
        | link(f, r) =>
          cases(A.LetBind) f:
            | s-var-bind(l2, b, val) =>
              if A.is-a-blank(b.ann) or A.is-a-any(b.ann):
                anf-name(val, "var", lam(new-val):
                      N.a-var(l2, N.a-bind(l2, b.id, b.ann), N.a-val(new-val.l, new-val),
                        anf(A.s-let-expr(l, r, body), k, scope))
                    end, scope)
              else:
                var-name = mk-id(l2, "var")
                anf(val, k-cont(lam(lettable):
                      N.a-let(l2, var-name.id-b, lettable,
                        N.a-var(l2, N.a-bind(l2, b.id, b.ann), N.a-val(l2, var-name.id-e),
                          anf(A.s-let-expr(l, r, body), k, scope)))
                    end), scope)
              end
            | s-let-bind(l2, b, val) => anf(val, k-cont(lam(lettable):
                    N.a-let(l2, N.a-bind(l2, b.id, b.ann), lettable,
                      anf(A.s-let-expr(l, r, body), k, scope))
                  end), scope)
          end
      end

    | s-letrec(l, binds, body) =>
      let-binds = for map(b from binds):
        A.s-var-bind(b.l, b.b, A.s-undefined(l))
      end
      assigns = for map(b from binds):
        A.s-assign(b.l, b.b.id, b.value)
      end
      new-scope = scope.setup-bindings(binds)
      anf(A.s-let-expr(l, let-binds, A.s-block(l, assigns + [list: body])), k, new-scope)

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
            with-exprs = with-members.map(get-value)
            anf-name-rec(with-exprs, "anf_variant_member", lam(ts):
                new-fields = for map2(f from with-members, t from ts):
                    N.a-field(f.l, f.name, t)
                  end
                kv(N.a-variant(l2, constr-loc, vname, members.map(anf-member), new-fields))
              end, scope)
          | s-singleton-variant(l2, vname, with-members) =>
            with-exprs = with-members.map(get-value)
            anf-name-rec(with-exprs, "anf_singleton_variant_member", lam(ts):
                new-fields = for map2(f from with-members, t from ts):
                    N.a-field(f.l, f.name, t)
                  end
                kv(N.a-singleton-variant(l2, vname, new-fields))
              end, scope)
        end
      end
      fun anf-variants(vs :: List<A.Variant>, ks :: (List<N.AVariant> -> N.AExpr)):
        cases(List) vs:
          | empty => ks([list: ])
          | link(f, r) =>
            anf-variant(f, lam(v): anf-variants(r, lam(rest-vs): ks([list: v] + rest-vs););)
        end
      end
      exprs = shared.map(get-value)

      anf-name-rec(exprs, "anf_shared", lam(ts):
          new-shared = for map2(f from shared, t from ts):
              N.a-field(f.l, f.name, t)
            end
          anf-variants(variants, lam(new-variants):
              k.apply(l, N.a-data-expr(l, data-name, data-name-t, new-variants, new-shared))
            end)
        end, scope)

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
                  lam(test): k.apply(l, N.a-if(l, test, anf-term(f.body, scope), anf-term(_else, scope))) end,
                  scope)
              | link(f2, r2) =>
                anf-name(
                  f.test,
                  "anf_if",
                  lam(test):
                    k.apply(l, N.a-if(l, test, anf-term(f.body, scope),
                        anf-if-branches(k-cont(lam(if-expr): N.a-lettable(l, if-expr) end), r)))
                  end,
                  scope)
            end
        end
      end
      anf-if-branches(k, branches)
    | s-cases-else(l, typ, val, branches, _else) =>
      anf-name(val, "cases_val",
        lam(v): k.apply(l, N.a-cases(l, typ, v, branches.map(anf-cases-branch(_, scope)), anf-term(_else, scope))) end,
        scope)
    | s-block(l, stmts) => anf-block(stmts, k, scope)
    | s-user-block(l, body) => anf(body, k, scope)

    | s-check-expr(l, expr, ann) =>
      name = mk-id(l, "ann_check_temp")
      bindings = [list: A.s-let-bind(l, A.s-bind(l, false, name.id, ann), expr)]
      anf(A.s-let-expr(l, bindings, A.s-id(l, name.id)), k, scope)

    | s-lam(l, params, args, ret, doc, body, _) =>
      new-scope = scope.activate(e)
      if A.is-a-blank(ret) or A.is-a-any(ret):
        k.apply(l, N.a-lam(l, args.map(lam(a): N.a-bind(a.l, a.id, a.ann) end), ret, anf-term(body, new-scope)))
      else:
        name = mk-id(l, "ann_check_temp")
        k.apply(l, N.a-lam(l, args.map(lam(a): N.a-bind(a.l, a.id, a.ann) end), ret,
            anf-term(A.s-let-expr(l,
                [list: A.s-let-bind(l, A.s-bind(l, false, name.id, ret), body)],
                A.s-id(l, name.id)), new-scope)))
      end
    | s-method(l, params, args, ret, doc, body, _) =>
      if A.is-a-blank(ret) or A.is-a-any(ret):
        k.apply(l, N.a-method(l, args.map(lam(a): N.a-bind(a.l, a.id, a.ann) end), ret, anf-term(body, scope)))
      else:
        name = mk-id(l, "ann_check_temp")
        k.apply(l, N.a-method(l, args.map(lam(a): N.a-bind(a.l, a.id, a.ann) end), ret,
            anf-term(A.s-let-expr(l,
                [list: A.s-let-bind(l, A.s-bind(l, false, name.id, ret), body)],
                A.s-id(l, name.id)), scope)))
      end
    | s-array(l, values) =>
      anf-name-rec(values, "anf_array_val", lam(vs):
        k.apply(l, N.a-array(l, vs))
      end, scope)

    | s-app(l, f, args) =>
      cases(A.Expr) f:
        | s-dot(l2, obj, m) =>
          anf-name(obj, "anf_method_obj", lam(v):
            anf-name-rec(args, "anf_arg", lam(vs):
              k.apply(l, N.a-method-app(l, v, m, vs))
            end, scope)
          end, scope)
        | else =>
          is-recursive = A.is-s-id-letrec(f) and scope.is-matching(f.id)
          anf-name(f, "anf_fun", lam(v):
              anf-name-rec(args, "anf_arg", lam(vs):
                  k.apply(l, N.a-app(l, v, vs, is-recursive))
                end, scope)
            end, scope)
      end

    | s-prim-app(l, f, args) =>
      anf-name-rec(args, "anf_arg", lam(vs):
          k.apply(l, N.a-prim-app(l, f, vs))
        end, scope)

    | s-instantiate(_, body, _) =>
      anf(body, k, scope)

    | s-dot(l, obj, field) =>
      anf-name(obj, "anf_bracket", lam(t-obj): k.apply(l, N.a-dot(l, t-obj, field)) end, scope)

    | s-bracket(l, obj, field) =>
      fname = cases(A.Expr) field:
          | s-str(_, s) => s
          | else => raise("Non-string field: " + torepr(field))
        end
      anf-name(obj, "anf_bracket", lam(t-obj): k.apply(l, N.a-dot(l, t-obj, fname)) end, scope)

    | s-ref(l, ann) =>
      k.apply(l, N.a-ref(l, ann))

    | s-get-bang(l, obj, field) =>
      anf-name(obj, "anf_get_bang", lam(t): k.apply(l, N.a-get-bang(l, t, field)) end, scope)

    | s-assign(l, id, value) =>
      anf-name(value, "anf_assign", lam(v): k.apply(l, N.a-assign(l, id, v)) end, scope)

    | s-obj(l, fields) =>
      exprs = fields.map(get-value)

      anf-name-rec(exprs, "anf_obj", lam(ts):
          new-fields = for map2(f from fields, t from ts):
              N.a-field(f.l, f.name, t)
            end
          k.apply(l, N.a-obj(l, new-fields))
        end, scope)

    | s-update(l, obj, fields) =>
      exprs = fields.map(get-value)

      anf-name(obj, "anf_update", lam(o):
          anf-name-rec(exprs, "anf_update", lam(ts):
              new-fields = for map2(f from fields, t from ts):
                  N.a-field(f.l, f.name, t)
                end
              k.apply(l, N.a-update(l, o, new-fields))
            end, scope)
        end, scope)

    | s-extend(l, obj, fields) =>
      exprs = fields.map(get-value)

      anf-name(obj, "anf_extend", lam(o):
          anf-name-rec(exprs, "anf_extend", lam(ts):
              new-fields = for map2(f from fields, t from ts):
                  N.a-field(f.l, f.name, t)
                end
              k.apply(l, N.a-extend(l, o, new-fields))
            end, scope)
        end, scope)

    | s-let(_, _, _) => raise("s-let should be handled by anf-block: " + torepr(e))
    | s-var(_, _, _) => raise("s-var should be handled by anf-block: " + torepr(e))
    | else => raise("Missed case in anf: " + torepr(e))
  end
end
