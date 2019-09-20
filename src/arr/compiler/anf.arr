#lang pyret

provide *
provide-types *

import ast as A
import srcloc as SL
import file("ast-anf.arr") as N
import string-dict as SD

type Loc = SL.Srcloc

type ANFCont = (N.ALettable -> N.AExpr)


fun get-value(o): o.value end

names = A.global-names
flat-prim-app = A.prim-app-info-c(false)

fun mk-id(loc, base):
  t = names.make-atom(base)
  { id: t, id-b: bind(loc, t), id-e: N.a-id(loc, t) }
end

fun anf-term(e :: A.Expr) -> N.AExpr:
  anf(e, lam(x): N.a-lettable(x.l, x) end)
end

fun bind(l, id): N.a-bind(l, id, A.a-blank) end

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

fun anf-cases-branch(branch):
  cases(A.CasesBranch) branch:
    | s-cases-branch(l, pat-loc, name, args, body) =>
      N.a-cases-branch(l, pat-loc, name, args.map(anf-cases-bind), anf-term(body))
    | s-singleton-cases-branch(l, pat-loc, name, body) =>
      N.a-singleton-cases-branch(l, pat-loc, name, anf-term(body))
  end
end

fun anf-name(expr :: A.Expr, name-hint :: String, k :: (N.AVal -> N.AExpr)) -> N.AExpr:
  anf(expr, lam(lettable):
      cases(N.ALettable) lettable:
        | a-val(_, v) => k(v)
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
    | empty => k([list: ])
    | link(f, r) =>
      anf-name(f, name-hint, lam(v):
          anf-name-rec(r, name-hint, lam(vs): k([list: v] + vs) end)
        end)
  end
end

fun anf-name-arr(expr :: A.Expr, name :: A.Name, idx :: Number, k :: ( -> N.AExpr)) -> N.AExpr:
  anf(expr, lam(lettable):
      N.a-arr-let(expr.l, N.a-bind(expr.l, name, A.a-blank), idx, lettable, k())
    end)
end

fun anf-name-arr-rec(
    exprs :: List<A.Expr>,
    name :: A.Name,
    ind :: Number,
    k :: ( -> N.AExpr)
  ) -> N.AExpr:
  cases(List) exprs:
    | empty => k()
    | link(f, r) =>
      anf-name-arr(f, name, ind, lam():
          anf-name-arr-rec(r, name, ind + 1, k)
        end)
  end
end

fun anf-program(e :: A.Program):
  cases(A.Program) e:
    | s-program(l, p, _, imports, block) =>
      # Note: provides have been desugared to a structure with no expressions, just
      # names and Ann information
      N.a-program(l, p, imports.map(anf-import), anf-term(block))
  end
end

fun anf-import-type(it :: A.ImportType):
  cases(A.ImportType) it:
    | s-const-import(l, mod) => N.a-import-builtin(l, mod)
    | s-special-import(l, kind, args) => N.a-import-special(l, kind, args)
  end
end

fun anf-import(i :: A.Import):
  cases(A.Import) i:
    | s-import-complete(l, vals, types, f, val-name, types-name) =>
      itype = cases(A.ImportType) f:
        | s-const-import(_, mod) => N.a-import-builtin(l, mod)
        | s-special-import(_, kind, args) => N.a-import-special(l, kind, args)
      end
      N.a-import-complete(l, vals, types, itype, val-name, types-name)
  end
end

fun anf-block(es-init :: List<A.Expr>, k :: ANFCont):
  fun anf-block-help(es):
    cases (List<A.Expr>) es:
      | empty => raise("Empty block")
      | link(f, r) =>
        # Note: assuming blocks don't end in let/var here
        if is-empty(r):
          anf(f, k)
        else:
          cases(A.Expr) f:
            | else => anf(f, lam(lettable):
                  N.a-seq(f.l, lettable, anf-block-help(r))
                end)
          end
        end
    end
  end
  anf-block-help(es-init)
end

fun anf(e :: A.Expr, k :: ANFCont) -> N.AExpr:
  cases(A.Expr) e:
    | s-module(l, answer, dvs, dts, provides, types, checks) =>
      adts = for map(dt from dts):
        N.a-defined-type(dt.name, dt.typ)
      end
      needs-value = dvs.filter(A.is-s-defined-value)
      anf-name-rec(needs-value.map(_.value), "defined_value", lam(advs):
        shadow advs = for map2(name from needs-value.map(_.name), adv from advs):
          N.a-defined-value(name, adv)
        end
        avars = dvs.filter(A.is-s-defined-var).map(lam(dvar):
          N.a-defined-var(dvar.name, dvar.id)
        end)

        anf-name(answer, "answer", lam(ans):
            anf-name(provides, "provides", lam(provs):
                anf-name(checks, "checks", lam(chks):
                    k(N.a-module(l, ans, advs + avars, adts, provs, types, chks))
                  end)
              end)
          end)
        
      end)
    | s-num(l, n, u) =>
        k(N.a-val(l, N.a-num(l, n, u)))
      # num, den are exact ints, and s-frac desugars to the exact rational num/den
    | s-frac(l, num, den, u) =>
      k(N.a-val(l, N.a-num(l, num / den, u))) # Possibly unneeded if removed by desugar?
      # num, den are exact ints, and s-rfrac desugars to the roughnum fraction corresponding to num/den
    | s-rfrac(l, num, den, u) =>
      k(N.a-val(l, N.a-num(l, num-to-roughnum(num / den), u))) # Possibly unneeded if removed by desugar?
    | s-str(l, s) => k(N.a-val(l, N.a-str(l, s)))
    | s-undefined(l) => k(N.a-val(l, N.a-undefined(l)))
    | s-bool(l, b) => k(N.a-val(l, N.a-bool(l, b)))
    | s-id(l, id) => k(N.a-val(l, N.a-id(l, id)))
    | s-srcloc(l, loc) => k(N.a-val(l, N.a-srcloc(l, loc)))
    | s-type-let-expr(l, binds, body, blocky) =>
      cases(List) binds:
        | empty => anf(body, k)
        | link(f, r) =>
          new-bind = cases(A.TypeLetBind) f:
            | s-type-bind(l2, name, params, ann) =>
              N.a-type-bind(l2, name, ann) # TODO(MATT): is this going to have to change?
            | s-newtype-bind(l2, name, namet) =>
              N.a-newtype-bind(l2, name, namet)
          end
          N.a-type-let(l, new-bind, anf(A.s-type-let-expr(l, r, body, blocky), k))
      end
    | s-let-expr(l, binds, body, blocky) =>
      cases(List) binds:
        | empty => anf(body, k)
        | link(f, r) =>
          cases(A.LetBind) f:
            | s-var-bind(l2, b, val) =>
              if A.is-a-blank(b.ann) or A.is-a-any(b.ann):
                anf-name(val, "var", lam(new-val):
                      N.a-var(l2, N.a-bind(l2, b.id, b.ann), N.a-val(new-val.l, new-val),
                        anf(A.s-let-expr(l, r, body, blocky), k))
                    end)
              else:
                var-name = mk-id(l2, "var")
                anf(val, lam(lettable):
                    N.a-let(l2, var-name.id-b, lettable,
                      N.a-var(l2, N.a-bind(l2, b.id, b.ann), N.a-val(l2, var-name.id-e),
                        anf(A.s-let-expr(l, r, body, blocky), k)))
                  end)
              end
            | s-let-bind(l2, b, val) => anf(val, lam(lettable):
                  N.a-let(l2, N.a-bind(l2, b.id, b.ann), lettable,
                    anf(A.s-let-expr(l, r, body, blocky), k))
                end)
          end
      end

    | s-letrec(l, binds, body, _) =>
      let-binds = for map(b from binds):
        A.s-var-bind(b.l, b.b, A.s-undefined(l))
      end
      assigns = for map(b from binds):
        A.s-assign(b.l, b.b.id, b.value)
      end
      anf(A.s-let-expr(l, let-binds, A.s-block(l, assigns + [list: body]), true), k)

    | s-data-expr(l, data-name, data-name-t, params, mixins, variants, shared, _check-loc, _check) =>
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
              end)
          | s-singleton-variant(l2, vname, with-members) =>
            with-exprs = with-members.map(get-value)
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
            anf-variant(f, lam(v): anf-variants(r, lam(rest-vs): ks([list: v] + rest-vs) end) end)
        end
      end
      exprs = shared.map(get-value)

      anf-name-rec(exprs, "anf_shared", lam(ts):
          new-shared = for map2(f from shared, t from ts):
              N.a-field(f.l, f.name, t)
            end
          anf-variants(variants, lam(new-variants):
              k(N.a-data-expr(l, data-name, data-name-t, new-variants, new-shared))
            end)
        end)

    | s-if-else(l, branches, _else, _) =>
      fun anf-if-branches(shadow k, shadow branches):
        cases(List) branches:
          | empty => raise("Empty branches")
          | link(f, r) =>
            cases(List) r:
              | empty =>
                anf-name(
                  f.test,
                  "anf_if",
                  lam(test): k(N.a-if(l, test, anf-term(f.body), anf-term(_else))) end
                  )
              | link(f2, r2) =>
                anf-name(
                  f.test,
                  "anf_if",
                  lam(test):
                    k(N.a-if(l, test, anf-term(f.body),
                        anf-if-branches(lam(if-expr): N.a-lettable(l, if-expr) end, r)))
                  end
                  )
            end
        end
      end
      anf-if-branches(k, branches)
    | s-cases-else(l, typ, val, branches, _else, _) =>
      anf-name(val, "cases_val",
        lam(v): k(N.a-cases(l, typ, v, branches.map(anf-cases-branch), anf-term(_else))) end)
    | s-block(l, stmts) => anf-block(stmts, k)

    | s-check-expr(l, expr, ann) =>
      name = mk-id(l, "ann_check_temp")
      bindings = [list: A.s-let-bind(l, A.s-bind(l, false, name.id, ann), expr)]
      anf(A.s-let-expr(l, bindings, A.s-id(l, name.id), false), k)

    | s-lam(l, name, params, args, ret, doc, body, _, _, _) =>
      if A.is-a-blank(ret) or A.is-a-any(ret):
        k(N.a-lam(l, name, args.map(lam(a): N.a-bind(a.l, a.id, a.ann) end), ret, anf-term(body)))
      else:
        temp = mk-id(l, "ann_check_temp")
        k(N.a-lam(l, name, args.map(lam(a): N.a-bind(a.l, a.id, a.ann) end), ret,
            anf-term(A.s-let-expr(l,
                [list: A.s-let-bind(l, A.s-bind(l, false, temp.id, ret), body)],
                A.s-id(l, temp.id), false))))
      end
    | s-method(l, name, params, args, ret, doc, body, _, _, _) =>
      if A.is-a-blank(ret) or A.is-a-any(ret):
        k(N.a-method(l, name, args.map(lam(a): N.a-bind(a.l, a.id, a.ann) end), ret, anf-term(body)))
      else:
        temp = mk-id(l, "ann_check_temp")
        k(N.a-method(l, name, args.map(lam(a): N.a-bind(a.l, a.id, a.ann) end), ret,
            anf-term(A.s-let-expr(l,
                [list: A.s-let-bind(l, A.s-bind(l, false, temp.id, ret), body)],
                A.s-id(l, temp.id), false))))
      end
    | s-tuple(l, fields) =>
      anf-name-rec(fields, "anf_tuple_fields", lam(vs):
       k(N.a-tuple(l, vs))
     end)
    | s-tuple-get(l, tup, index, index-loc) => 
       anf-name(tup, "anf_tuple_get", lam(v): k(N.a-tuple-get(l, v, index)) end)
    | s-array(l, values) =>
      array-id = names.make-atom("anf_array")
      N.a-let(
        l,
        bind(l, array-id),
        N.a-prim-app(l, "makeArrayN", [list: N.a-num(l, values.length(), A.u-one(l))], flat-prim-app),
        anf-name-arr-rec(values, array-id, 0, lam():
          k(N.a-val(l, N.a-id(l, array-id)))
        end))

    | s-app-enriched(l, f, args, app-info) =>
      cases(A.Expr) f:
        | s-dot(l2, obj, m) =>
          anf-name(obj, "anf_method_obj", lam(v):
            anf-name-rec(args, "anf_arg", lam(vs):
              k(N.a-method-app(l, v, m, vs))
            end)
          end)
        | s-lam(f-l, _, _, params, ann, _, body, _, _, blocky) =>
          ### NOTE: This case implements the inline-lams visitor transformation
          ### It can be safely eliminated without affecting the semantics of
          ### the transformation, but does help eliminate some unneeded lambdas
          if (params.length() == args.length()):
            let-binds = for lists.map2(p from params, a from args):
              A.s-let-bind(p.l, p, a)
            end
            inlined = cases(A.Ann) ann:
              | a-blank => A.s-let-expr(l, let-binds, body, blocky)
              | a-any(_) => A.s-let-expr(l, let-binds, body, blocky)
              | else =>
                a = A.global-names.make-atom("inline_body")
                A.s-let-expr(l,
                  let-binds
                    + [list: A.s-let-bind(body.l, A.s-bind(l, false, a, ann), body)],
                  A.s-id(l, a), false)
            end
            anf(inlined, k)
          else:
            anf-name(f, "anf_fun", lam(v):
                anf-name-rec(args, "anf_arg", lam(vs):
                    k(N.a-app(l, v, vs, app-info))
                  end)
              end)
          end
        | else =>
          anf-name(f, "anf_fun", lam(v):
              anf-name-rec(args, "anf_arg", lam(vs):
                  k(N.a-app(l, v, vs, app-info))
                end)
            end)
      end

    | s-prim-app(l, f, args, app-info) =>
      anf-name-rec(args, "anf_arg", lam(vs):
          k(N.a-prim-app(l, f, vs, app-info))
        end)

    | s-instantiate(_, body, _) =>
      anf(body, k)

    | s-dot(l, obj, field) =>
      anf-name(obj, "anf_bracket", lam(t-obj): k(N.a-dot(l, t-obj, field)) end)

    | s-bracket(l, obj, field) =>
      raise("Impossible")

    | s-ref(l, ann) =>
      k(N.a-ref(l, ann))

    | s-id-var(l, id) =>
      k(N.a-id-var(l, id))

    | s-id-letrec(l, id, safe) =>
      if safe:
        k(N.a-val(l, N.a-id-safe-letrec(l, id)))
      else:
        k(N.a-id-letrec(l, id, safe))
      end

    | s-get-bang(l, obj, field) =>
      anf-name(obj, "anf_get_bang", lam(t): k(N.a-get-bang(l, t, field)) end)

    | s-assign(l, id, value) =>
      anf-name(value, "anf_assign", lam(v): k(N.a-assign(l, id, v)) end)

    | s-obj(l, fields) =>
      exprs = fields.map(get-value)

      anf-name-rec(exprs, "anf_obj", lam(ts):
          new-fields = for map2(f from fields, t from ts):
              N.a-field(f.l, f.name, t)
            end
          k(N.a-obj(l, new-fields))
        end)

    | s-update(l, obj, fields) =>
      exprs = fields.map(get-value)

      anf-name(obj, "anf_update", lam(o):
          anf-name-rec(exprs, "anf_update", lam(ts):
              new-fields = for map2(f from fields, t from ts):
                  N.a-field(f.l, f.name, t)
                end
              k(N.a-update(l, o, new-fields))
            end)
        end)

    | s-extend(l, obj, fields) =>
      exprs = fields.map(get-value)

      anf-name(obj, "anf_extend", lam(o):
          anf-name-rec(exprs, "anf_extend", lam(ts):
              new-fields = for map2(f from fields, t from ts):
                  N.a-field(f.l, f.name, t)
                end
              k(N.a-extend(l, o, new-fields))
            end)
        end)

    | s-let(_, _, _) => raise("s-let should have been desugared already: " + torepr(e))
    | s-var(_, _, _) => raise("s-var should have been desugared already: " + torepr(e))
    | s-spy-block(_, _, _) => raise("s-spy-block should have been desugared already: " + torepr(e))
    | s-user-block(l, body) => raise("s-user-block should have been desugared already: " + torepr(e))
    | else => raise("Missed case in anf: " + torepr(e))
  end
end
