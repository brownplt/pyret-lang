#lang pyret

provide:
  checkers,
  append-nothing-if-necessary,
  wrap-toplevels,
  inline-lams,
  set-recursive,
  set-tail-position,
  set-safe-letrec-binds,
  strip-annotations,
  wrap-extra-imports,
  import-to-dep,
  ann-to-typ,
  get-named-provides,
  canonicalize-provides,
  localize-provides,
  get-typed-provides
end
provide-types *
import file("ast.arr") as A
import file("compile-structs.arr") as CS
import file("type-structs.arr") as T
import file("type-check-structs.arr") as TCS
import string-dict as SD
import js-file("ts-ast-util") as TAU

type URI = String

checkers = TAU.checkers

append-nothing-if-necessary = TAU.append-nothing-if-necessary

wrap-toplevels = TAU.wrap-toplevels

inline-lams = TAU.inline-lams

set-recursive = TAU.set-recursive

set-tail-position = TAU.set-tail-position

set-safe-letrec-binds = TAU.set-safe-letrec-binds

strip-annotations = TAU.strip-annotations

wrap-extra-imports = TAU.wrap-extra-imports

import-to-dep = TAU.import-to-dep

is-s-data-expr = A.is-s-data-expr

is-t-name = T.is-t-name
type NameChanger = (T.Type%(is-t-name) -> T.Type)


# not exported
fun collect-shared-fields(uri :: URI, compile-env :: CS.CompileEnvironment, vs :: List<A.Variant>) -> SD.StringDict<T.Type>:
  if is-empty(vs):
    [SD.string-dict: ]
  else:
    init-members = members-to-t-members(uri, compile-env, vs.first.with-members)
    vs.rest.foldl(lam(v, shared-members):
      v.with-members.foldl(lam(m, shadow shared-members):
        if shared-members.has-key(m.name):
          existing-mem-type = shared-members.get-value(m.name)
          this-mem-type = member-to-t-member(uri, compile-env, m)
          if existing-mem-type == this-mem-type:
            shared-members
          else:
            shared-members.remove(m.name)
          end
        else:
          shared-members
        end
      end, shared-members)
    end, init-members)
  end
end

# not exported
fun v-members-to-t-members(uri :: URI, compile-env :: CS.CompileEnvironment, ms):
  ms.foldr(lam(m, members):
    cases(A.VariantMember) m:
      | s-variant-member(l, kind, bind) =>
        typ = if A.is-s-mutable(kind):
          T.t-ref(ann-to-typ(uri, compile-env, bind.ann), l, false)
        else:
          ann-to-typ(uri, compile-env, bind.ann)
        end
        link({bind.id.toname(); typ}, members)
    end
  end, empty)
end

# not exported
fun member-to-t-member(uri :: URI, compile-env :: CS.CompileEnvironment, m):
  cases(A.Member) m:
    | s-data-field(l, name, val) =>
      T.t-top(l, false)
    | s-mutable-field(l, name, ann, val) =>
      T.t-ref(ann-to-typ(uri, compile-env, ann), false)
    | s-method-field(l, name, params, args, ann, _, _, _, _, _) =>
      shadow args = map(
        lam(arg-ann):
          ann-to-typ(uri, compile-env, arg-ann)
        end,
        map(_.ann, args)
      )
      shadow ret = ann-to-typ(uri, compile-env, ann)
      arrow-part =
        T.t-arrow(args, ret, l, false)
      if is-empty(params): arrow-part
      else:
        tvars = for map(p from params): T.t-var(p, l, false) end
        T.t-forall(tvars, arrow-part, l, false)
      end
  end
end

# not exported
fun members-to-t-members(uri :: URI, compile-env :: CS.CompileEnvironment, ms):
  ms.foldl(lam(m, members):
    cases(A.Member) m:
      | s-data-field(l, name, val) =>
        members.set(name, member-to-t-member(uri, compile-env, m))
      | s-mutable-field(l, name, ann, val) =>
        members.set(name, member-to-t-member(uri, compile-env, m))
      | s-method-field(l, name, params, args, ann, _, _, _, _, _) =>
        members.set(name, member-to-t-member(uri, compile-env, m))
    end
  end, [SD.string-dict: ])
end

# exported
# TODO(MATT): a-blank should have location
fun ann-to-typ(uri :: URI, compile-env :: CS.CompileEnvironment, a :: A.Ann) -> T.Type:
  fun mappy-ann-to-typ(to-map :: A.Ann) -> T.Type:
    ann-to-typ(uri, compile-env, to-map)
  end
  cases(A.Ann) a:
    | a-blank => T.t-top(A.dummy-loc, false)
    | a-any(l) => T.t-top(l, false)
    | a-name(l, id) =>
      cases(A.Name) id:
        | s-type-global(name) =>
          cases(Option<String>) compile-env.globals.types.get(name):
            | none =>
              raise("Name not found in globals.types: " + name)
            | some(mod-uri) =>
              T.t-name(T.module-uri(mod-uri.uri-of-definition), id, l, false)
          end
        | s-atom(_, _) => T.t-name(T.module-uri(uri), id, l, false)
        | else => raise("Bad name found in ann-to-typ: " + id.key())
      end
    | a-type-var(l, id) =>
      T.t-var(id, l, false)
    | a-arrow(l, args, ret, use-parens) =>
      T.t-arrow(map(mappy-ann-to-typ, args), ann-to-typ(uri, compile-env, ret), l, false)
    | a-arrow-argnames(l, args, ret, use-parens) =>
      T.t-arrow(map({(arg): ann-to-typ(uri, compile-env, arg.ann)}, args), ann-to-typ(uri, compile-env, ret), l, false)
    | a-method(l, args, ret) =>
      raise("Cannot provide a raw method")
    | a-record(l, fields) =>
      T.t-record(fields.foldl(lam(f, members):
        members.set(f.name, ann-to-typ(uri, compile-env, f.ann))
      end, [SD.string-dict: ]), l, false)
    | a-tuple(l, fields) =>
      T.t-tuple(map(mappy-ann-to-typ, fields), l, false)
    | a-app(l, ann, args) =>
      T.t-app(ann-to-typ(uri, compile-env, ann), map(mappy-ann-to-typ, args), l, false)
    | a-pred(l, ann, exp) =>
      # TODO(joe): give more info than this to type checker?  only needed dynamically, right?
      ann-to-typ(uri, compile-env, ann)
    | a-dot(l, obj, field) =>
      # TODO(joe): maybe-b = resolved.module-bindings.get-now(obj.key())
      # Then use the information to provide the right a-dot type by looking
      # it up on the module.
      T.t-top(l, false)
    | a-checked(checked, residual) =>
      raise("a-checked should only be generated by the type-checker")
  end
end
# not exported
fun data-expr-to-datatype(uri :: URI, compile-env :: CS.CompileEnvironment, exp :: A.Expr % (is-s-data-expr)) -> T.DataType:
  cases(A.Expr) exp:
    | s-data-expr(l, name, _, params, _, variants, shared-members, _, _) =>

      tvars = for map(tvar from params):
        T.t-var(tvar, l, false)
      end

      tvariants = for map(tv from variants):
        cases(A.Variant) tv:
          | s-variant(l2, constr-loc, vname, members, with-members) =>
            T.t-variant(
              vname,
              v-members-to-t-members(uri, compile-env, members),
              members-to-t-members(uri, compile-env, with-members),
              l2)
          | s-singleton-variant(l2, vname, with-members) =>
            T.t-singleton-variant(
              vname,
              members-to-t-members(uri, compile-env, with-members),
              l2)
        end
      end

      shared-across-variants = collect-shared-fields(uri, compile-env, variants)
      shared-fields = members-to-t-members(uri, compile-env, shared-members)
      all-shared-fields = shared-across-variants.fold-keys(lam(key, all-shared-fields):
        if shared-fields.has-key(key):
          all-shared-fields
        else:
          all-shared-fields.set(key, shared-across-variants.get-value(key))
        end
      end, shared-fields)

      T.t-data(
        name,
        tvars,
        tvariants,
        all-shared-fields,
        l)
  end
end

# exported
fun get-named-provides(resolved :: CS.NameResolution, uri :: URI, compile-env :: CS.CompileEnvironment) -> CS.Provides:
  cases(A.Program) resolved.ast:
    | s-program(_, _ , _, _, provide-blocks, _, _) =>
      cases(A.ProvideBlock) provide-blocks.first block:
          # NOTE(joe): assume the provide block is resolved
        | s-provide-block(_, _, provide-specs) =>
          mp-specs = provide-specs.filter(A.is-s-provide-module)
          mod-provides = for fold(mp from [SD.string-dict:], m from mp-specs):
            cases(A.NameSpec) m.name-spec:
              | s-remote-ref(l, shadow uri, name, as-name) =>
                mod-info = compile-env.provides-by-uri-value(uri)
                mod-uri = mod-info.modules.get-value(name.toname())
                mp.set(as-name.toname(), mod-uri)
              | s-local-ref(l, name, as-name) =>
                mb = resolved.env.module-bindings.get-value-now(name.key())
                mp.set(as-name.toname(), mb.uri)
              | else => raise("All provides should be resolved to local or remote refs")
            end
          end

          vp-specs = provide-specs.filter(A.is-s-provide-name)
          val-provides = for fold(vp from [SD.string-dict:], v from vp-specs):
            cases(A.NameSpec) v.name-spec:
              | s-remote-ref(l, shadow uri, name, as-name) =>
                origin-name = name.toname()
                val-export = compile-env.value-by-uri-value(uri, origin-name)
                origin = val-export.origin
                corrected-origin = CS.bind-origin(
                  as-name.l,
                  origin.definition-bind-site,
                  origin.new-definition,
                  origin.uri-of-definition,
                  origin.original-name)
                vp.set(as-name.toname(), CS.v-alias(corrected-origin, origin-name))
              | s-local-ref(l, name, as-name) =>
                vb = resolved.env.bindings.get-value-now(name.key())
                # NOTE(joe/ben): The as-name below has important semantic meaning.
                # This makes it so if you provide the same definition with
                # multiple names, each gets a separate identity from the POV of
                # the module system.

                # This is _different_ from (rename-out) in Racket, a closely
                # related feature. In Racket,
                #
                # (provide x (rename-out (x y))) (define x 10)
                #
                # is different from
                #
                # (provide x y) (define x 10) (define y x)
                #
                # In Pyret,
                # provide: x, x as y end
                # x = 10
                #
                # is (from the module system's POV) the same as
                #
                # provide: x, y end
                # x = 10
                # y = x
                corrected-origin = CS.bind-origin(
                  as-name.l,
                  vb.origin.definition-bind-site,
                  vb.origin.new-definition,
                  vb.origin.uri-of-definition,
                  as-name)
                provided-value = cases(CS.ValueBinder) vb.binder:
                  | vb-var => CS.v-var(corrected-origin, ann-to-typ(uri, compile-env, vb.ann))
                  | else => CS.v-just-type(corrected-origin, ann-to-typ(uri, compile-env, vb.ann))
                end
                vp.set(as-name.toname(), provided-value)
            end
          end
          #|
          spy "get-named-provides":
            vp-specs,
            val-provides
          end
          |#

          tp-specs = provide-specs.filter(A.is-s-provide-type)
          typ-provides = for fold(tp from [SD.string-dict:], t from tp-specs):
            cases(A.NameSpec) t.name-spec:
              | s-remote-ref(l, shadow uri, name, as-name) =>
                remote-typ = compile-env.type-by-uri-value(uri, name.toname())
                tp.set(as-name.toname(), remote-typ)
              | s-local-ref(l, name, as-name) =>
                tb = resolved.env.type-bindings.get-value-now(name.key())
                typ = cases(CS.TypeBindTyp) tb.typ:
                  | tb-none => T.t-top(l, false)
                  | tb-typ(typ) => typ
                end
                tp.set(as-name.toname(), typ)
            end
          end

          dp-specs = provide-specs.filter(A.is-s-provide-data)
          data-provides = for fold(dp from [SD.string-dict:], d from dp-specs):
            cases(A.NameSpec) d.name-spec:
              | s-remote-ref(l, shadow uri, name, as-name) =>
                origin-name = name.toname()
                data-export = compile-env.datatype-by-uri-value(uri, origin-name)
                origin = data-export.origin
                corrected-origin = CS.bind-origin(
                  as-name.l,
                  origin.definition-bind-site,
                  false, # NOTE(joe/ben): This seems like it ought to be false,
                         # but writing a test where that matters isn't really
                         # doable, so it could also be origin.new-definition
                         # without changing behavior
                  origin.uri-of-definition,
                  origin.original-name)
                dp.set(as-name.toname(), CS.d-alias(corrected-origin, origin-name))

                # TODO(joe): do remote lookup here to get a better location than SL.builtin for the origin
                # dp.set(as-name.toname(), CS.d-alias(CS.bind-origin(l, SL.builtin(uri), false, uri, name), name.toname()))
              | s-local-ref(l, name, as-name) =>
                exp = resolved.env.datatypes.get-value-now(name.toname())
                dp.set(as-name.toname(), CS.d-type(CS.bind-origin(l, exp.l, true, uri, name), data-expr-to-datatype(uri, compile-env, exp)))
            end
          end
          provs = CS.provides(
              uri,
              mod-provides,
              val-provides,
              typ-provides,
              data-provides)
          provs
      end
  end
end

# not exported
fun canonicalize-members(ms :: T.TypeMembers, uri :: URI, tn :: NameChanger) -> T.TypeMembers:
  T.type-member-map(ms, lam(_, typ): canonicalize-names(typ, uri, tn) end)
end

# not exported
fun canonicalize-fields(ms :: List<{String; T.Type}>, uri :: URI, tn :: NameChanger) -> List<{String; T.Type}>:
  ms.map(lam({name; typ}): {name; canonicalize-names(typ, uri, tn)} end)
end

# not exported
fun canonicalize-variant(v :: T.TypeVariant, uri :: URI, tn :: NameChanger) -> T.TypeVariant:
  c = canonicalize-members(_, uri, tn)
  cases(T.TypeVariant) v:
    | t-variant(name, fields, with-fields, l) =>
      T.t-variant(name, canonicalize-fields(fields, uri, tn), c(with-fields), l)
    | t-singleton-variant(name, with-fields, l) =>
      T.t-singleton-variant(name, c(with-fields), l)
  end
end

# not exported
fun canonicalize-data-export(de :: CS.DataExport, uri :: URI, tn :: NameChanger) -> CS.DataExport:
  cases(CS.DataExport) de:
    | d-alias(origin, name) => de
    | d-type(origin, typ) => CS.d-type(origin, canonicalize-data-type(typ, uri, tn))
  end
end

# not exported
fun canonicalize-data-type(dtyp :: T.DataType, uri :: URI, tn :: NameChanger) -> T.DataType:
  cases(T.DataType) dtyp:
    | t-data(name, params, variants, fields, l) =>
      T.t-data(
          name,
          params,
          map(canonicalize-variant(_, uri, tn), variants),
          canonicalize-members(fields, uri, tn),
          l)
  end
end

# not exported
# TODO(MATT): add all the correct cases to this
fun canonicalize-names(typ :: T.Type, uri :: URI, transform-name :: NameChanger) -> T.Type:
  c = canonicalize-names(_, uri, transform-name)
  cases(T.Type) typ:
    | t-name(module-name, id, l, _) => transform-name(typ)
    | t-var(id, l, _) => typ
    | t-arrow(args, ret, l, inferred) => T.t-arrow(map(c, args), c(ret), l, inferred)
    | t-tuple(elts, l, inferred) => T.t-tuple(map(c, elts), l, inferred)
    | t-app(onto, args, l, inferred) => T.t-app(c(onto), map(c, args), l, inferred)
    | t-top(l, inferred) => T.t-top(l, inferred)
    | t-bot(l, inferred) => T.t-bot(l, inferred)
    | t-record(fields, l, inferred) =>
      T.t-record(canonicalize-members(fields, uri, transform-name), l, inferred)
    | t-forall(introduces, onto, l, inferred) => T.t-forall(map(c, introduces), c(onto), l, inferred)
    | t-ref(t, l, inferred) => T.t-ref(c(t), l, inferred)
    | t-data-refinement(data-type, variant-name, l, inferred) =>
      T.t-data-refinement(c(data-type), variant-name, l, inferred)
    | t-existential(id, l, _) => raise("Internal error: Cannot canonicalize free existential types " + to-repr(typ))
  end
end

# not exported
fun canonicalize-value-export(ve :: CS.ValueExport, uri :: URI, tn):
  cases(CS.ValueExport) ve:
    | v-alias(o, n) => CS.v-alias(o, n)
    | v-just-type(o, t) => CS.v-just-type(o, canonicalize-names(t, uri, tn))
    | v-var(o, t) => CS.v-var(o, canonicalize-names(t, uri, tn))
    | v-fun(o, t, name, flatness) => CS.v-fun(o, canonicalize-names(t, uri, tn), name, flatness)
  end
end

# not exported
fun transform-dict-helper(canonicalizer):
  lam(d, uri, transformer):
    for SD.fold-keys(s from [SD.string-dict: ], v from d):
      s.set(v, canonicalizer(d.get-value(v), uri, transformer))
    end
  end
end

# not exported
transform-value-dict = transform-dict-helper(canonicalize-value-export)
transform-dict = transform-dict-helper(canonicalize-names)
transform-data-dict = transform-dict-helper(canonicalize-data-export)

# not exported
fun transform-provides(provides, compile-env, transformer):
  cases(CS.Provides) provides:
  | provides(from-uri, modules, values, aliases, data-definitions) =>
    new-vals = transform-value-dict(values, from-uri, transformer)
    new-aliases = transform-dict(aliases, from-uri, transformer)
    new-data-definitions = transform-data-dict(data-definitions, from-uri, transformer)
    CS.provides(from-uri, modules, new-vals, new-aliases, new-data-definitions)
  end
end

# exported
fun canonicalize-provides(provides :: CS.Provides, compile-env :: CS.CompileEnvironment):
  doc: ```
  Produces a new provides data structure that has no `dependency` NameOrigins
  in the types, by looking up dependencies in the compile environment.
  Also produces an error if there is a module URI or dependency that is not
  mentioned in the compile-env.
  ```
  transformer = lam(t :: T.Type%(is-t-name)):
    cases(T.Type) t:
    | t-name(origin, name, loc, inferred) =>
      cases(T.NameOrigin) origin:
      | local => T.t-name(T.module-uri(provides.from-uri), name, loc, inferred)
      | module-uri(uri) => t
      | dependency(d) =>
        provides-for-d = compile-env.provides-by-dep-key(d)
        cases(Option<CS.Provides>) provides-for-d:
        | some(p) => T.t-name(T.module-uri(p.from-uri), name, loc, inferred)
        | none => raise("Unknown module dependency for type: " + torepr(t) + " in provides for " + provides.from-uri)
        end
      end
    end
  end
  transform-provides(provides, compile-env, transformer)
end

# exported
fun localize-provides(provides :: CS.Provides, compile-env :: CS.CompileEnvironment):
  doc: ```
  Produces a new provides data structure that has no `module-uri` NameOrigins
  in the types, by looking up uris in the compile environment, or using `local`.

  Also produces an error if there is a module URI or dependency that is not
  mentioned in the compile-env.
  ```
  transformer = lam(t :: T.Type%(is-t-name)):
    cases(T.Type) t:
    | t-name(origin, name, loc, inferred) =>
      cases(T.NameOrigin) origin:
      | local => t
      | module-uri(uri) =>
        if uri == provides.from-uri:
          T.t-name(T.local, name, loc, inferred)
        else:
          t
        end
      | dependency(d) =>
        provides-for-d = compile-env.my-modules.get(d)
        cases(Option<CS.Provides>) provides-for-d:
        | some(p) => t
        | none => raise("Unknown module dependency for type: " + torepr(t) + " in provides for " + provides.from-uri)
        end
      end
    end
  end
  transform-provides(provides, compile-env, transformer)
end

# exported
# TODO(MATT): this does not actually get the provided module values
fun get-typed-provides(resolved, typed :: TCS.Typed, uri :: URI, compile-env :: CS.CompileEnvironment):
  transformer = lam(t):
    cases(T.Type) t:
      | t-name(origin, name, l, inferred) =>
        T.t-name(origin, A.s-type-global(name.toname()), l, inferred)
      | else => t
    end
  end
  c = canonicalize-names(_, uri, transformer)
  cases(A.Program) typed.ast block:
    | s-program(_, _, _, _, provide-blocks, _, _) =>
      cases(A.ProvideBlock) provide-blocks.first block:
        | s-provide-block(_, _, provide-specs) =>

          mp-specs = provide-specs.filter(A.is-s-provide-module)
          mod-provides = for fold(mp from [SD.string-dict:], m from mp-specs):
            cases(A.NameSpec) m.name-spec:
              | s-remote-ref(l, shadow uri, name, as-name) =>
                mod-info = compile-env.provides-by-uri-value(uri)
                mod-uri = mod-info.modules.get-value(name.toname())
                mp.set(as-name.toname(), mod-uri)
              | s-local-ref(l, name, as-name) =>
                mb = resolved.env.module-bindings.get-value-now(name.key())
                mp.set(as-name.toname(), mb.uri)
              | else => raise("All provides should be resolved to local or remote refs")
            end
          end

          vp-specs = provide-specs.filter(A.is-s-provide-name)
          val-provides = for fold(vp from [SD.string-dict:], v from vp-specs):
            cases(A.NameSpec) v.name-spec:
              | s-remote-ref(l, shadow uri, name, as-name) =>
                origin-name = name.toname()
                val-export = compile-env.value-by-uri-value(uri, origin-name)
                vp.set(as-name.toname(), CS.v-alias(val-export.origin, origin-name))
              | s-local-ref(l, name, as-name) =>
                tc-typ = typed.info.types.get-value(name.key())
                vb = resolved.env.bindings.get-value-now(name.key())
                corrected-origin = CS.bind-origin(
                  as-name.l,
                  vb.origin.definition-bind-site,
                  vb.origin.new-definition,
                  vb.origin.uri-of-definition,
                  as-name)
                # TODO(joe): Still have v-var questions here
                vp.set(as-name.toname(), CS.v-just-type(corrected-origin, c(tc-typ)))
            end
          end

          tp-specs = provide-specs.filter(A.is-s-provide-type)
          typ-provides = for fold(tp from [SD.string-dict:], t from tp-specs):
            cases(A.NameSpec) t.name-spec:
              | s-remote-ref(l, shadow uri, name, as-name) =>
                remote-typ = compile-env.type-by-uri-value(uri, name.toname())
                tp.set(as-name.toname(), remote-typ)
              | s-local-ref(l, name, as-name) =>
                key = name.key()
                cases(Option) typed.info.data-types.get(key):
                  | some(typ) => tp.set(name, c(typ))
                  | none =>
                    typ = typed.info.aliases.get-value(key)
                    tp.set(as-name.toname(), c(typ))
                end
            end

          end

          dp-specs = provide-specs.filter(A.is-s-provide-data)
          data-provides = for fold(dp from [SD.string-dict:], d from dp-specs):
            cases(A.NameSpec) d.name-spec:
              | s-remote-ref(l, shadow uri, name, as-name) =>
                origin-name = name.toname()
                data-export = compile-env.datatype-by-uri-value(uri, origin-name)
                origin = data-export.origin
                dp.set(as-name.toname(), CS.d-alias(origin, origin-name))
              | s-local-ref(l, name, as-name) =>
                exp = resolved.env.datatypes.get-value-now(name.toname())
                origin = CS.bind-origin(l, exp.l, true, uri, name)
                dp.set(as-name.toname(), CS.d-type(origin, canonicalize-data-type(typed.info.data-types.get-value(exp.namet.key()), uri, transformer)))

            end
          end
          provs = CS.provides(
              uri,
              mod-provides,
              val-provides,
              typ-provides,
              data-provides)
          provs
      end
  end
end
