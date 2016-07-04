#lang pyret

provide *
provide-types *
import srcloc as SL
import ast as A
import parse-pyret as PP
import file("compile-structs.arr") as CS
import file("ast-anf.arr") as N
import file("type-structs.arr") as T
import file("type-check-structs.arr") as TCS
import string-dict as SD
import either as E
import lists as L

type URI = String
type Loc = SL.Srcloc

type NameOrigin = T.NameOrigin
local = T.local
module-uri = T.module-uri
dependency = T.dependency

fun ok-last(stmt):
  not(
    A.is-s-let(stmt) or
    A.is-s-tuple-let(stmt) or
    A.is-s-var(stmt) or
    A.is-s-rec(stmt) or
    A.is-s-fun(stmt) or
    A.is-s-data(stmt) or
    A.is-s-contract(stmt) or
    A.is-s-check(stmt) or
    A.is-s-type(stmt) or
    A.is-s-newtype(stmt)
  )
end

fun checkers(l): A.s-app(l, A.s-dot(l, A.s-id(l, A.s-name(l, "builtins")), "current-checker"), [list: ]) end

fun append-nothing-if-necessary(prog :: A.Program) -> A.Program:
  cases(A.Program) prog:
    | s-program(l1, _provide, _provide-types, imports, body) =>
      cases(A.Expr) body:
        | s-block(l2, stmts) =>
          cases(List) stmts:
            | empty =>
              A.s-program(l1, _provide, _provide-types, imports,
                A.s-block(l2, [list: A.s-id(l2, A.s-name(l2, "nothing"))]))
            | link(_, _) =>
              last-stmt = stmts.last()
              if ok-last(last-stmt): prog
              else:
                A.s-program(l1, _provide, _provide-types, imports,
                  A.s-block(l2, stmts + [list: A.s-id(A.dummy-loc, A.s-name(l2, "nothing"))]))
              end
          end
        | else => prog
      end
  end
end

fun wrap-if-needed(exp :: A.Expr) -> A.Expr:
  l = exp.l
  if ok-last(exp):
    A.s-app(l, A.s-dot(l, A.s-id(l, A.s-name(l, "builtins")), "trace-value"),
      [list: A.s-srcloc(l, l), exp])
  else: exp
  end
end

fun wrap-toplevels(prog :: A.Program) -> A.Program:
  cases(A.Program) prog:
    | s-program(l1, _prov, _prov-types, imps, body) =>
      new-body = cases(A.Expr) body:
        | s-block(l2, stmts) => A.s-block(l2, map(wrap-if-needed, stmts))
        | else => wrap-if-needed(body)
      end
      A.s-program(l1, _prov, _prov-types, imps, new-body)
  end
end

fun count-apps(expr) block:
  var count = 0
  visitor = A.default-iter-visitor.{
      method s-app(self, l, f, args) block:
        count := count + 1
        f.visit(self) and args.map(_.visit(self))
      end
    }
  expr.visit(visitor)
  count
end

fun value-delays-exec-of(name, expr):
  A.is-s-lam(expr) or A.is-s-method(expr)
end

letrec-visitor = A.default-map-visitor.{
  env: SD.make-string-dict(),
  method s-letrec(self, l, binds, body, blocky):
    bind-envs = for map2(b1 from binds, i from range(0, binds.length())) block:
      rhs-is-delayed = value-delays-exec-of(b1.b.id, b1.value)
      acc = self.env.unfreeze()
      for each2(b2 from binds, j from range(0, binds.length())):
        key = b2.b.id.key()
        if i < j:
          acc.set-now(key, false)
        else if i == j:
          acc.set-now(key, rhs-is-delayed)
        else:
          acc.set-now(key, true)
        end
      end
      acc.freeze()
    end
    new-binds = for map2(b from binds, bind-env from bind-envs):
      b.visit(self.{ env: bind-env })
    end
    body-env = bind-envs.last().set(binds.last().b.id.key(), true)
    new-body = body.visit(self.{ env: body-env })
    A.s-letrec(l, new-binds, new-body, blocky)
  end,
  method s-id-letrec(self, l, id, _):
    A.s-id-letrec(l, id, self.env.get-value(id.key()))
  end
}

fun make-renamer(replacements :: SD.StringDict):
  A.default-map-visitor.{
    method s-atom(self, base, serial):
      a = A.s-atom(base, serial)
      k = a.key()
      if replacements.has-key(k):
        replacements.get-value(k)
      else:
        a
      end
    end
  }
end

fun wrap-extra-imports(p :: A.Program, env :: CS.ExtraImports) -> A.Program:
  expr = p.block
  cases(CS.ExtraImports) env:
    | extra-imports(imports) =>
      full-imports = p.imports + for map(i from imports):
          name-to-use = if i.as-name == "_": A.s-underscore(p.l) else: A.s-name(p.l, i.as-name) end
          cases(CS.Dependency) i.dependency:
            | builtin(name) =>
              loc = SL.builtin(i.as-name)
              A.s-import-complete(
                p.l,
                i.values.map(A.s-name(loc, _)),
                i.types.map(A.s-name(loc, _)),
                A.s-const-import(p.l, name),
                name-to-use,
                name-to-use)
            | dependency(protocol, args) =>
              A.s-import-complete(
                p.l,
                i.values.map(A.s-name(p.l, _)),
                i.types.map(A.s-name(p.l, _)),
                A.s-special-import(p.l, protocol, args),
                name-to-use,
                name-to-use)
          end
        end
      A.s-program(p.l, p._provide, p.provided-types, full-imports, p.block)
  end
end

fun import-to-dep(imp):
  cases(A.ImportType) imp:
    # crossover compatibility
    | s-const-import(_, modname) => CS.builtin(modname)
    | s-special-import(_, protocol, args) => CS.dependency(protocol, args)
  end
end

fun import-to-dep-anf(imp):
  cases(N.AImportType) imp:
    | a-import-builtin(_, name) => CS.builtin(name)
    | a-import-special(_, kind, args) => CS.dependency(kind, args)
  end
end

fun some-pred<a>(pred :: (a -> Boolean), o :: Option<a>) -> a:
  cases(Option) o block:
    | none => raise("Expected some but got none")
    | some(exp) =>
      when not(pred(exp)):
        raise("Predicate failed for " + torepr(exp))
      end
      exp
  end
end

is-s-data-expr = A.is-s-data-expr

is-t-name = T.is-t-name
type NameChanger = (T.Type%(is-t-name) -> T.Type)


fun get-named-provides(resolved :: CS.NameResolution, uri :: URI, compile-env :: CS.CompileEnvironment) -> CS.Provides:
  fun field-to-typ(f :: A.AField) -> T.TypeMember:
    T.t-member(f.name, ann-to-typ(f.ann))
  end

  fun collect-shared-fields(vs :: List<A.Variant>) block:
    init-members = SD.make-mutable-string-dict()
    for each(m from vs.first.with-members):
      init-members.set-now(m.name, member-to-t-member(m))
    end

    shared-across-variants = init-members

    for each(v from vs.rest):
      for each(m from v.with-members):
        when shared-across-variants.has-key-now(m.name):
          existing-mem = shared-across-variants.get-value-now(m.name)
          this-mem = member-to-t-member(m)
          when not(existing-mem == this-mem):
            shared-across-variants.remove-now(m.name)
          end
        end
      end
    end

    for map(k from shared-across-variants.keys-list-now()):
      shared-across-variants.get-value-now(k)
    end
  end

  fun v-member-to-t-member(m):
    cases(A.VariantMember) m:
      | s-variant-member(l, kind, bind) =>
        typ = if A.is-s-mutable(kind):
            T.t-ref(ann-to-typ(bind.ann), l)
          else:
            ann-to-typ(bind.ann)
          end
        T.t-member(bind.id.toname(), typ)
    end
  end

  fun member-to-t-member(m):
    cases(A.Member) m:
      | s-data-field(l, name, val) =>
        T.t-member(name, T.t-top(l))
      | s-mutable-field(l, name, ann, val) =>
        T.t-member(name, T.t-ref(ann-to-typ(ann)))
      | s-method-field(l, name, params, args, ann, _, _, _, _) =>
        arrow-part =
          T.t-arrow(map(ann-to-typ, map(_.ann, args)), ann-to-typ(ann), l)
        typ =
          if is-empty(params): arrow-part
          else:
            tvars = for map(p from params): T.t-var(p, l) end
            T.t-forall(tvars, arrow-part, l)
          end
        T.t-member(name, typ)
    end
  end

  # TODO(MATT): a-blank should have location
  fun ann-to-typ(a :: A.Ann) -> T.Type:
    cases(A.Ann) a:
      | a-blank => T.t-top(A.dummy-loc)
      | a-any(l) => T.t-top(l)
      | a-name(l, id) =>
        cases(A.Name) id:
          | s-type-global(name) =>
            cases(Option<String>) compile-env.globals.types.get(name):
              | none => raise("Name not found in globals.types: " + name)
              | some(key) =>
                cases(Option<CS.Provides>) compile-env.mods.get(key):
                  | none => raise("Module not found in compile-env.mods: " + key
                        + " (looked up for " + name + " for module " + uri + ")")
                  | some(mod) => T.t-name(T.module-uri(mod.from-uri), id, l)
                end
            end
          | s-atom(_, _) => T.t-name(T.module-uri(uri), id, l)
          | else => raise("Bad name found in ann-to-typ: " + id.key())
        end
      | a-type-var(l, id) =>
        T.t-var(id, l)
      | a-arrow(l, args, ret, use-parens) =>
        T.t-arrow(map(ann-to-typ, args), ann-to-typ(ret), l)
      | a-method(l, args, ret) =>
        raise("Cannot provide a raw method")
      | a-record(l, fields) =>
        T.t-record(map(field-to-typ, fields), l)
      | a-tuple(l, fields) =>
        T.t-top(l)
      | a-app(l, ann, args) =>
        T.t-app(ann-to-typ(ann), map(ann-to-typ, args), l)
      | a-pred(l, ann, exp) =>
        # TODO(joe): give more info than this to type checker?  only needed dynamically, right?
        ann-to-typ(ann)
      | a-dot(l, obj, field) =>
        maybe-b = resolved.type-bindings.get-now(obj.key())
        cases(Option) maybe-b:
          | none =>
            T.t-top(l)
          | some(b) =>
            exp = for some-pred(v from b.ann):
              A.is-s-import-complete(v)
            end
            dep = import-to-dep(exp.import-type)
            provided-from-other = compile-env.mods.get-value(dep.key())
            T.t-name(module-uri(provided-from-other.from-uri), A.s-name(l, field), l)
        end
      | a-checked(checked, residual) =>
        raise("a-checked should only be generated by the type-checker")
    end
  end
  fun data-expr-to-datatype(exp :: A.Expr % (is-s-data-expr)) -> T.Type:
    cases(A.Expr) exp:
      | s-data-expr(l, name, _, params, _, variants, shared-members, _) =>

        tvars = for map(tvar from params):
          T.t-var(tvar, l)
        end

        tvariants = for map(tv from variants):
          cases(A.Variant) tv:
            | s-variant(l2, constr-loc, vname, members, with-members) =>
              T.t-variant(
                vname,
                map(v-member-to-t-member, members),
                map(member-to-t-member, with-members))
            | s-singleton-variant(l2, vname, with-members) =>
              T.t-singleton-variant(
                vname,
                map(member-to-t-member, with-members))
          end
        end

        shared-across-variants = collect-shared-fields(variants)

        all-shared-fields = map(member-to-t-member, shared-members) + shared-across-variants

        T.t-forall(
          tvars,
          T.t-data(
            name,
            tvariants,
            all-shared-fields,
            l),
          l)
    end
  end
  cases(A.Program) resolved.ast:
    | s-program(l, provide-complete, _, _, _) =>
      cases(A.Provide) provide-complete block:
        | s-provide-complete(_, values, aliases, datas) =>
          val-typs = SD.make-mutable-string-dict()
          for each(v from values):
            val-typs.set-now(v.v.toname(), ann-to-typ(v.ann))
          end
          alias-typs = SD.make-mutable-string-dict()
          for each(a from aliases):
            # TODO(joe): recursive lookup here until reaching a non-alias?
            target-binding = resolved.type-bindings.get-value-now(a.in-name.key())
            typ = cases(Option) target-binding.ann:
              | none => T.t-top(l)
              | some(target-ann) =>
                if A.is-Ann(a):
                  ann-to-typ(target-ann)
                else:
                  T.t-top(l)
                end
            end
            alias-typs.set-now(a.out-name.toname(), typ)
          end
          data-typs = SD.make-mutable-string-dict()
          for each(d from datas):
            exp = resolved.datatypes.get-value-now(d.d.key())
            data-typs.set-now(d.d.key(), data-expr-to-datatype(exp))
          end
          CS.provides(
              uri,
              val-typs.freeze(),
              alias-typs.freeze(),
              data-typs.freeze()
            )
      end
  end
end


fun canonicalize-members(ms :: T.TypeMembers, uri :: URI, tn :: NameChanger) -> T.TypeMembers:
  for map(f from ms):
    T.t-member(f.field-name, canonicalize-names(f.typ, uri, tn))
  end
end

fun canonicalize-variant(v :: T.TypeVariant, uri :: URI, tn :: NameChanger) -> T.TypeVariant:
  c = canonicalize-members(_, uri, tn)
  cases(T.TypeVariant) v:
    | t-variant(name, fields, with-fields) =>
      T.t-variant(name, c(fields), c(with-fields))
    | t-singleton-variant(name, with-fields) =>
      T.t-singleton-variant(name, c(with-fields))
  end
end

#fun canonicalize-datatype(dtyp :: T.DataType, uri :: URI) -> T.DataType:
#  cases(T.DataType) dtyp:
#    | t-datatype(name, params, variants, fields) =>
#      T.t-datatype(
#          name,
#          params,
#          map(canonicalize-variant(_, uri), variants),
#          canonicalize-members(fields, uri))
#  end
#end

fun canonicalize-names(typ :: T.Type, uri :: URI, transform-name :: NameChanger) -> T.Type:
  c = canonicalize-names(_, uri, transform-name)
  cases(T.Type) typ:
    | t-name(module-name, id, l) => transform-name(typ)
    | t-var(id, l) => typ
    | t-arrow(args, ret, l) => T.t-arrow(map(c, args), c(ret), l)
    | t-tuple(elts, l) => T.t-tuple(map(c, elts), l)
    | t-app(onto, args, l) => T.t-app(c(onto), map(c, args), l)
    | t-top(l) => T.t-top(l)
    | t-bot(l) => T.t-bot(l)
    | t-record(fields, l) =>
      T.t-record(canonicalize-members(fields, uri, transform-name), l)
    | t-forall(introduces, onto, l) => T.t-forall(map(c, introduces), c(onto), l)
    | t-ref(t, l) => T.t-ref(c(t), l)
    | t-existential(id, l) => typ
    | t-data(name, variants, fields, l) =>
      T.t-data(name, map(canonicalize-variant(_, uri, transform-name), variants), canonicalize-members(fields, uri, transform-name), l)
  end
end

fun find-mod(compile-env, uri) -> Option<String>:
  for find(depkey from compile-env.mods.keys-list()):
    other-uri = compile-env.mods.get-value(depkey).from-uri
    other-uri == uri
  end
end

fun transform-dict(d, uri, transformer):
  for fold(s from [SD.string-dict:], v from d.keys-list()):
    s.set(v, canonicalize-names(d.get-value(v), uri, transformer))
  end
end

fun transform-provides(provides, compile-env, transformer):
  cases(CS.Provides) provides:
  | provides(from-uri, values, aliases, data-definitions) =>
    new-vals = transform-dict(values, from-uri, transformer)
    new-aliases = transform-dict(aliases, from-uri, transformer)
    new-data-definitions = transform-dict(data-definitions, from-uri, transformer)
    CS.provides(from-uri, new-vals, new-aliases, new-data-definitions)
  end
end

fun canonicalize-provides(provides :: CS.Provides, compile-env :: CS.CompileEnvironment):
  doc: ```
  Produces a new provides data structure that has no `dependency` NameOrigins
  in the types, by looking up dependencies in the compile environment.
  Also produces an error if there is a module URI or dependency that is not
  mentioned in the compile-env.
  ```
  transformer = lam(t :: T.Type%(is-t-name)):
    cases(T.Type) t:
    | t-name(origin, name, loc) =>
      cases(T.NameOrigin) origin:
      | local => T.t-name(T.module-uri(provides.from-uri), name, loc)
      | module-uri(uri) =>
        cases(Option<String>) find-mod(compile-env, uri):
        | some(_) => T.t-name(T.module-uri(uri), name, loc)
        | none =>
          if string-index-of(uri, "builtin://") == 0:
            T.t-name(T.module-uri(uri), name, loc)
          else if uri == provides.from-uri:
            T.t-name(T.module-uri(uri), name, loc)
          else:
          # TODO(joe): This should become an error once things are localized again

            #T.t-name(T.module-uri(uri), name, loc)
            raise("Unknown module URI for type: " + torepr(t) + " in provides for " + provides.from-uri)
          end
        end
      | dependency(d) =>
        provides-for-d = compile-env.mods.get(d)
        cases(Option<CS.Provides>) provides-for-d:
        | some(p) => T.t-name(module-uri(p.from-uri), name, loc)
        | none => raise("Unknown module dependency for type: " + torepr(t) + " in provides for " + provides.from-uri)
        end
      end
    end
  end
  transform-provides(provides, compile-env, transformer)
end

fun localize-provides(provides :: CS.Provides, compile-env :: CS.CompileEnvironment):
  doc: ```
  Produces a new provides data structure that has no `module-uri` NameOrigins
  in the types, by looking up uris in the compile environment, or using `local`.

  Also produces an error if there is a module URI or dependency that is not
  mentioned in the compile-env.
  ```
  transformer = lam(t :: T.Type%(is-t-name)):
    cases(T.Type) t:
    | t-name(origin, name, loc) =>
      cases(T.NameOrigin) origin:
      | local => t
      | module-uri(uri) =>
        if uri == provides.from-uri:
          T.t-name(T.local, name, loc)
        else:
          cases(Option<String>) find-mod(compile-env, uri):
          | some(d) => T.t-name(T.dependency(d), name, loc)
          | none =>
            if string-index-of(uri, "builtin://") == 0:
              T.t-name(T.module-uri(uri), name, loc)
            else:
              # TODO(joe): This should become an error once things are localized again
              #T.t-name(T.module-uri(uri), name, loc)

              raise("Unknown module URI for type: " + torepr(t) + " in provides for " + provides.from-uri)
            end
          end
        end
      | dependency(d) =>
        provides-for-d = compile-env.mods.get(d)
        cases(Option<CS.Provides>) provides-for-d:
        | some(p) => t
        | none => raise("Unknown module dependency for type: " + torepr(t) + " in provides for " + provides.from-uri)
        end
      end
    end
  end
  transform-provides(provides, compile-env, transformer)
end

# TODO(MATT): this does not actually get the provided module values
fun get-typed-provides(typed :: TCS.Typed, uri :: URI, compile-env :: CS.CompileEnvironment):
  transformer = lam(t):
    cases(T.Type) t:
      | t-name(origin, name, l) =>
        T.t-name(origin, A.s-type-global(name.toname()), l)
      | else => t
    end
  end
  c = canonicalize-names(_, uri, transformer)
  cases(A.Program) typed.ast block:
    | s-program(_, provide-complete, _, _, _) =>
      cases(A.Provide) provide-complete block:
        | s-provide-complete(_, values, aliases, datas) =>
          val-typs = SD.make-mutable-string-dict()
          for each(v from values):
            val-typs.set-now(v.v.toname(), c(typed.info.types.get-value(v.v.key())))
          end
          alias-typs = SD.make-mutable-string-dict()
          for each(a from aliases):
            # TODO(joe): recursive lookup here until reaching a non-alias?
           cases(Option) typed.info.data-types.get(a.in-name.key()):
              | some(typ) => alias-typs.set-now(a.out-name.toname(), c(typ))
              | none =>
                # NOTE(joe): This was commented _in_ on new-horizons.
                #cases(Option) typed.info.branders.get-now(a.in-name.key()):
                #  | some(typ) => alias-typs.set-now(a.out-name.toname(), c(typ))
                #  | else =>
                #    typ = typed.info.aliases.get-value-now(a.in-name.key())
                #    alias-typs.set-now(a.out-name.toname(), c(typ))
                #end

                # NOTE(joe): We look up `typ` by key, and then
                # canonicalize-names converts all the names to be s-global-type
                # which removes all gensym information from what's provided
                # as types.
                typ = typed.info.aliases.get-value(a.in-name.key())
                alias-typs.set-now(a.out-name.toname(), c(typ))
            end
          end
          data-typs = SD.make-mutable-string-dict()
          for each(d from datas):
            data-typs.set-now(d.d.toname(), c(typed.info.data-types.get-value(d.d.key())))
          end
          CS.provides(
              uri,
              val-typs.freeze(),
              alias-typs.freeze(),
              data-typs.freeze()
            )
      end
  end
end
