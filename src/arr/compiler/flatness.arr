provide *
provide-types *

import ast as A
import string-dict as SD

import file("ast-anf.arr") as AA
import file("ast-util.arr") as AU
import file("compile-structs.arr") as C

# A flatness environment maps from ANF id names to
#
# - none, if the name is for  a function with an infinitely deep body
# - some(n), where n is the number of nested calls that the function contains
#
# If a name isn't present, it is equivalent to containing a mapping for none
#
# This notion is naturally extended to named annotations, which are similar to
# functions in that they delay computation until later
type Flatness = Option<Number>
type FEnv = SD.MutableStringDict<Flatness>

fun flatness-max(a :: Flatness, b :: Flatness) -> Flatness:
  # read the docs, maybe there's a quicker way to write this
  cases (Option) a:
    | some(a-val) =>
      cases (Option) b:
        | some(b-val) =>
          some(num-max(a-val, b-val))
        | none => none
      end
    | none => none
  end
end


fun ann-flatness(ann :: A.Ann, val-env :: FEnv, ann-env :: FEnv) -> Flatness:
  doc: ```
  Calculate the flatness of an annotation. Does not change val-env and ann-env
  ```
  cases(A.Ann) ann:
    | a-blank => some(0)
    | a-any(l) => some(0)
    | a-name(l, id) =>
      ann-env.get-now(id.key()).or-else(none)
    | a-type-var(l, id) => some(0)
    | a-arrow(l, _, _, _) =>
      #NOTE(joe): This is a flat check because it's not higher-order; we don't check args and ret
      some(0)
    | a-arrow-argnames(l, _, _, _) =>
      #NOTE(joe): This is a flat check because it's not higher-order; we don't check args and ret
      some(0)
    | a-method(l, _, _) => some(0)
    | a-record(l, fields) =>
      for fold(flatness from some(0), f from fields):
        flatness-max(flatness, ann-flatness(f.ann, val-env, ann-env))
      end
    | a-tuple(l, fields) =>
      for fold(flatness from some(0), f from fields):
        flatness-max(flatness, ann-flatness(f, val-env, ann-env))
      end
    | a-app(l, base, _) =>
      # NOTE(joe): the args are ignored because we don't dynamically check
      # the Number in List<Number>
      ann-flatness(base, val-env, ann-env)
    | a-pred(l, base, exp) =>
      val-flatness = val-env.get-now(exp.id.key()).or-else(none)
      flatness-max(
        ann-flatness(base, val-env, ann-env),
        val-flatness
      )
    | a-unit(l, base, u) => ann-flatness(base, val-env, ann-env)
    | a-dot(l, obj, field) => none # TODO(joe): module-ids should make this doable...
    | a-checked(checked, residual) => none
  end
end


fun make-expr-data-env(
    aexpr :: AA.AExpr,
    sd :: FEnv,
    ad :: FEnv,
    type-name-to-variants :: SD.MutableStringDict<List<AA.AVariant>>,
    alias-to-type-name :: SD.MutableStringDict<String>):

  doc: ```
(Mutably) fills in the sd (value environment) with flatnesses for predicates
and constructors, and ad (type environment) with flatnesses for datatype annotations
and type aliases. Return value should be ignored.
  ```

  cases(AA.AExpr) aexpr block:
    | a-type-let(_, bind, body) =>
      cases(AA.ATypeBind) bind:
        | a-newtype-bind(l, name-of-type, name-of-brand-value) =>
          # We know that the annotation for a newtype bind is just a flat
          # brand check, so make it some(0)
          ad.set-now(name-of-type.key(), some(0))
        | a-type-bind(l, name-of-alias, underlying-ann) =>
          ad.set-now(name-of-alias.key(), ann-flatness(underlying-ann, sd, ad))
      end
      make-expr-data-env(body, sd, ad, type-name-to-variants, alias-to-type-name)
    | a-let(_, bind, val, body) => block:
        if AA.is-a-data-expr(val) block:
          type-name-to-variants.set-now(bind.id.key(), val.variants)
          # Make self-mapping entry so we know it's a "type" name
          alias-to-type-name.set-now(bind.id.key(), bind.id.key())
        else if AA.is-a-id-safe-letrec(val):
          # If we say
          # x = Type
          # y = x
          # z = y
          # We say z and y are aliases of x
          type-name-opt = alias-to-type-name.get-now(val.id.key())
          when is-some(type-name-opt):
            alias-to-type-name.set-now(bind.id.key(), type-name-opt.value)
          end
        else if AA.is-a-dot(val) and AA.is-a-id-safe-letrec(val.obj):
          # Check for: xyz = Type.is-variant or xyz = Type.flat-constructor
          type-name-opt = alias-to-type-name.get-now(val.obj.id.key())
          when is-some(type-name-opt) block:
            type-name = type-name-opt.value
            variants = type-name-to-variants.get-value-now(type-name)

            is-is-function = any(lam(v): ("is-" + v.name) == val.field end, variants)
            when is-is-function:
              sd.set-now(bind.id.key(), some(0))
            end

            the-variant = find(lam(v): (v.name == val.field) and AA.is-a-variant(v) end, variants)
            when is-some(the-variant):
              variant-flatness = for fold(flatness from some(0), m from the-variant.value.members):
                flatness-max(flatness, ann-flatness(m.bind.ann, sd, ad))
              end
              sd.set-now(bind.id.key(), variant-flatness)
            end

          end
        else:
          nothing
        end
        make-lettable-data-env(val, sd, ad, type-name-to-variants,
          alias-to-type-name)
        make-expr-data-env(body, sd, ad, type-name-to-variants,
          alias-to-type-name)
      end
    | a-arr-let(_, bind, idx, e, body) => block:
        make-lettable-data-env(e, sd, ad, type-name-to-variants,
          alias-to-type-name)
        make-expr-data-env(body, sd, ad, type-name-to-variants,
          alias-to-type-name)
      end
    | a-var(_, bind, val, body) =>
      make-expr-data-env(body, sd, ad, type-name-to-variants,
        alias-to-type-name)
    | a-seq(_, lettable, expr) =>
      block:
        make-lettable-data-env(lettable, sd, ad, type-name-to-variants,
          alias-to-type-name)
        make-expr-data-env(expr, sd, ad, type-name-to-variants,
          alias-to-type-name)
      end
    | a-lettable(_, l) =>
      make-lettable-data-env(l, sd, ad, type-name-to-variants,
        alias-to-type-name)
  end
end

fun make-lettable-data-env(
    lettable :: AA.ALettable,
    sd :: FEnv,
    ad :: FEnv,
    type-name-to-variants :: SD.MutableStringDict<List<AA.AVariant>>,
    alias-to-type-name :: SD.MutableStringDict<String>):
  default-ret = none
  cases(AA.ALettable) lettable:
    | a-module(_, answer, dv, dt, provides, types, checks) =>
      default-ret
    | a-if(_, c, t, e) =>
      block:
        make-expr-data-env(t, sd, ad, type-name-to-variants,
          alias-to-type-name)
        make-expr-data-env(e, sd, ad, type-name-to-variants,
          alias-to-type-name)
      end
    | a-assign(_, id, value) =>
      block:
        when AA.is-a-id(value) block:
          when sd.has-key-now(value.id.key()):
            sd.set-now(id.key(), sd.get-value-now(value.id.key()))
          end

          when alias-to-type-name.has-key-now(value.id.key()):
            val-type = alias-to-type-name.get-value-now(value.id.key())
            alias-to-type-name.set-now(id.key(), val-type)
          end
        end

        when AA.is-a-id-safe-letrec(value):
          type-name-opt = alias-to-type-name.get-now(value.id.key())
          when is-some(type-name-opt):
            alias-to-type-name.set-now(id.key(), type-name-opt.value)
          end
        end
      end
    | a-app(_, f, args, _) => default-ret
    | a-method-app(_, obj, meth, args) => default-ret
    | a-prim-app(_, f, args, _) => default-ret
    | a-ref(_, ann) => default-ret
    | a-tuple(_, fields) => default-ret
    | a-tuple-get(_, tup, index) => default-ret
    | a-obj(_, fields) => default-ret
    | a-update(_, supe, fields) => default-ret
    | a-extend(_, supe, fields) => default-ret
    | a-dot(_, obj, field) => default-ret
    | a-colon(_, obj, field) => default-ret
    | a-get-bang(_, obj, field) => default-ret
    | a-lam(_, name, args, ret, body) => default-ret
    | a-method(_, name, args, ret, body) => default-ret
    | a-id-var(_, id) => default-ret
    | a-id-letrec(_, id, safe) => default-ret
    | a-id-safe-letrec(_, id) => default-ret
    | a-val(_, v) => default-ret
    | a-data-expr(l, name, namet, vars, shared) => default-ret
    | a-cases(_, typ, val, branches, els) => block:
        visit-branch = lam(case-branch):
          make-expr-data-env(case-branch.body, sd, ad, type-name-to-variants,
            alias-to-type-name)
        end
        each(visit-branch, branches)
        make-expr-data-env(els, sd, ad, type-name-to-variants,
          alias-to-type-name)
      end
  end
end


fun make-expr-flatness-env(aexpr :: AA.AExpr, sd :: FEnv, ad :: FEnv) -> Flatness:
  doc: ```
  Calculate the flatness of aexpr, and along the way mutably update sd to
  contain mappings for all defined names of functions
  ```
  cases(AA.AExpr) aexpr block:
    | a-type-let(_, bind, body) =>
      make-expr-flatness-env(body, sd, ad)
    | a-let(_, bind, val, body) =>

      val-flatness = if AA.is-a-lam(val) block:

        ret-flatness = ann-flatness(val.ret, sd, ad)
        args-flatness = for fold(f from ret-flatness, elt from val.args):
          flatness-max(f, ann-flatness(elt.ann, sd, ad))
        end


        body-flatness = make-expr-flatness-env(val.body, sd, ad)
        lam-flatness = flatness-max(body-flatness, args-flatness)

        sd.set-now(bind.id.key(), lam-flatness)
        # flatness of defining this lambda is 0, since we're not actually
        # doing anything with it
        some(0)
      else if AA.is-a-id-safe-letrec(val):
        block:
          # If we're binding this name to something that's already been defined
          # just copy over the definition
          known-flatness-opt = sd.get-now(val.id.key())
          cases (Option) known-flatness-opt:
            | some(flatness) => sd.set-now(bind.id.key(), flatness)
            | none => none
          end
          # flatness of the binding part of the let is 0 since we don't
          # call anything
          some(0)
        end
      else:
        make-lettable-flatness-env(val, sd, ad)
      end

      # Compute the flatness of the body
      body-flatness = make-expr-flatness-env(body, sd, ad)

      ann-f = ann-flatness(bind.ann, sd, ad)

      flatness-max(flatness-max(val-flatness, body-flatness), ann-f)
    | a-arr-let(_, bind, idx, e, body) =>
      # Could maybe try to add some string like "bind.name + idx" to the
      # sd to let us keep track of the flatness if e is an a-lam, but for
      # now we don't since I'm not sure it'd work right.
      flatness-max(ann-flatness(bind.ann, sd, ad),
        flatness-max(make-lettable-flatness-env(e, sd, ad), make-expr-flatness-env(body, sd, ad)))
    | a-var(_, bind, val, body) =>
      # Do same thing with a-var as with a-let for now
      flatness-max(ann-flatness(bind.ann, sd, ad), make-expr-flatness-env(body, sd, ad))
    | a-seq(_, lettable, expr) =>
      a-flatness = make-lettable-flatness-env(lettable, sd, ad)
      b-flatness = make-expr-flatness-env(expr, sd, ad)
      flatness-max(a-flatness, b-flatness)
    | a-lettable(_, l) =>
      make-lettable-flatness-env(l, sd, ad)
  end
end

fun get-flatness-for-call(fun-name :: String, sd :: FEnv) -> Flatness:
  # If it's not in our lookup dict OR the flatness is none treat it the same
  if sd.has-key-now(fun-name):
    cases(Option) sd.get-value-now(fun-name):
      | some(flatness) => some(flatness + 1)
      | none => none
    end
  else:
    none
  end
end

fun make-lettable-flatness-env(lettable :: AA.ALettable, sd :: FEnv, ad :: FEnv) -> Flatness:
  default-ret = some(0)
  cases(AA.ALettable) lettable:
    | a-module(_, answer, dv, dt, provides, types, checks) =>
      default-ret
    | a-if(_, c, t, e) =>
      flatness-max(make-expr-flatness-env(t, sd, ad), make-expr-flatness-env(e, sd, ad))

    # NOTE -- a-assign might not be flat b/c it checks annotations
    | a-assign(_, id, value) =>
      block:
        when AA.is-a-id(value) and sd.has-key-now(value.id.key()):
          sd.set-now(id.key(),
            flatness-max(sd.get-now(id.key()).or-else(some(0)), sd.get-value-now(value.id.key())))
        end
        default-ret
      end

    | a-app(_, f, args, _) =>
      # Look up flatness in the dictionary
      if AA.is-a-id(f) or AA.is-a-id-safe-letrec(f):
        get-flatness-for-call(f.id.key(), sd)
      else:
        # This should never happen in a "correct" program, but it's not our job
        # to do this kind of checking here, so don't raise an error.
        none
      end

    | a-method-app(_, obj, meth, args) =>
      # For now method calls are infinite flatness
      none

      # TODO: Treat prim-app as flat always? Track depths of prim-anns?
    | a-prim-app(_, f, args, _) => get-flatness-for-call(f, sd)

      # May check unknown annotations, so is nonflat
    | a-update(_, supe, fields) => none

      # These are flat value constructors, and due to ANF, they only contain
      # values as sub-fields
    | a-ref(_, ann) => default-ret
    | a-tuple(_, fields) => default-ret
    | a-tuple-get(_, tup, index) => default-ret
    | a-obj(_, fields) => default-ret

    | a-extend(_, supe, fields) => default-ret
    | a-dot(_, obj, field) => default-ret
    | a-colon(_, obj, field) => default-ret
    | a-get-bang(_, obj, field) => default-ret
    | a-lam(_, name, args, ret, body) => default-ret
    | a-method(_, name, args, ret, body) => default-ret
    | a-id-var(_, id) => default-ret
    | a-id-letrec(_, id, safe) => default-ret
    | a-id-safe-letrec(_, id) => default-ret
    | a-val(_, v) => default-ret
    | a-data-expr(l, name, namet, vars, shared) => default-ret
    # NOTE -- cases might not be flat b/c it checks annotations
    | a-cases(_, typ, val, branches, els) =>
      # Flatness is the max of the flatness all the cases branches
      combine = lam(case-branch, max-flat):
        branch-flatness = make-expr-flatness-env(case-branch.body, sd, ad)
        flatness-max(max-flat, branch-flatness)
      end
      max-flat = branches.foldl(combine, some(0))

      else-flat = make-expr-flatness-env(els, sd, ad)
      typ-flat = ann-flatness(typ, sd, ad)
      flatness-max(typ-flat, flatness-max(max-flat, else-flat))
  end
end

fun make-prog-flatness-env(anfed :: AA.AProg, bindings :: SD.MutableStringDict<C.ValueBind>, type-bindings :: SD.MutableStringDict<C.TypeBind>, env :: C.CompileEnvironment)
-> { SD.MutableStringDict<Option<Number>>; SD.MutableStringDict<Option<Number>> } block:

  sd = SD.make-mutable-string-dict()
  for SD.each-key-now(k from bindings):
    vb = bindings.get-value-now(k)
    when C.is-bo-module(vb.origin):
      if A.is-s-global(vb.atom) block:
        name = vb.atom.toname()
        cases (Option) env.global-value(name):
          | none => nothing
          | some(ve) =>
            cases(C.ValueExport) ve:
              | v-fun(_, _, flatness) => sd.set-now(vb.atom.key(), flatness)
              | else => nothing
            end
        end
      else:
        cases(Option) env.value-by-uri(vb.origin.uri, vb.atom.toname()):
          | none => raise("The name: " + vb.atom.toname() + " could not be found on the module " + vb.origin.uri)
          | some(value-export) =>
            cases(C.ValueExport) value-export:
              | v-fun(_, _, flatness) =>
                sd.set-now(k, flatness)
              | else =>
                nothing
            end
        end
      end
    end
  end


  ad = SD.make-mutable-string-dict()
  fun init-type-provides(provides, tb) block:
    name = tb.atom.toname()

    # NOTE(joe): Datatypes _must_ just be flat brand checks

    when provides.data-definitions.has-key(name):
      ad.set-now(tb.atom.key(), some(0))
    end

    # NOTE(joe): Right now we don't trust any cross-module aliases. We need to
    # get either a representation of flatness for annotations in provides, or
    # make sure that all provided annotations have a path back to the
    # underlying annotation in terms of datatypes and simple constructors so we
    # can use ann-flatness on them

    when provides.aliases.has-key(name):
      ad.set-now(tb.atom.key(), none)
    end

  end
  for SD.each-key-now(k from type-bindings):
    tb = type-bindings.get-value-now(k)
    when C.is-bo-module(tb.origin):
      if A.is-s-type-global(tb.atom):
        name = tb.atom.toname()
        provides-opt = env.provides-by-type-name(name)
        cases (Option) provides-opt:
          | none => nothing
          | some(provides) =>
            init-type-provides(provides, tb)
        end
      else:
        cases(Option) env.provides-by-uri(tb.origin.uri):
          | none => raise("There is a type binding whose module is not in the compile env: " + to-repr(k) + " " + tb.origin.uri)
          | some(mod-provides) =>
            init-type-provides(mod-provides, tb)
        end
      end
    end
  end


  cases(AA.AProg) anfed:
    | a-program(_, prov, imports, body) => block:
      make-expr-data-env(body, sd, ad,
        SD.make-mutable-string-dict(), SD.make-mutable-string-dict())
      make-expr-flatness-env(body, sd, ad)
      { sd; ad }
      end
  end
end


fun get-defined-values(ast):
  fun help(ae):
    cases(AA.AExpr) ae:
      | a-type-let(_, _, body) => help(body)
      | a-let(_, _, _, body) => help(body)
      | a-arr-let(_, _, _, _, body) => help(body)
      | a-var(_, _, _, body) => help(body)
      | a-seq(_, _, e2) => help(e2)
      | a-lettable(_, e) =>
        block:
          when not(AA.is-a-module(e)):
            raise("Ill-formed ANF ast: " + torepr(e))
          end
          e
        end
    end
  end

  the-module = help(ast.body)
  the-dvs = the-module.defined-values

  dvs-dict = for fold(s from [SD.string-dict:], d from the-dvs):
    cases(AA.ADefinedValue) d:
      | a-defined-value(name, val) => s.set(name, val.id.key())
      | a-defined-var(name, id) => s.set(name, id.key())
    end
  end

  dvs-dict
end

fun get-flat-provides(provides, { flatness-env; _ }, ast) block:
  dvs-dict = get-defined-values(ast)
  cases(C.Provides) provides block:
    | provides(uri, values, aliases, datatypes) =>
      new-values = for SD.fold-keys(s from [SD.string-dict:], k from values):
        maybe-flatness = flatness-env.get-now(dvs-dict.get-value(k))
        existing-val = values.get-value(k)
        new-val = cases(Option) maybe-flatness:
          | none => existing-val
          | some(flatness-result) =>
            C.v-fun(existing-val.t, k, flatness-result)
        end
        s.set(k, new-val)
      end
      C.provides(uri, new-values, aliases, datatypes)
  end
end

