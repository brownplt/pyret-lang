provide *
provide-types *

import ast as A
import string-dict as SD

import file("ast-anf.arr") as AA
import file("ast-util.arr") as AU
import file("compile-structs.arr") as C

fun make-expr-data-env(
    aexpr :: AA.AExpr,
    sd :: SD.MutableStringDict<Option<Number>>,
    type-name-to-variants :: SD.MutableStringDict<List<AA.AVariant>>,
    alias-to-type-name :: SD.MutableStringDict<String>):
  cases(AA.AExpr) aexpr:
    | a-type-let(_, bind, body) =>
      make-expr-data-env(body, sd, type-name-to-variants,
        alias-to-type-name)
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
          when is-some(type-name-opt):
            type-name = type-name-opt.value
            variants = type-name-to-variants.get-value-now(type-name)
            is-is-function = string-index-of(val.field, "is-") == 0
            when is-is-function or any(lam(v): (v.name == val.field) and AA.is-a-variant(v) end, variants):
              sd.set-now(bind.id.key(), some(0))
            end
          end
        else:
          none
        end
        make-lettable-data-env(val, sd, type-name-to-variants,
          alias-to-type-name)
        make-expr-data-env(body, sd, type-name-to-variants,
          alias-to-type-name)
      end
    | a-arr-let(_, bind, idx, e, body) => block:
        make-lettable-data-env(e, sd, type-name-to-variants,
          alias-to-type-name)
        make-expr-data-env(body, sd, type-name-to-variants,
          alias-to-type-name)
      end
    | a-var(_, bind, val, body) =>
      make-expr-data-env(body, sd, type-name-to-variants,
        alias-to-type-name)
    | a-seq(_, lettable, expr) =>
      block:
        make-lettable-data-env(lettable, sd, type-name-to-variants,
          alias-to-type-name)
        make-expr-data-env(expr, sd, type-name-to-variants,
          alias-to-type-name)
      end
    | a-lettable(_, l) =>
      make-lettable-data-env(l, sd, type-name-to-variants,
        alias-to-type-name)
  end
end

fun make-lettable-data-env(
    lettable :: AA.ALettable,
    sd :: SD.MutableStringDict<Option<Number>>,
    type-name-to-variants :: SD.MutableStringDict<List<AA.AVariant>>,
    alias-to-type-name :: SD.MutableStringDict<String>):
  default-ret = none
  cases(AA.ALettable) lettable:
    | a-module(_, answer, dv, dt, provides, types, checks) =>
      default-ret
    | a-if(_, c, t, e) =>
      block:
        make-expr-data-env(t, sd, type-name-to-variants,
          alias-to-type-name)
        make-expr-data-env(e, sd, type-name-to-variants,
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
    | a-prim-app(_, f, args) => default-ret
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
          make-expr-data-env(case-branch.body, sd, type-name-to-variants,
            alias-to-type-name)
        end
        each(visit-branch, branches)
        make-expr-data-env(els, sd, type-name-to-variants,
          alias-to-type-name)
      end
  end
end


fun flatness-max(a :: Option<Number>, b :: Option<Number>) -> Option<Number>:
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

fun is-trivial-ann(ann):
  A.is-a-blank(ann) or A.is-a-any(ann)
    or (A.is-a-tuple(ann) and ann.fields.all(lam(field-a): A.is-a-blank(field-a) or A.is-a-any(field-a) end))
end

# Maybe compress Option<Number> into a type like FlatnessInfo or something (maybe something without "Info" in the name)
fun make-expr-flatness-env(
    aexpr :: AA.AExpr,
    sd :: SD.MutableStringDict<Option<Number>>) -> Option<Number>:
  cases(AA.AExpr) aexpr:
    | a-type-let(_, bind, body) =>
      make-expr-flatness-env(body, sd)
    | a-let(_, bind, val, body) =>
      val-flatness = if AA.is-a-lam(val) block:
        has-nontrivial-arg-ann = for lists.any(elt from val.args):
          not(is-trivial-ann(elt))
        end
        has-nontrivial-ann = has-nontrivial-arg-ann or not(is-trivial-ann(val.ret))

        lam-flatness = if has-nontrivial-ann:
          none
        else:
          make-expr-flatness-env(val.body, sd)
        end

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
        make-lettable-flatness-env(val, sd)
      end

      # Compute the flatness of the body
      body-flatness = make-expr-flatness-env(body, sd)

      flatness-max(val-flatness, body-flatness)
    | a-arr-let(_, bind, idx, e, body) =>
      # Could maybe try to add some string like "bind.name + idx" to the
      # sd to let us keep track of the flatness if e is an a-lam, but for
      # now we don't since I'm not sure it'd work right.
      flatness-max(make-lettable-flatness-env(e, sd), make-expr-flatness-env(body, sd))
    | a-var(_, bind, val, body) =>
      # Do same thing with a-var as with a-let for now
      make-expr-flatness-env(body, sd)
    | a-seq(_, lettable, expr) =>
      a-flatness = make-lettable-flatness-env(lettable, sd)
      b-flatness = make-expr-flatness-env(expr, sd)
      flatness-max(a-flatness, b-flatness)
    | a-lettable(_, l) => make-lettable-flatness-env(l, sd)
  end
end

fun get-flatness-for-call(function-name :: String, sd :: SD.MutableStringDict<Option<Number>>) -> Option<Number>:
  # If it's not in our lookup dict OR the flatness is none treat it the same
  if sd.has-key-now(function-name):
    cases(Option) sd.get-value-now(function-name):
      | some(flatness) => some(flatness + 1)
      | none => none
    end
  else:
    none
  end
end

fun make-lettable-flatness-env(
    lettable :: AA.ALettable,
    sd :: SD.MutableStringDict<Option<Number>>) -> Option<Number>:
  default-ret = some(0)
  cases(AA.ALettable) lettable:
    | a-module(_, answer, dv, dt, provides, types, checks) =>
      default-ret
    | a-if(_, c, t, e) =>
      flatness-max(make-expr-flatness-env(t, sd), make-expr-flatness-env(e, sd))

    # NOTE -- a-assign might not be flat b/c it checks annotations
    | a-assign(_, id, value) =>
      block:
        when AA.is-a-id(value) and sd.has-key-now(value.id.key()):
          sd.set-now(id.key(), sd.get-value-now(value.id.key()))
        end
        default-ret
      end
    | a-app(_, f, args, _) =>
      # Look up flatness in the dictionary
      if AA.is-a-id(f):
        get-flatness-for-call(f.id.key(), sd)
      else:
        # This should never happen in a "correct" program, but it's not our job
        # to do this kind of checking here, so don't raise an error.
        none
      end
    | a-method-app(_, obj, meth, args) =>
      # For now method calls are infinite flatness
      none
    | a-prim-app(_, f, args) => get-flatness-for-call(f, sd)
      # TODO: Treat prim-app as flat
      # Not worrying about these cases yet, though if they all deal with values, should be trivial
    | a-ref(_, ann) => default-ret
    | a-tuple(_, fields) => default-ret
    | a-tuple-get(_, tup, index) => default-ret
    | a-obj(_, fields) => default-ret

    # NOTE -- update might not be flat b/c it checks annotations
    | a-update(_, supe, fields) => default-ret
    | a-extend(_, supe, fields) => default-ret
    | a-dot(_, obj, field) => default-ret
    | a-colon(_, obj, field) => default-ret
    | a-get-bang(_, obj, field) =>
      default-ret
    | a-lam(_, name, args, ret, body) => default-ret
    | a-method(_, name, args, ret, body) =>
      default-ret
    | a-id-var(_, id) =>
      default-ret
    | a-id-letrec(_, id, safe) =>
      default-ret
    | a-id-safe-letrec(_, id) =>
      default-ret
    | a-val(_, v) =>
      default-ret
    | a-data-expr(l, name, namet, vars, shared) =>
      default-ret
    # NOTE -- cases might not be flat b/c it checks annotations
    | a-cases(_, typ, val, branches, els) =>
      # Flatness is the max of the flatness all the cases branches
      combine = lam(case-branch, max-flat):
        branch-flatness = make-expr-flatness-env(case-branch.body, sd)
        flatness-max(max-flat, branch-flatness)
      end
      max-flat = branches.foldl(combine, some(0))

      else-flat = make-expr-flatness-env(els, sd)
      flatness-max(max-flat, else-flat)
  end
end

fun make-prog-flatness-env(anfed :: AA.AProg, bindings :: SD.MutableStringDict<C.ValueBind>, env :: C.CompileEnvironment) -> SD.StringDict<Number> block:

  sd = SD.make-mutable-string-dict()
  for SD.each-key-now(k from bindings):
    vb = bindings.get-value-now(k)
    when C.is-bo-module(vb.origin):
      cases(Option) vb.origin.mod block:
        | none =>
          when A.is-s-global(vb.atom) block:
            name = vb.atom.toname()
            uri = env.globals.values.get-value(name)
            provides-opt = env.mods.get(uri)
            cases (Option) provides-opt:
              | none => nothing
              | some(provides) =>
                ve = provides.values.get-value(name)
                cases(C.ValueExport) ve:
                  | v-fun(_, _, flatness) => sd.set-now(vb.atom.key(), flatness)
                  | else => nothing
                end
            end
          end
        | some(import-type) =>
          dep = AU.import-to-dep(import-type).key()
          cases(Option) env.mods.get(dep):
            | none => raise("There is a binding whose module is not in the compile env: " + to-repr(k) + " " + to-repr(import-type))
            | some(provides) =>
              exported-as = vb.atom.toname()
              value-export = provides.values.get-value(exported-as)
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

  flatness-env = cases(AA.AProg) anfed:
    | a-program(_, prov, imports, body) => block:
        make-expr-data-env(body, sd,
          SD.make-mutable-string-dict(), SD.make-mutable-string-dict())
        make-expr-flatness-env(body, sd)
        #print("flatness env: " + tostring(sd) + "\n\n")
        sd
      end
  end
  #print("flatness env: " + tostring(flatness-env) + "\n")
  flatness-env.freeze()
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

fun get-flat-provides(provides, flatness-env, ast) block:
  dvs-dict = get-defined-values(ast)
  cases(C.Provides) provides block:
    | provides(uri, values, aliases, datatypes) =>
      new-values = for SD.fold-keys(s from [SD.string-dict:], k from values):
        maybe-flatness = flatness-env.get(dvs-dict.get-value(k))
        existing-val = values.get-value(k)
        new-val = cases(Option) maybe-flatness:
          | none => existing-val
          | some(flatness-result) =>
            cases(Option) flatness-result:
              | none => existing-val
              | some(flatness) => C.v-fun(existing-val.t, k, some(flatness))
            end
        end
        s.set(k, new-val)
      end
      C.provides(uri, new-values, aliases, datatypes)
  end
end

