#lang pyret

provide *
provide-types *
import ast as A
import file as F
import string-dict as SD
import file("anf.arr") as N
import file("anf-loop-compiler.arr") as AL
import file("ast-anf.arr") as AA
import file("ast-util.arr") as AU
import file("compile-structs.arr") as C
import file("concat-lists.arr") as CL
import file("desugar.arr") as D
import file("desugar-check.arr") as CH
import file("js-ast.arr") as J

# TODO(joe): add methods for printing to module vs static information
data CompiledCodePrinter:
  | ccp-dict(dict :: SD.StringDict) with:
    method to-j-expr(self, d):
      J.j-parens(J.j-obj(for CL.map_list(k from d.keys-list()):
          J.j-field(k, d.get-value(k))
        end))
    end,
    method pyret-to-js-static(self) -> String:
      self.to-j-expr(self.dict.remove("theModule")).to-ugly-source()
    end,
    method print-js-static(self, printer):
      self.to-j-expr(self.dict.remove("theModule")).print-ugly-source(printer)
    end,
    method pyret-to-js-pretty(self, width) -> String:
      self.to-j-expr(self.dict).tosource().pretty(width).join-str("\n")
    end,
    method pyret-to-js-runnable(self) -> String:
      self.to-j-expr(self.dict).to-ugly-source()
    end,
    method print-js-runnable(self, printer):
      self.to-j-expr(self.dict).print-ugly-source(printer)
    end
  | ccp(compiled :: J.JExpr) with:
    method pyret-to-js-pretty(self, width) -> String:
      self.compiled.tosource().pretty(width).join-str("\n")
    end,
    method pyret-to-js-runnable(self) -> String:
      self.compiled.to-ugly-source()
    end,
    method print-js-runnable(self, printer):
      self.compiled.print-ugly-source(printer)
    end
  | ccp-string(compiled :: String) with:
    method pyret-to-js-pretty(self, width) -> String:
      raise("Cannot generate pretty JS from code string")
    end,
    method pyret-to-js-runnable(self) -> String:
      self.compiled
    end,
    method print-js-runnable(self, printer):
      printer(self.compiled)
    end
  | ccp-file(path :: String) with:
    method pyret-to-js-pretty(self, width) -> String:
      raise("Cannot generate pretty JS from code string")
    end,
    method pyret-to-js-runnable(self) -> String block:
      F.file-to-string(self.path)
    end,
    method print-js-runnable(self, printer):
      printer(self.pyret-to-js-runnable())
    end
end

fun flatness-max(a :: Option<Number>, b :: Option<Number>) -> Option<Number> block:
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

# Maybe compress Option<Number> into a type like FlatnessInfo or something (maybe something "Info" in the name)
fun make-expr-flatness-env(
    aexpr :: AA.AExpr,
    sd :: SD.MutableStringDict<Option<Number>>) -> Option<Number>:
  cases(AA.AExpr) aexpr:
    | a-type-let(_, bind, body) =>
      make-expr-flatness-env(body, sd)
    | a-let(_, bind, val, body) =>
      val-flatness = if AA.is-a-lam(val) or AA.is-a-method(val) block:
        lam-flatness = make-expr-flatness-env(val.body, sd)
        sd.set-now(bind.id.key(), lam-flatness)
        # flatness of defining this lambda is 0, since we're not actually
        # doing anything with it
        some(0)
      else if AA.is-a-id-letrec(val) and val.safe:
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
  # Look up flatness in the dictionary
  fun inc-flatness(flat-opt :: Option<Number>):
    flat-opt.and-then(lam(x): x + 1 end)
  end

  # If it's not in our lookup dict OR the flatness is none treat it the same
  val = sd.get-now(function-name).or-else(none)
  cases (Option) val:
    | some(flatness) => some(flatness + 1)
    | none => none
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
    | a-lam(_, name, args, ret, body) =>
      # I believe the only way we can reach this case is if we write code
      # like:
      # lam(x): x end
      # That is, we define a lambda, but don't bind it to anything
      if string-equal(name, ""):
        default-ret
      else:
        raise("lam should be anonymous!")
      end
    | a-method(_, name, args, ret, body) =>
      default-ret
    | a-id-var(_, id) =>
      default-ret
    | a-id-letrec(_, id, safe) =>
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

  for each(k from bindings.keys-list-now()):
    vb = bindings.get-value-now(k)
    when C.is-bo-module(vb.origin):
      cases(Option) vb.origin.mod:
        | none => nothing
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
        make-expr-flatness-env(body, sd)
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
      new-values = for fold(s from [SD.string-dict:], k from values.keys-list()):
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

fun trace-make-compiled-pyret(add-phase, program-ast, env, bindings, provides, options)
  -> { C.Provides; C.CompileResult<CompiledCodePrinter> } block:
  anfed = add-phase("ANFed", N.anf-program(program-ast))
  flatness-env = add-phase("Build flatness env", make-prog-flatness-env(anfed, bindings, env))
  flat-provides = add-phase("Get flat-provides", get-flat-provides(provides, flatness-env, anfed))
  compiled = anfed.visit(AL.splitting-compiler(env, add-phase, flatness-env, flat-provides, options))
  {flat-provides; add-phase("Generated JS", C.ok(ccp-dict(compiled)))}
end

