#lang pyret

provide *
provide-types *
import file("compile-structs.arr") as C
import file("concat-lists.arr") as CL
import ast as A
import string-dict as SD
import file as F
import file("anf.arr") as N
import file("ast-anf.arr") as AA
import file("anf-loop-compiler.arr") as AL
import file("desugar-check.arr") as CH
import file("desugar.arr") as D
import file("ast-util.arr") as AU
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
        sd.set-now(tostring(bind.id), lam-flatness)
        # flatness of defining this lambda is 0, since we're not actually
        # doing anything with it
        some(0)
      else if AA.is-a-id-letrec(val):
        block:
          # If we're binding this name to something that's already been defined
          # just copy over the definition
          known-flatness-opt = sd.get-now(tostring(val.id))
          cases (Option) known-flatness-opt:
            | some(flatness) => sd.set-now(tostring(bind.id), flatness)
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
  val = sd.get-now(tostring(function-name)).or-else(none)
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
    | a-assign(_, id, value) =>
      block:
        if AA.is-a-id(value) and sd.has-key-now(tostring(value.id)):
          sd.set-now(tostring(id), sd.get-value-now(tostring(value.id)))
        else:
          #FIXME: this branch is required, kind of inconvenient
          none
        end
        default-ret
      end
    | a-app(_, f, args) =>
      # Look up flatness in the dictionary
      if AA.is-a-id(f):
        get-flatness-for-call(tostring(f.id), sd)
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

fun make-prog-flatness-env(anfed :: AA.AProg) -> SD.StringDict<Number> block:
  sd = SD.make-mutable-string-dict()
  flatness-env = cases(AA.AProg) anfed:
    | a-program(_, prov, imports, body) => block:
        #print("AST is " + tostring(body.tosource().pretty(2000)) + "\n")
        make-expr-flatness-env(body, sd)
        sd
      end
  end
  #print("flatness env: " + tostring(flatness-env) + "\n")
  flatness-env.freeze()
end

fun make-compiled-pyret(program-ast, env, provides, options) -> CompiledCodePrinter block:
  anfed = N.anf-program(program-ast)
  flatness-env = make-prog-flatness-env(anfed)
  print("Flatness env is " + tostring(flatness-env) + "\n")
  compiled = anfed.visit(AL.splitting-compiler(env, flatness-env, provides, options))
  ccp-dict(compiled)
end

fun trace-make-compiled-pyret(trace, phase, program-ast, env, provides, options) block:
  var ret = trace
  anfed = N.anf-program(program-ast)
  ret := phase("ANFed", anfed, ret)
  flatness-env = make-prog-flatness-env(anfed)
  phase("Generated JS", ccp-dict(anfed.visit(AL.splitting-compiler(env, flatness-env, provides, options))), ret)
end
