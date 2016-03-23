
provide { make-repl: make-repl } end
import either as E
import load-lib as L
import namespace-lib as N
import string-dict as SD
import runtime-lib as R
import ast as A
import "compiler/ast-util.arr" as U
import "compiler/resolve-scope.arr" as RN
import "compiler/compile-structs.arr" as CS
import "compiler/compile-lib.arr" as CL
import "compiler/repl-support.arr" as RS
import "compiler/type-structs.arr" as TS
import "compiler/ast-util.arr" as AU

type Either = E.Either

fun filter-env-by-imports(env :: CS.CompileEnvironment, l :: CL.Locator, g :: CS.Globals) -> CS.Globals:
  cases(A.Program) CL.get-ast(l.get-module(), l.uri()):
    | s-program(_, _, _, imports, _) =>
      for fold(shadow g from g, i from imports):
        cases(A.Import) RN.expand-import(i, env):
          | s-import-complete(_, values, types, file, vals-name, type-name) =>
            mod = env.mods.get-value(AU.import-to-dep(file).key())
            new-vals = for fold(vs from g.values, k from values.map(_.toname())):
              vs.set(k, mod.values.get-value(k))
            end
            new-types = for fold(ts from g.types, k from types.map(_.toname())):
              ts.set(k, mod.aliases.get-value(k))
            end
            CS.globals(
              new-vals.set(vals-name.toname(), TS.t-top),
              new-types.set(type-name.toname(), TS.t-top))
        end
      end
  end
end

fun make-repl<a>(
    runtime :: R.Runtime,
    defs-locator :: CL.Locator,
    compile-context :: a,
    finder :: (a, CS.Dependency -> CL.Located<a>)):

  var nspace = defs-locator.get-namespace(runtime)
  var globals = defs-locator.get-globals()
  var modules = SD.make-mutable-string-dict()
  var current-type-check = false

  fun update-env(result, loc):
    nspace := N.make-namespace-from-result(result)
    cr = L.get-result-compile-result(result)
    globals := filter-env-by-imports(cr.compile-env, loc, globals)
    provided = cr.provides.values.keys-list()
    new-vals = for fold(vs from globals.values, provided-name from provided):
      vs.set(provided-name, cr.provides.values.get-value(provided-name))
    end
    tprovided = cr.provides.aliases.keys-list()
    new-types = for fold(ts from globals.types, provided-name from tprovided):
      ts.set(provided-name, cr.provides.aliases.get-value(provided-name))
    end
    globals := CS.globals(new-vals, new-types)

    #provided = loc.get-provides().values.keys-list()
    #new-vals = for fold(vs from globals.values, provided-name from provided):
    #  vs.set(provided-name, CS.v-just-there)
    #end
    #tprovided = loc.get-provides().types.keys-list()
    #new-types = for fold(ts from globals.types, provided-name from tprovided):
    #  ts.set(provided-name, CS.t-just-there)
    #end
  end

  fun restart-interactions(type-check :: Boolean):
    current-type-check := type-check
    modules := SD.make-mutable-string-dict()
    worklist = CL.compile-worklist(finder, defs-locator, compile-context)
    compiled = CL.compile-program-with(worklist, modules, CS.default-compile-options.{type-check: current-type-check})
    result = CL.run-program(worklist, compiled, runtime, CS.default-compile-options.{type-check: current-type-check})
    globals := defs-locator.get-globals()
    cases(Either) result:
      | right(answer) =>
        when L.is-success-result(answer):
          update-env(answer, defs-locator)
        end
      | left(err) =>
        nothing
    end
    result
  end

  fun run-interaction(repl-locator :: CL.Locator):
    worklist = CL.compile-worklist(finder, repl-locator, compile-context)
    compiled = CL.compile-program-with(worklist, modules, CS.default-compile-options.{type-check: current-type-check})
    result = CL.run-program(worklist, compiled, runtime, CS.default-compile-options.{type-check: current-type-check})
    cases(Either) result:
      | right(answer) =>
        when L.is-success-result(answer):
          update-env(answer, repl-locator)
        end
      | left(err) =>
        nothing
    end
    result
  end

  {
    restart-interactions: restart-interactions,
    run-interaction: run-interaction,
    runtime: runtime,
    get-current-namespace: lam(): nspace end,
    get-current-globals: lam(): globals end
  }
end
