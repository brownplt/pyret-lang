
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

type Either = E.Either

fun filter-env-by-imports(env :: CS.CompileEnvironment, l :: CL.Locator, g :: CS.Globals) -> CS.Globals:
  cases(A.Program) CL.get-ast(l.get-module(), l.uri()):
    | s-program(_, _, _, imports, _) =>
      for fold(shadow g from g, i from imports):
        cases(A.Import) RN.expand-import(i, env):
          | s-import-complete(_, values, types, mode, vals-name, type-name) =>
            new-vals = for fold(vs from g.values, k from values.map(_.toname())):
              vs.set(k, CS.v-just-there)
            end
            new-types = for fold(ts from g.types, k from types.map(_.toname())):
              ts.set(k, CS.t-just-there)
            end
            CS.globals(
              new-vals.set(vals-name.toname(), CS.v-just-there),
              new-types.set(type-name.toname(), CS.t-just-there))
        end
      end
  end
end

fun make-repl(
    runtime :: R.Runtime,
    defs-locator :: CL.Locator,
    compile-context :: CL.CompileContext,
    finder :: (CL.CompileContext, CS.Dependency -> CL.Locator)):

  var nspace = defs-locator.get-namespace(runtime)
  var globals = defs-locator.get-globals()

  compile-lib = CL.make-compile-lib(finder)

  fun update-env(result, loc):
    nspace := N.make-namespace-from-result(result)
    globals := filter-env-by-imports(L.get-result-compile-env(result), loc, globals)
    provided = loc.get-provides().values.keys-list()
    new-vals = for fold(vs from globals.values, provided-name from provided):
      vs.set(provided-name, CS.v-just-there)
    end
    tprovided = loc.get-provides().types.keys-list()
    new-types = for fold(ts from globals.types, provided-name from tprovided):
      ts.set(provided-name, CS.t-just-there)
    end
    globals := CS.globals(new-vals, new-types)
  end

  fun restart-interactions():
    worklist = compile-lib.compile-worklist(defs-locator, compile-context)
    result = CL.compile-and-run-worklist(compile-lib, worklist, runtime, CS.default-compile-options)
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
    worklist = compile-lib.compile-worklist(repl-locator, compile-context)
    result = CL.compile-and-run-worklist(compile-lib, worklist, runtime, CS.default-compile-options)
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
