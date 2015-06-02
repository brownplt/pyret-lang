
provide { make-repl: make-repl } end
import either as E
import load-lib as L
import namespace-lib as N
import string-dict as SD
import runtime-lib as R
import "compiler/compile-structs.arr" as CS
import "compiler/compile-lib.arr" as CL
import "compiler/repl-support.arr" as RS

type Either = E.Either


fun make-repl(
    runtime :: R.Runtime,
    defs-locator :: CL.Locator,
    compile-context :: CL.CompileContext,
    finder :: (CL.CompileContext, CS.Dependency -> CL.Locator)):

  var nspace = defs-locator.get-namespace(runtime)
  var globals = defs-locator.get-globals()

  compile-lib = CL.make-compile-lib(finder)

  fun update-env(result, loc):
    nspace := nspace.merge-all(result)
    provided = loc.get-provides().values.keys-list()
    for each(provided-name from provided):
      # TODO(joe): Also need to add _type_ bindings for the repl here
      globals := CS.globals(globals.values.set(provided-name, CS.v-just-there), globals.types)
    end
  end

  fun restart-interactions():
    worklist = compile-lib.compile-worklist(defs-locator, compile-context)
    result = CL.compile-and-run-worklist(compile-lib, worklist, runtime, CS.default-compile-options)
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
          update-env(answer, defs-locator)
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
