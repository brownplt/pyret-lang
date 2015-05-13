
provide { make-repl: make-repl } end
import runtime-lib as R
import namespace-lib as N
import string-dict as SD
import "compiler/compile-structs.arr" as CS
import "compiler/compile-lib.arr" as CL
import "compiler/repl-support.arr" as RS


fun make-repl(
    runtime :: R.Runtime,
    defs-locator :: CL.Locator,
    compile-context :: CL.CompileContext,
    finder :: (CL.CompileContext, CS.Dependency -> CL.Locator)):

  var nspace = defs-locator.get-namespace(runtime)
  var compile-env = defs-locator.get-compile-env()

  compile-lib = CL.make-compile-lib(finder)

  fun update-env(result, loc):
    nspace := nspace.merge-all(result)
    provided = loc.get-provides().to-list()
    for each(provided-name from provided):
      # TODO(joe): Also need to add _type_ bindings for the repl here
      compile-env := CS.compile-env(link(CS.builtin-id(provided-name), compile-env.bindings), compile-env.types)
    end
  end

  fun restart-interactions():
    worklist = compile-lib.compile-worklist(defs-locator, compile-context)
    result = CL.compile-and-run-worklist(compile-lib, worklist, runtime, CS.default-compile-options)
    update-env(result, defs-locator)
    result
  end

  fun run-interaction(repl-locator :: CL.Locator):
    worklist = compile-lib.compile-worklist(repl-locator, compile-context)
    result = CL.compile-and-run-worklist(compile-lib, worklist, runtime, CS.default-compile-options)
    update-env(result, repl-locator)
    result
  end

  {
    restart-interactions: restart-interactions,
    run-interaction: run-interaction,
    runtime: runtime,
    get-current-namespace: lam(): nspace end,
    get-current-compile-env: lam(): compile-env end
  }
end
