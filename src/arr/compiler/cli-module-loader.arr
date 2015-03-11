provide *
import namespace-lib as N
import runtime-lib as R
import load-lib as L
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CS
import "compiler/locators/file.arr" as FL
import "compiler/locators/legacy-path.arr" as LP
import "compiler/locators/builtin.arr" as BL

fun module-finder(ctxt, dep :: CS.Dependency):
  cases(CS.Dependency) dep:
    | dependency(protocol, args) =>
      if protocol == "file":
        FL.file-locator(dep.arguments.get(0), CS.standard-builtins)
      else if protocol == "legacy-path":
        LP.legacy-path-locator(dep.arguments.get(0))
      else:
        raise("Unknown import type: " + protocol)
      end
    | builtin(modname) =>
      BL.make-builtin-locator(modname)
  end
end

fun get-compiler():
  CL.make-compile-lib(module-finder)
end

fun compile(path):
  base-module = CS.dependency("file", [list: path])
  cl = get-compiler()
  base = module-finder({}, base-module)
  wl = cl.compile-worklist(base, {})
  compiled = cl.compile-program(wl)
  compiled
end

fun run(path):
  base-module = CS.dependency("file", [list: path])
  cl = get-compiler()
  base = module-finder({}, base-module)
  wl = cl.compile-worklist(base, {})
  r = R.make-runtime()
  result = CL.compile-and-run-worklist(cl, wl, r)
  if L.is-success-result(result):
    print(L.render-check-results(result))
  else:
    print(L.render-error-message(result))
  end
  result
end

