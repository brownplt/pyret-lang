provide *
import namespace-lib as N
import runtime-lib as R
import load-lib as L
import either as E
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CS
import "compiler/locators/file.arr" as FL
import "compiler/locators/legacy-path.arr" as LP
import "compiler/locators/builtin.arr" as BL

type Either = E.Either

fun module-finder(ctxt, dep :: CS.Dependency):
  cases(CS.Dependency) dep:
    | dependency(protocol, args) =>
      if protocol == "file":
        FL.file-locator(dep.arguments.get(0), CS.standard-globals)
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

fun compile(path, options):
  base-module = CS.dependency("file", [list: path])
  cl = get-compiler()
  base = module-finder({}, base-module)
  wl = cl.compile-worklist(base, {})
  compiled = cl.compile-program(wl, options)
  compiled
end

fun run(path, options):
  base-module = CS.dependency("file", [list: path])
  cl = get-compiler()
  base = module-finder({}, base-module)
  wl = cl.compile-worklist(base, {})
  r = R.make-runtime()
  result = CL.compile-and-run-worklist(cl, wl, r, options)
  cases(Either) result:
    | right(answer) =>
      if L.is-success-result(answer):
        print(L.render-check-results(answer))
      else:
        print(L.render-error-message(answer))
        raise("There were execution errors")
      end
      result
    | left(errors) =>
      print-error("Compilation errors:")
      for lists.each(e from errors):
        print-error(tostring(e))
      end
      raise("There were compilation errors")
  end
end

