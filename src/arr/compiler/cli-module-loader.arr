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

type CLIContext = {
  current-load-path :: List<String>
}

fun add-to-load-path(current-load-path, path):
  split = string-split-all(path, "/")
  print(split)
  print(current-load-path + split.take(split.length() - 1))
end

fun module-finder(ctxt :: CLIContext, dep :: CS.Dependency):
  cases(CS.Dependency) dep:
    | dependency(protocol, args) =>
      if protocol == "file":
        clp = ctxt.current-load-path
        this-path = dep.arguments.get(0)
        real-path = clp.join-str("/") + "/" + this-path
        new-context = ctxt.{current-load-path: add-to-load-path(clp, this-path)}
        CL.located(FL.file-locator(real-path, CS.standard-globals), new-context)
      else if protocol == "legacy-path":
        CL.located(LP.legacy-path-locator(dep.arguments.get(0)), ctxt)
      else:
        raise("Unknown import type: " + protocol)
      end
    | builtin(modname) =>
      CL.located(BL.make-builtin-locator(modname), ctxt)
  end
end

fun get-compiler():
  CL.make-compile-lib(module-finder)
end

fun compile(path, options):
  base-module = CS.dependency("file", [list: path])
  cl = get-compiler()
  base = module-finder({current-load-path: [list: "./"]}, base-module)
  wl = cl.compile-worklist(base.locator, base.context)
  compiled = cl.compile-program(wl, options)
  compiled
end

fun run(path, options):
  base-module = CS.dependency("file", [list: path])
  cl = get-compiler()
  base = module-finder({current-load-path:[list: "./"]}, base-module)
  wl = cl.compile-worklist(base.locator, base.context)
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

