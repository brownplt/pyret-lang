provide *
import either as E
import load-lib as L
import string-dict as SD
import runtime-lib as R
import pathlib as P
import render-error-display as RED
import file("../../../src/arr/compiler/compile-lib.arr") as CL
import file("../../../src/arr/compiler/cli-module-loader.arr") as CLI
import file("../../../src/arr/compiler/compile-structs.arr") as CS

fun make-fresh-module-testing-context():
  modules = SD.make-mutable-string-dict()
  fun name-to-locator(name :: String):
    {
      method needs-compile(self, provs): true end,
      method get-module(self): CL.pyret-string(modules.get-value-now(name)) end,
      method get-extra-imports(self): CS.standard-imports end,
      method get-modified-time(self): 0 end,
      method get-options(self, options): options end,
      method get-native-modules(self): empty end,
      method get-dependencies(self): CL.get-standard-dependencies(self.get-module(), self.uri()) end,
      method get-globals(self): CS.standard-globals end,
      method uri(self): "file://" + name end,
      method name(self): name end,
      method set-compiled(self, ctxt, provs): nothing end,
      method get-compiled(self): none end,
      method _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
    }
  end

  fun dfind(ctxt, dep):
    cases(CS.Dependency) dep block:
      | builtin(modname) =>
        CLI.module-finder(ctxt, dep)
      | else =>
        CL.located(name-to-locator(dep.arguments.get(0)), ctxt)
    end
  end


  fun compile-mod(name):
    loc = name-to-locator(name)
    wlist = CL.compile-worklist(dfind, loc, CLI.default-test-context)
    spy: wlist: wlist.map(lam(x): x.locator.uri() end) end
    result = CL.compile-program(wlist, CS.default-compile-options)
    errors = result.loadables.filter(CL.is-error-compilation)
    cases(List) errors:
      | empty =>
        E.right(result.loadables)
      | link(_, _) =>
        E.left(errors.map(_.result-printer))
    end
  end

  fun get-compile-errs(str):
    cases(E.Either) compile-mod(str):
      | right(ans) =>
        empty
      | left(errs) =>
        errs.map(_.problems).foldr(_ + _, empty)
    end
  end

  fun compile-error-messages(str):
    for lists.map(err from get-compile-errs(str)):
      RED.display-to-string(err.render-reason(), torepr, empty)
    end
  end

  {
    add-module: lam(name, program-str): modules.set-now(name, program-str) end,
    dfind: dfind,
    name-to-locator: name-to-locator,
    compile-mod: compile-mod,
    get-compile-errs: get-compile-errs,
    compile-error-messages: compile-error-messages
  }
end

fun error-with(errs, str):
  lists.any(lam(x): string-contains(x, str) end, errs)
end

check:
  m = make-fresh-module-testing-context()
  m.add-module("a", ```
provide:
  * hiding (j, pos2d),
end
data MyPosn:
  | pos2d(x, y)
  | pos3d(x, y, z)
end
  ```)

  errs = m.compile-error-messages("a")   
  errs is%(error-with) "j"
end


