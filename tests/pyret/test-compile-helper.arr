provide *
import either as E
import load-lib as L
import string-dict as SD
import runtime-lib as R
import file("../../src/arr/compiler/compile-lib.arr") as CL
import file("../../src/arr/compiler/cli-module-loader.arr") as CLI
import file("../../src/arr/compiler/compile-structs.arr") as CS

var i = 0
fun string-to-locator(program :: String) block:
  name = "compile-helper-program-" + tostring(i)
  i := i + 1
  {
    method needs-compile(self, provs): true end,
    method get-modified-time(self): 0 end,
    method get-options(self, options): options end,
    method get-module(self): CL.pyret-string(program) end,
    method get-native-modules(self): [list:] end,
    method get-extra-imports(self): CS.standard-imports end,
    method get-dependencies(self): CL.get-standard-dependencies(self.get-module(), self.uri()) end,
    method get-globals(self): CS.standard-globals end,
    method uri(self): "file://" + name end,
    method name(self): name end,
    method set-compiled(self, _, _): nothing end,
    method get-compiled(self): none end,
    method _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
  }
end

fun dfind(ctxt, dep):
  cases(CS.Dependency) dep:
    | builtin(modname) =>
      CLI.module-finder(ctxt, dep)
    | else =>
      CL.located(string-to-locator(dep.arguments.get(0)), ctxt)
  end
end

fun run-to-result(program):
  floc = string-to-locator(program)
  res = CL.compile-and-run-locator(floc, dfind, CLI.default-test-context, L.empty-realm(), R.make-runtime(), [SD.mutable-string-dict:], CS.default-compile-options.{compile-module: true})
  res
end

fun compile-str(program):
  loc = string-to-locator(program)
  wlist = CL.compile-worklist(CLI.module-finder, loc, CLI.default-test-context)
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
  cases(E.Either) compile-str(str):
    | right(ans) =>
      empty
    | left(errs) =>
      errs.map(_.problems).foldr(_ + _, empty)
  end
end

