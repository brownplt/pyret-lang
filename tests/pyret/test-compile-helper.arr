provide *
import either as E
import load-lib as L
import string-dict as SD
import runtime-lib as R
import pathlib as P
import file("../../src/arr/compiler/compile-lib.arr") as CL
import file("../../src/arr/compiler/cli-module-loader.arr") as CLI
import file("../../src/arr/compiler/compile-structs.arr") as CS

fun string-to-named-locator(program :: String, name :: String):
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

var i = 0
fun string-to-locator(program :: String) block:
  name = "compile-helper-program-" + tostring(i)
  i := i + 1
  string-to-named-locator(program, name)
end

fun dfind(ctxt, dep):
  cases(CS.Dependency) dep block:
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

fun run-to-result-named(program, name):
  floc = string-to-named-locator(program, name)
  res = CL.compile-and-run-locator(floc, dfind, CLI.default-test-context, L.empty-realm(), R.make-runtime(), [SD.mutable-string-dict:], CS.default-compile-options.{compile-module: true})
  res
end

fun run-to-result-typed(loc, base-path):
  res = CL.compile-and-run-locator(loc, CLI.module-finder, make-base-path-context(base-path), L.empty-realm(), R.make-runtime(), [SD.mutable-string-dict:], CS.default-compile-options.{compile-module: true, type-check: true})
  res
end

fun make-base-path-context(base-path):
  {current-load-path: P.resolve(base-path), cache-base-dir: P.resolve("./tests/compiled")}
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

fun get-compile-result(str):
  cases(E.Either) compile-str(str):
    | right(loadables) =>
      loadables.last()
    | left(errs) =>
      raise("Unexpected compile error for " + str + "\n" + to-repr(errs))
  end
end

fun run-str(str): 
  result = run-to-result(str)
  cases(E.Either) result block:
    | left(err) =>
      { program: str, success: false, runtime: false, errors: err }
    | right(ans) =>
      if L.is-success-result(ans):
        { program: str, success: true, runtime: true, ans: ans }
      else:
        { program: str, success: false, runtime: true, errors: ans }
      end
  end
end

fun output(act, exp):
  if exp.success:
    act.success
  else:
    (exp.runtime == act.runtime) and exp.check-errors(act.errors)
  end
end

success = { success: true, runtime: true }

fun check-contract-error(errors):
  string-contains(L.render-error-message(errors).message, "annotation")
end

contract-error = { success: false, runtime: true, check-errors: check-contract-error }

fun field-error(fields):
  { success: false,
    runtime: true,
    check-errors: lam(errors):
        msg = L.render-error-message(errors).message
        for lists.all(f from fields):
          string-contains(msg, "field `" + f + "`")
        end
      end
  }
end

fun compile-error(check-err):
  {
    success: false,
    runtime: false,
    check-errors:
      lam(errors):
        for lists.any(err from errors):
          lists.any(check-err, err.problems)
        end
      end
  }
end
