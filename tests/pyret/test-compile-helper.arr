provide *
import either as E
import load-lib as L
import string-dict as SD
import runtime-lib as R
import js-file("../../src/arr/compiler/ts-pathlib") as P
import js-file('../../src/arr/compiler/ts-render-error-display') as RED
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
    method get-extra-imports(self): CS.minimal-imports end,
    method get-dependencies(self): CL.get-standard-dependencies(self.get-module(), self.uri()) end,
    method get-globals(self): CS.standard-globals end,
    method uri(self): "file://" + name end,
    method name(self): self.uri() end,
    method set-compiled(self, _, _): nothing end,
    method get-compiled(self, options): CL.arr-file(self.get-module(), self.get-extra-imports(), self.get-options(options)) end,
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

test-compile-options = CS.make-default-compile-options('./').{
  compile-module: true,
  builtin-js-dirs: [list: "./build/runtime"],
  runtime-builtin-relative-path: some("./"),
}

our-test-context = CLI.default-test-context.{options: test-compile-options}

fun run-to-result(program):
  floc = string-to-locator(program)
  res = CL.compile-and-run-locator(floc, dfind, our-test-context, L.empty-realm(), R.make-runtime(), [SD.mutable-string-dict:], test-compile-options)
  res
end

fun run-to-result-named(program, name):
  floc = string-to-named-locator(program, name)
  res = CL.compile-and-run-locator(floc, dfind, our-test-context, L.empty-realm(), R.make-runtime(), [SD.mutable-string-dict:], test-compile-options)
  res
end

fun run-to-result-typed(loc, base-path):
  res = CL.compile-and-run-locator(loc, CLI.module-finder, make-base-path-context(base-path), L.empty-realm(), R.make-runtime(), [SD.mutable-string-dict:], test-compile-options.{type-check: true})
  res
end

fun make-base-path-context(base-path):
  {current-load-path: P.resolve(base-path),
   cache-base-dir: P.resolve("./tests/compiled"),
   compiled-read-only-dirs: P.resolve("./tests/compiled"),
   options: test-compile-options}
end

fun compile-str(program):
  loc = string-to-locator(program)
  wlist = CL.compile-worklist(CLI.module-finder, loc, our-test-context)
  result = CL.compile-program(wlist, test-compile-options)
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

fun contract-error-contains(check-err):
  { success: false,
    runtime: true,
    check-errors:
      lam(errors):
        msg = L.render-error-message(errors).message
        string-contains(msg, check-err)
      end
  }
end
  

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

fun compile-error-messages(str):
  for lists.map(err from get-compile-errs(str)):
    RED.display-to-string(err.render-reason(), torepr, empty)
  end
end
