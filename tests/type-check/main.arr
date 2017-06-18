#lang pyret

import ast as A
import either as E
import filelib as FL
import runtime-lib as R
import load-lib as L
import string-dict as SD
import file("../../src/arr/compiler/compile-lib.arr") as CL
import file("../../src/arr/compiler/compile-structs.arr") as CS
import file("../../src/arr/compiler/type-defaults.arr") as TD
import file("../../src/arr/compiler/locators/builtin.arr") as BL
import file("../../src/arr/compiler/cli-module-loader.arr") as CLI

fun string-to-locator(name, str :: String):
  {
    method needs-compile(self, provs): true end,
    method get-modified-time(self): 0 end,
    method get-options(self, options): options end,
    method get-module(self): CL.pyret-string(str) end,
    method get-extra-imports(self): CS.minimal-imports end,
    method get-native-modules(self): [list:] end,
    method get-dependencies(self): CL.get-dependencies(self.get-module(), self.uri()) end,
    method get-globals(self): CS.standard-globals end,
    method uri(self): "tc-test://" + name end,
    method name(self): name end,
    method set-compiled(self, ctxt, provs): nothing end,
    method get-compiled(self): none end,
    method _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
  }
end

fun dfind(ctxt, dep):
  l = cases(CS.Dependency) dep:
    | builtin(modname) =>
      BL.make-builtin-locator(modname)
  end
  CL.located(l, nothing)
end

compile-str = lam(filename):
  base-module = CS.dependency("file", [list: filename])
  base = CLI.module-finder(CLI.default-test-context, base-module)
  wlist = CL.compile-worklist(CLI.module-finder, base.locator, base.context)
  result = CL.compile-program(wlist, CS.default-compile-options.{type-check: true})
  errors = result.loadables.filter(CL.is-error-compilation)
  cases(List) errors:
    | empty =>
      E.right(result.loadables)
    | link(_, _) =>
      E.left(errors.map(_.result-printer))
  end
end

fun is-arr-file(filename):
  string-index-of(filename, ".arr") == (string-length(filename) - 4)
end

check "These should all be good programs":
  base = "./tests/type-check/good/"
  good-progs = FL.list-files(base)
  for each(prog from good-progs):
    when is-arr-file(prog) block:
      filename = base + prog
      result = compile-str(filename)
      result satisfies E.is-right
      when E.is-left(result):
        "Should be okay: " is filename
      end
    end
  end
end

check "These should all be bad programs":
  base = "./tests/type-check/bad/"
  bad-progs = FL.list-files(base)
  for each(prog from bad-progs):
    when is-arr-file(prog) block:
      filename  = base + prog
      result = compile-str(filename)
      result satisfies E.is-left
      cases(E.Either) result:
        | right(_) =>
          "Should be error: " is filename
        | left(problems) =>
          for each(problem from problems):
            tostring(problem) satisfies is-string
          end
      end
    end
  end
end

#|
check "All builtins should have a type":
  covered = TD.make-default-types()
  for each(builtin from CS.standard-globals.values.keys-list()):
    builtin-typ = covered.get-now(A.s-global(builtin).key())
    builtin-typ satisfies is-some
    when is-none(builtin-typ):
      "Should have a type: " is builtin
    end
  end
end
|#
