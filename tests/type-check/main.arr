#lang pyret

import ast as A
import either as E
import filelib as FL
import namespace-lib as N
import runtime-lib as R
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CS
import "compiler/type-defaults.arr" as TD
import "compiler/locators/builtin.arr" as BL
import "compiler/cli-module-loader.arr" as CLI

fun string-to-locator(name, str :: String):
  {
    needs-compile(self, provs): true end,
    get-module(self): CL.pyret-string(str) end,
    get-extra-imports(self): CS.minimal-imports end,
    get-dependencies(self): CL.get-dependencies(self.get-module(), self.uri()) end,
    get-provides(self): CL.get-provides(self.get-module(), self.uri()) end,
    get-globals(self): CS.standard-globals end,
    get-namespace(self, runtime): N.make-base-namespace(runtime) end,
    uri(self): "tc-test://" + name end,
    name(self): name end,
    set-compiled(self, ctxt, provs): nothing end,
    get-compiled(self): none end,
    _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
  }
end

fun dfind(ctxt, dep):
  l = cases(CS.Dependency) dep:
    | builtin(modname) =>
      BL.make-builtin-locator(modname)
  end
  CL.located(l, nothing)
end

compile-str = lam(filename, str):
  l = string-to-locator(filename, str)
  wlist = CL.compile-worklist(dfind, l, {})
  result = CL.compile-program(wlist, CS.default-compile-options.{type-check: true})
  errors = result.loadables.filter(CL.is-error-compilation)
  cases(List) errors:
    | empty =>
      E.right(result.loadables)
    | link(_, _) =>
      E.left(errors.map(_.result-printer))
  end
end

fun compile-program(path):
  base-module = CS.dependency("file", [list: path])
  base = CLI.module-finder({current-load-path:[list: "./"]}, base-module)
  wl = CL.compile-worklist(CLI.module-finder, base.locator, base.context)
  r = R.make-runtime()
  CL.compile-and-run-worklist(wl, r, CS.default-compile-options.{type-check: true})
end

fun is-arr-file(filename):
  string-index-of(filename, ".arr") == (string-length(filename) - 4)
end

check "These should all be good programs":
  base = "./tests/type-check/good/"
  good-progs = FL.list-files(base)
  for each(prog from good-progs):
    when is-arr-file(prog):
      filename = base + prog
      result = compile-program(filename)
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
    when is-arr-file(prog):
      filename  = base + prog
      result = compile-program(filename)
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

#check "These should all be good programs":
#  base = "./tests/type-check/good/"
#  good-progs = FL.list-files(base)
#  for each(prog from good-progs):
#    when is-arr-file(prog):
#      filename  = base + prog
#      prog-file = FL.open-input-file(filename)
#      prog-text = FL.read-file(prog-file)
#      result = compile-str(filename, prog-text)
#      result satisfies E.is-right
#      when E.is-left(result):
#        "Should be okay: " is filename
#      end
#      FL.close-output-file(prog-file)
#    end
#  end
#end

#check "These should all be bad programs":
#  base = "./tests/type-check/bad/"
#  bad-progs = FL.list-files(base)
#  for each(prog from bad-progs):
#    when is-arr-file(prog):
#      filename  = base + prog
#      prog-file = FL.open-input-file(filename)
#      prog-text = FL.read-file(prog-file)
#      result    = compile-str(filename, prog-text)
#      result satisfies E.is-left
#      cases(E.Either) result:
#        | right(_) =>
#          "Should be error: " is filename
#        | left(problems) =>
#          for each(problem from problems):
#            tostring(problem) satisfies is-string
#          end
#      end
#      FL.close-output-file(prog-file)
#    end
#  end
#end

check "All builtins should have a type":
  covered = TD.make-default-typs()
  for each(builtin from CS.standard-globals.values.keys-list()):
    builtin-typ = covered.get-now(A.s-global(builtin).key())
    builtin-typ satisfies is-some
    when is-none(builtin-typ):
      "Should have a type: " is builtin
    end
  end
end
