#lang pyret

import ast as A
import exec as X
import filelib as FL
import "compiler/compile.arr" as CM
import "compiler/compile-structs.arr" as CS
import "compiler/type-defaults.arr" as TD

exec-result = lam(result):
  str = result.code.pyret-to-js-runnable()
  X.exec(str, "test", ".", true, "Pyret", [list:])
end
compile-str = lam(filename, str):
  CM.compile-js(
          CM.start,
          "Pyret",
          str,
          filename,
          CS.minimal-builtins,
          {
            check-mode : true,
            allow-shadowed : false,
            collect-all: false,
            type-check: true,
            show-steps: false,
            ignore-unbound: false
          }
          ).result
end
run-str = lam(str):
  compiled = compile-str(str)
  cases(CS.CompileResult) compiled:
    | ok(code) => exec-result(compiled)
    | err(errs) => raise("Compilation failure when a run was expected " + torepr(errs) + "\n Program was:\n " + str)
  end
end

fun is-arr-file(filename):
  string-index-of(filename, ".arr") == (string-length(filename) - 4)
end


check "These should all be good programs":
  base = "./tests/type-check/good/"
  good-progs = FL.list-files(base)
  for each(prog from good-progs):
    when is-arr-file(prog):
      filename  = base + prog
      prog-file = FL.open-input-file(filename)
      prog-text = FL.read-file(prog-file)
      result = compile-str(filename, prog-text)
      result satisfies CS.is-ok
      when CS.is-err(result):
        "Should be okay: " is filename
      end
      FL.close-output-file(prog-file)
    end
  end
end

check "These should all be bad programs":
  base = "./tests/type-check/bad/"
  bad-progs = FL.list-files(base)
  for each(prog from bad-progs):
    when is-arr-file(prog):
      filename  = base + prog
      prog-file = FL.open-input-file(filename)
      prog-text = FL.read-file(prog-file)
      result    = compile-str(filename, prog-text)
      result satisfies CS.is-err
      cases(CS.CompileResult) result:
        | ok(_) =>
          "Should be error: " is filename
        | err(problems) =>
          for each(problem from problems):
            tostring(problem) satisfies is-string
          end
      end
      FL.close-output-file(prog-file)
    end
  end
end

check "All builtins should have a type":
  covered = TD.make-default-typs()
  for each(builtin from CS.runtime-builtins):
    builtin-typ = covered.get-now(A.s-global(builtin.id).key())
    builtin-typ satisfies is-some
    when is-none(builtin-typ):
      "Should have a type: " is builtin
    end
  end
end
