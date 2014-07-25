#lang pyret

import exec as X
import filelib as FL
import "compiler/compile.arr" as CM
import "compiler/compile-structs.arr" as CS

exec-result = lam(result):
  str = result.code.pyret-to-js-runnable()
  X.exec(str, "test", ".", true, "Pyret", [list:])
end
compile-str = lam(str):
  CM.compile-js(
          CM.start,
          "Pyret",
          str,
          "test",
          CS.minimal-builtins,
          {
            check-mode : true,
            allow-shadowed : false,
            collect-all: false,
            type-check: true,
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


check "These should all be good programs":
  base = "./tests/type-check/good/"
  good-progs = FL.list-files(base)
  for each(prog from good-progs):
    prog-file = FL.open-input-file(base + prog)
    prog-text = FL.read-file(prog-file)
    compile-str(prog-text) satisfies CS.is-ok
    FL.close-output-file(prog-file)
  end
end

check "These should all be bad programs":
  base = "./tests/type-check/bad/"
  bad-progs = FL.list-files(base)
  for each(prog from bad-progs):
    prog-file = FL.open-input-file(base + prog)
    prog-text = FL.read-file(prog-file)
    compile-str(prog-text) satisfies CS.is-err
    FL.close-output-file(prog-file)
  end
end
