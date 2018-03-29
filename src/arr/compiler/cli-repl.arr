import runtime-lib as RT
import load-lib as L
import string-dict as SD
import file as F
import cmdline-lib as CL

import js-file("./cli-repl") as CR

import file("repl.arr") as R
import file("compile-structs.arr") as CS
import file("cli-module-loader.arr") as CLI

r = RT.make-runtime()

repl = R.make-repl(r, [SD.mutable-string-dict:], L.empty-realm(), CLI.default-start-context, lam(): CLI.module-finder end)
fun restart-interactions(src):
  i = repl.make-definitions-locator(lam(): src end, CS.standard-globals)
  repl.restart-interactions(i, CS.default-compile-options)
end
fun run-interaction(src):
  i = repl.make-interaction-locator(lam(): src end)
  repl.run-interaction(i)
end

all-cmdline-params = CL.command-line-arguments()
file-name = all-cmdline-params.first
other-args = all-cmdline-params.rest

input-file =  cases(List) other-args:
                | empty => ""
                | link(first, rest) => F.file-to-string(first)
              end

CR.start({
  restart-interactions: restart-interactions,
  run-interaction: run-interaction,
  definitions: input-file
})
