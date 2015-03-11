import load-lib as L
import repl as R
import runtime-lib as RT
import string-dict as SD
import "compiler/compile-structs.arr" as CS
import "compiler/repl-support.arr" as RS

check:
  r = RT.make-runtime()
  var current-defs = "5"
  loc = RS.make-repl-definitions-locator("definitions", "pyret://definitions", lam(): current-defs end, CS.minimal-builtins)
  dfind = RS.make-definitions-finder([SD.string-dict:])
  repl = R.make-repl(r, loc, {}, dfind)

  result1 = repl.restart-interactions()
  L.get-result-answer(result1) is some(5)

  current-defs := "x = 5"
  result2 = repl.restart-interactions()
  L.get-result-answer(result2) is none

  interaction1 = RS.make-repl-interaction-locator("interactions1", "pyret://interactions1", lam(): "y = 10\nx" end, repl)
  result3 = repl.run-interaction(interaction1)
  L.get-result-answer(result3) is some(5)

  interaction2 = RS.make-repl-interaction-locator("interactions2", "pyret://interactions2", lam(): "y" end, repl)
  result4 = repl.run-interaction(interaction2)
  L.get-result-answer(result4) is some(10)


end
