import load-lib as L
import repl as R
import runtime-lib as RT
import string-dict as SD
import either as E
import "compiler/compile-structs.arr" as CS
import "compiler/repl-support.arr" as RS

type Either = E.Either


check:
  r = RT.make-runtime()
  var current-defs = "5"
  loc = RS.make-repl-definitions-locator("definitions", "pyret://definitions", lam(): current-defs end, CS.standard-globals)
  dfind = RS.make-definitions-finder([SD.string-dict:])
  repl = R.make-repl(r, loc, {}, dfind)

  result1 = repl.restart-interactions()
  L.get-result-answer(result1.v) is some(5)

  current-defs := "x = 5"
  result2 = repl.restart-interactions()
  L.get-result-answer(result2.v) is none

  var ic = 0
  fun next-interaction(src):
    ic := ic + 1
    i = RS.make-repl-interaction-locator("interactions" + tostring(ic), "pyret://interactions" + tostring(ic), lam(): src end, repl)
    repl.run-interaction(i)
  end

  result3 = next-interaction("y = 10\nx")
  print(result3)
  L.get-result-answer(result3.v) is some(5)

  result4 = next-interaction("y")
  L.get-result-answer(result4.v) is some(10)

  result5 = next-interaction("include image")
  result5.v satisfies L.is-success-result

  result6 = next-interaction("is-function(rectangle)")
  cases(Either) result6:
    | right(v) =>
      L.get-result-answer(result6.v) is some(true)
    | left(err) =>
      print(err)
  end

  current-defs := "import string-dict from string-dict\n55"
  result7 = repl.restart-interactions()
  print(result7)
  cases(Either) result7:
    | right(v) =>
      L.get-result-answer(result7.v) is some(55)
    | left(err) =>
      print(err)
  end

  # should fail because y no longer bound
  result8 = next-interaction("y")
  print(result8)
  result8 satisfies E.is-left

  result9 = next-interaction("is-function(string-dict.make)")
  print(result9)
  L.get-result-answer(result9.v) is some(true)

  result10 = next-interaction("import string-dict as SD")
  print(result10)
  result10 satisfies E.is-right

  result11 = next-interaction(```
    sd1 :: SD.StringDict = [string-dict:]
    sd2 = [SD.string-dict:]
    sd1 == sd2
  ```)
  L.get-result-answer(result11.v) is some(true)

  # fails because shadows string-dict from import ... from
  result12 = next-interaction("include string-dict")
  result12 satisfies E.is-left

  current-defs := "include string-dict"
  result13 = repl.restart-interactions()
  result13 satisfies E.is-right

  result14 = next-interaction("[string-dict: 'x', 10].get-value('x')")
  L.get-result-answer(result14.v) is some(10)

  result15 = next-interaction("shadow string-dict = 57")
  result15 satisfies E.is-right

  result16 = next-interaction("string-dict")
  L.get-result-answer(result16.v) is some(57)
end
