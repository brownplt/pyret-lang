import load-lib as L
import runtime-lib as RT
import string-dict as SD
import either as E
import file("../../../src/arr/compiler/locators/builtin.arr") as B
import file("../../../src/arr/compiler/repl.arr") as R
import file("../../../src/arr/compiler/compile-structs.arr") as CS

type Either = E.Either

fun get-run-answer(res):
  cases(Either) res block:
    | right(ans) => ans
    | left(err) => 
      print-error("Expected an answer, but got compilation errors:")
      for lists.each(e from err):
        print-error(tostring(e))
      end
  end
end
val = lam(str): L.get-result-answer(get-run-answer(str)) end
msg = lam(str): L.render-error-message(get-run-answer(str)) end

check:
  r = RT.make-runtime()
  var current-defs = "5"
  loc = R.make-repl-definitions-locator(lam(): current-defs end, CS.standard-globals)
  dfind = R.make-definitions-finder([SD.string-dict:], B.make-builtin-locator)
  repl = R.make-repl(r, [SD.mutable-string-dict:], L.empty-realm(), loc, {}, dfind)

  result1 = repl.restart-interactions(false)
  L.get-result-answer(result1.v) is some(5)

  current-defs := "x = 5"
  result2 = repl.restart-interactions(false)
  L.get-result-answer(result2.v) is none

  fun next-interaction(src):
    i = repl.make-interaction-locator(lam(): src end)
    repl.run-interaction(i)
  end

  result3 = next-interaction("y = 10\nx")
  val(result3) is some(5)

  result4 = next-interaction("y")
  val(result4) is some(10)

  result5 = next-interaction("include image")
  result5.v satisfies L.is-success-result

  result6 = next-interaction("is-function(rectangle)")
  val(result6) is some(true)

  current-defs := "import string-dict as SD\nstring-dict = SD.string-dict\n55"
  result7 = repl.restart-interactions(false)
  val(result7) is some(55)

  # should fail because y no longer bound
  result8 = next-interaction("y")
  result8 satisfies E.is-left

  result9 = next-interaction("is-function(string-dict.make)")
  val(result9) is some(true)

  result10 = next-interaction("import string-dict as SD2")
  result10 satisfies E.is-right

  result11 = next-interaction(```
    sd1 :: SD.StringDict = [SD2.string-dict:]
    sd2 = [SD.string-dict:]
    sd1 == sd2
  ```)
  val(result11) is some(true)

  # fails because shadows SD above
  result12 = next-interaction("import string-dict as SD")
  result12 satisfies E.is-left

  current-defs := "import string-dict as SD\nstring-dict = SD.string-dict"
  result13 = repl.restart-interactions(false)
  result13 satisfies E.is-right

  result14 = next-interaction("[string-dict: 'x', 10].get-value('x')")
  val(result14) is some(10)

  result15 = next-interaction("shadow string-dict = 57")
  result15 satisfies E.is-right

  result16 = next-interaction("string-dict")
  val(result16) is some(57)

  result17 = next-interaction("import repl(\"interactions://2\") as I2")
  result17 satisfies E.is-right

  result18 = next-interaction("I2.string-dict")
  val(result18) is some(57)

  result19 = next-interaction("import repl(\"definitions://\") as Defs")
  result19 satisfies E.is-right

  result20 = next-interaction("is-object(Defs.string-dict)")
  val(result20) is some(true)

# TODO(joe): once type-checking in envs works
#|
  current-defs := "import string-dict as SD\nis-object(SD.string-dict)"
  result17 = repl.restart-interactions(true)
  val(result17) is some(true)
|#
end
