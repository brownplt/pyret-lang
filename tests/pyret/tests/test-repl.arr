import load-lib as L
import runtime-lib as RT
import string-dict as SD
import either as E
import pathlib as P
import file("../../../src/arr/compiler/locators/builtin.arr") as B
import file("../../../src/arr/compiler/repl.arr") as R
import file("../../../src/arr/compiler/compile-structs.arr") as CS
import file("../../../src/arr/compiler/cli-module-loader.arr") as CLI

print("Running repl-tests: " + tostring(time-now()) + "\n")

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

fun startswith(hay, needle):
  needle-len = string-length(needle)
  hay-len = string-length(hay)
  (needle-len <= hay-len) and
  string-equal(string-substring(hay, 0, needle-len), needle)
end

check:
  r = RT.make-runtime()

  repl = R.make-repl(r, [SD.mutable-string-dict:], L.empty-realm(), CLI.default-test-context, lam(): CLI.module-finder end)
  fun restart(src, type-check):
    i = repl.make-definitions-locator(lam(): src end, CS.standard-globals)
    repl.restart-interactions(i, CS.default-compile-options.{type-check: type-check})
  end
  fun next-interaction(src):
    i = repl.make-interaction-locator(lam(): src end)
    repl.run-interaction(i)
  end

  result1 = restart("5", false)
  L.get-result-answer(result1.v) is some(5)

  result2 = restart("x = 5", false)
  L.get-result-answer(result2.v) is none

  result3 = next-interaction("y = 10\nx")
  val(result3) is some(5)

  result4 = next-interaction("y")
  val(result4) is some(10)

  result5 = next-interaction("include string-dict")
  result5.v satisfies L.is-success-result

  result6 = next-interaction("is-function(make-string-dict)")
  val(result6) is some(true)

  importsd = "import string-dict as SD\nstring-dict = SD.string-dict\n55"
  result7 = restart(importsd, false)
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

  importbindsd = "import string-dict as SD\nstring-dict = SD.string-dict"
  result13 = restart(importbindsd, false)
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

  result21 = restart("x :: Number = 5\nx", true)
  val(result21) is some(5)

  result22 = next-interaction("fun f() -> String: x end")
  result22 satisfies E.is-left

  result23 = next-interaction("fun g() -> Number: x end\ng()")
  val(result23) is some(5)

  result24 = restart("{x; y} = {1; 2}\nx", false)
  val(result24) is some(1)

  result25 = next-interaction("x + y")
  val(result25) is some(3)

  # try nested tuples and make sure all the bindings appear in the REPL
  result26 = restart("{{a; b}; {c; d; e} as f; {g; h}} = {{1; 2}; {3; 4; {5; 6}}; {{7; 8}; 9}}\na", false)
  val(result26) is some(1)

  result27 = next-interaction("a + e.{1} + f.{0} + g.{1} + h")
  val(result27) is some(27)

  result28 = restart("var var-to-change-at-repl = 7", false)
  result29 = next-interaction("var-to-change-at-repl := 5\nvar-to-change-at-repl")
  val(result29) is some(5)
  result30 = next-interaction("var-to-change-at-repl")
  val(result30) is some(5)
  result31 = next-interaction("fun f(): var-to-change-at-repl end")
  result32 = next-interaction("var-to-change-at-repl := 22")
  result33 = next-interaction("f()")
  val(result33) is some(22)

  # Make sure a stack 3 levels deep works
  result34 = restart("fun f(): 9() end", false)
  result35 = next-interaction("fun g(): f() end")
  result36 = next-interaction("g()")
  result36.v satisfies L.is-failure-result
  L.get-result-stacktrace(result36.v) is=~
  [raw-array:
    "definitions://: line 1, column 9",
    "interactions://1: line 1, column 9",
    "interactions://2: line 1, column 0"]

  # Call a bunch of flat functions
  result37 = restart("fun f(o): o.x end\n" +
                     "fun g(): f({}) end\n" +
                     "fun h(): f({x: \"a\"}) end\n" +
                     "fun j(): string-append(g(), h()) end", false)
  result38 = next-interaction("j()")
  result38.v satisfies L.is-failure-result
  L.get-result-stacktrace(result38.v) is=~
  [raw-array:
    "definitions://: line 1, column 10",
    "definitions://: line 2, column 9",
    "definitions://: line 4, column 23",
    "interactions://1: line 1, column 0"]

  #Call a tail-recursive function that has an error at the deepest level
  result39 = restart("fun len(l, acc):\n" +
  "  cases (List) l:\n" +
  "    | empty => l.notafield\n" +
  "    | link(_, r) => len(r, 1 + acc)\n" +
  "  end\n" +
  "end",
  false)
  result40 = next-interaction("len(range(0, 10), 0)")
  stacktrace = L.get-result-stacktrace(result40.v)

  # Due to tail call optimization, the stack trace may not have any of the
  # "middle" frames, though it should definitely have the topmost and bottom
  # most frames.
  raw-array-get(stacktrace, 0) is "definitions://: line 3, column 15"
  raw-array-get(stacktrace,
    raw-array-length(stacktrace) - 1) is "interactions://1: line 1, column 0"

  result41 = restart("fun f(o): o.x end\n" +
                     "fun g(): f(5)\n end", false)
  result42 = next-interaction("g()")
  result42.v satisfies L.is-failure-result
  L.get-result-stacktrace(result42.v) is=~
  [raw-array:
    "definitions://: line 1, column 10",
    "definitions://: line 2, column 9",
    "interactions://1: line 1, column 0"]

  #Method call test
  result43 = restart("fun f(o): o.x() end\n" +
                     "fun g(): f({x: 5})\n end", false)
  result44 = next-interaction("g()")
  result44.v satisfies L.is-failure-result
  L.get-result-stacktrace(result44.v) is=~
  [raw-array:
    "definitions://: line 1, column 10",
    "definitions://: line 2, column 9",
    "interactions://1: line 1, column 0"]

  # stacktrace through list.map()
  result45 = restart("fun f():\n" +
    "h = lam(x): 9() end\n" +
    "[list: 1, 2, 3].map(h)\n" +
    "end", false)
  result46 = next-interaction("f()")
  result46.v satisfies L.is-failure-result
  stacktrace46 = L.get-result-stacktrace(result46.v)

  raw-array-get(stacktrace46, 0) is "definitions://: line 2, column 12"
  # Don't check the actual line number in the builtin:lists
  startswith(raw-array-get(stacktrace46, 1), "builtin://lists:")
  raw-array-get(stacktrace46, 2) is "definitions://: line 3, column 0"
  raw-array-get(stacktrace46, 3) is "interactions://1: line 1, column 0"

  # stacktrace through builtin raw-list-map
  result47 = restart("fun f():\n" +
    "h = lam(x): x.somefield end\n" +
    "builtins.raw-list-map(h, [list: 1, 2, 3])\n" +
    "end", false)
  result48 = next-interaction("f()")
  result48.v satisfies L.is-failure-result
  L.get-result-stacktrace(result48.v) is=~
  [raw-array:
    "definitions://: line 2, column 12",
    "definitions://: line 3, column 0",
    "interactions://1: line 1, column 0"]


  result49 = restart("fun sum(x):\n" +
    "if x == 0:\n" +
    "  9()\n" +
    "else:\n" +
    "  x + sum(x - 1)\n" +
    "end\n" +
  "end", false)

  # Should be plenty enough to bounce
  ncalls = 1000
  result50 = next-interaction("sum(" + tostring(ncalls) + ")")
  result50.v satisfies L.is-failure-result
  stacktrace50-list = raw-array-to-list(L.get-result-stacktrace(result50.v))

  stacktrace50-list is
  [list: "definitions://: line 3, column 2"] +
  repeat(ncalls, "definitions://: line 5, column 6") +
  [list: "interactions://1: line 1, column 0"]

end
