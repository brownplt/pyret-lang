import load-lib as L
import runtime-lib as RT
import string-dict as SD
import either as E
import pathlib as P
import render-error-display as RED
import file("../../../src/arr/compiler/locators/builtin.arr") as B
import file("../../../src/arr/compiler/repl.arr") as R
import file("../../../src/arr/compiler/compile-structs.arr") as CS
import file("../../../src/arr/compiler/cli-module-loader.arr") as CLI

print("Running repl-tests: " + tostring(time-now()) + "\n")

type Either = E.Either

fun val(res):
  cases(Either) res block:
    | right(ans) => L.get-result-answer(ans)
    | left(err) =>
      print-error("Expected an answer, but got compilation errors:")
      for lists.each(e from err):
        print-error(tostring(e))
      end
      nothing
  end
end
fun msgs(res) block:
  cases(Either) res block:
    | right(ans) =>
      L.render-error-message(ans).message
    | left(shadow res) =>
      sep = "\n========================\n"
      for map(r from res):
        cases(CS.CompileResult) r:
          | ok(code) => tostring(code)
          | err(problems) =>
            for map(p from problems):
              RED.display-to-string(p.render-reason(), torepr, empty)
            end.join-str("sep")
        end
      end.join-str(sep)
  end
end

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

check:
  result = restart("5", false)
  L.get-result-answer(result.v) is some(5)
end

check:
  result = restart("x = 5", false)
  L.get-result-answer(result.v) is none

  result2 = next-interaction("y = 10\nx")
  val(result2) is some(5)

  result3 = next-interaction("y")
  val(result3) is some(10)

  result4 = next-interaction("include string-dict")
  result4.v satisfies L.is-success-result

  result5 = next-interaction("is-function(make-string-dict)")
  val(result5) is some(true)
end

check:
  prog = ```
         import string-dict as SD
         include from SD:
         string-dict as sd
         end
         ```
  result = restart(prog, false)
  L.get-result-answer(result.v) is none

  result2 = next-interaction("sd")
  result2 satisfies E.is-right

  result3 = next-interaction("sd = 5")
  result3 satisfies E.is-left
  msg3 = msgs(result3)
  msg3 is%(string-contains) "declaration of `sd` at "
  msg3 is%(string-contains) "imported from" # TODO: update this text when the error message is fixed
  
end

check:
  result = restart("import some from option", false)
  L.get-result-answer(result.v) is none

  result2 = next-interaction("some = 5")
  result2 satisfies E.is-left
  msg2 = msgs(result2)
  msg2 is%(string-contains) "declaration of `some` at "
  msg2 is%(string-contains) "shadows a previous declaration of `some` defined at builtin://option"
  msg2 is%(string-contains) "and imported from"

  result3 = restart("some = 5", false)
  result3 satisfies E.is-left
  msg3 = msgs(result3)
  msg3 is%(string-contains) "declaration of `some` at"
  msg3 is%(string-contains) "shadows a previous declaration of `some` defined at builtin://option"
  msg3 is-not%(string-contains) "and imported from"

  result4 = restart("include ast", false)
  L.get-result-answer(result4.v) is none

  result5 = next-interaction("s-program = 5")
  result5 satisfies E.is-left
  msg5 = msgs(result5)
  msg5 is%(string-contains) "declaration of `s-program` at "
  msg5 is%(string-contains) "shadows a previous declaration of `s-program` defined at builtin://ast"
  msg5 is%(string-contains) "and imported from"
end

check:
  importsd = "import string-dict as SD\nstring-dict = SD.string-dict\n55"
  result = restart(importsd, false)
  result satisfies E.is-right
  val(result) is some(55)

  # should fail because y no longer bound
  result2 = next-interaction("y")
  result2 satisfies E.is-left

  result3 = next-interaction("is-function(string-dict.make)")
  result3 satisfies E.is-right
  val(result3) is some(true)

  result4 = next-interaction("import string-dict as SD2")
  result4 satisfies E.is-right

  result5 = next-interaction(```
    sd1 :: SD.StringDict = [SD2.string-dict:]
    sd2 = [SD.string-dict:]
    sd1 == sd2
  ```)
  val(result5) is some(true)

  # fails because shadows SD above
  result6 = next-interaction("import string-dict as SD")
  result6 satisfies E.is-left

  result7 = next-interaction(```
    include from SD:
      mutable-string-dict,
      type MutableStringDict
    end
    msd1 :: SD.MutableStringDict = [mutable-string-dict:]
    msd2 :: MutableStringDict = [SD2.mutable-string-dict:]
    msd1 =~ msd2
  ```)
  val(result7) is some(true)

end

check "include-from at repl":
  new-import-sd = ```
  import string-dict as SD
  include from SD:
  string-dict as sd,
  type StringDict as StrD
  end
  ```
  result1 = restart(new-import-sd, false)
  result1 satisfies E.is-right

  result2 = next-interaction("sd")
  result2 satisfies E.is-right

  result3 = next-interaction("s :: StrD = [sd:]")
  result3 satisfies E.is-right
end

check:
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

  raw-array-length(stacktrace46) is 5
  raw-array-get(stacktrace46, 0) is "definitions://: line 2, column 12"
  # Don't check the actual line number in the builtin:lists
  raw-array-get(stacktrace46, 1) is%(string-starts-with) "builtin://lists:"
  raw-array-get(stacktrace46, 2) is%(string-starts-with) "builtin://lists:"
  raw-array-get(stacktrace46, 3) is "definitions://: line 3, column 0"
  raw-array-get(stacktrace46, 4) is "interactions://1: line 1, column 0"

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

  # Make sure the repl has standard-imports like lists and arrays

  result51 = next-interaction("lists.sort([list: 3, 2, 1]) == [list: 1, 2, 3]")
  val(result51) is some(true)

  result52 = next-interaction("sets.any(lam(x): x == 0 end, [set: 1, 2, 3])")
  val(result52) is some(false)

  result53 = next-interaction("sets.empty-set == [list-set: ]")
  val(result53) is some(true)

  # Make sure the repl can import built-in typed modules

  tc-result = restart("import string-dict as SD", true)
  tc-result.v satisfies L.is-success-result
  should-succeed-constructor = next-interaction("SD.make-mutable-string-dict().set-now('a', 2)")
  should-succeed-constructor.v satisfies L.is-success-result

  tc-result2 = restart("include string-dict", true)
  tc-result2.v satisfies L.is-success-result
  should-succeed-string-dict = next-interaction("is-function(make-string-dict)")
  val(should-succeed-string-dict) is some(true)

  tc-result3 = restart("import string-dict as SD", true)
  tc-result3.v satisfies L.is-success-result
  next-interaction("x = SD.make-mutable-string-dict()")
  next-interaction("x.set-now('a', 5)")
  should-succeed-set = next-interaction("x.get-value-now('a')")
  val(should-succeed-set) is some(5)

  print("Done running repl-tests: " + tostring(time-now()) + "\n")

end
