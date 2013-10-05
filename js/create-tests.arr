#lang pyret

import ast as A
import json as J
import file as F
import pyret-eval as E
import format as format
import namespaces as N
import io as IO
import "pyret-to-js.arr" as P

TESTS-PATH = "test-runner/tests.js"

runtime-ids = ["test-print"] + builtins.keys(N.whalesong-env)
fun toplevel-to-js(ast :: A.Program):
  P.program-to-js(ast, runtime-ids)
end

data TestPredicate:
  | test-print(correct-output :: String)
  | equal-to(result :: String)
  | predicate(pred-fun :: String)
end

data TestCase:
  | str-test-case(name :: String, program :: String, pred :: TestPredicate)
end

fun make-test(test-name :: String, ast :: A.Program, pred :: TestPredicate):
  cases (TestPredicate) pred:
    | equal-to(result) =>
      result-program = A.parse-tc(result, "test-equals", {check : false, env : N.whalesong-env})
      format("testEquals('~a', ~a, ~a)", [test-name, toplevel-to-js(ast), toplevel-to-js(result-program)])
    | predicate(pred-fun) =>
      format("testPred('~a', ~a, ~a)", [test-name, toplevel-to-js(ast), pred-fun])
    | test-print(correct-output) =>
      format("testPrint('~a', ~a, ~a)", [test-name, toplevel-to-js(ast),
        J.stringify({expected: correct-output})])
  end
end

fun generate-test-files(test-cases :: list.List<TestCase>):
  tests-file = F.output-file(TESTS-PATH, false)
  tests-file.display("var TESTS = {};\n")
  for list.each(test from test-cases):
    cases (TestCase) test:
      | str-test-case(name, program-text, pred) =>
        env = N.whalesong-env.{test-print: nothing}
        program = A.parse-tc(program-text, name, {check : false, env: env})
        tests-file.display(format("TESTS['~a'] = ~a;\n", [name, make-test(name, program, pred)]))
    end
  end
end

TESTS = [
  str-test-case(
      "number",
      "2",
      equal-to("2")
    ),
  str-test-case(
      "another-number",
      "2",
      predicate("isNumber")
    ),
  str-test-case(
      "addition",
      "2 + 2",
      equal-to("4")
    ),
  str-test-case(
      "print 5",
      "test-print(5)",
      test-print("5\n")
    )
]

fun generate-output(filename):
  in = F.input-file(filename)
  stdout = F.output-file(filename + ".out", false)
  var the-output = ""
  fun capturing-print(val):
    the-output := the-output + torepr(val) + "\n"
    val
  end
  env = N.whalesong-env.{test-print: capturing-print}
  program = A.parse-tc(in.read-file(), "test", {check : false, env: env})
  E.eval(A.to-native(program), env, {})
  stdout.display(the-output)
end

#generate-output("tests/numbers/five.arr")

generate-test-files(TESTS)
