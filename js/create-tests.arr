#lang pyret

import ast as A
import file as F
import format as format
import "pyret-to-js.arr" as P

TESTS-PATH = "test-runner/tests.js"


data TestPredicate:
  | equal-to(result :: String)
  | predicate(pred-fun :: String)
end

data TestCase:
  | str-test-case(name :: String, program :: String, pred :: TestPredicate)
end

fun make-test(test-name :: String, ast :: A.Program, pred :: TestPredicate):
  cases (TestPredicate) pred:
    | equal-to(result) =>
      result-program = A.parse-tc(result, "test-equals", {check : false})
      format("testEquals('~a', ~a, ~a)", [test-name, P.program-to-js(ast), P.program-to-js(result-program)])
    | predicate(pred-fun) =>
      format("testPred('~a', ~a, ~a)", [test-name, P.program-to-js(ast), pred-fun])
  end
end

fun generate-test-files(test-cases :: list.List<TestCase>):
  tests-file = F.output-file(TESTS-PATH, false)
  tests-file.display("var TESTS = {};\n")
  for list.each(test from test-cases):
    cases (TestCase) test:
      | str-test-case(name, program-text, pred) =>
        program = A.parse-tc(program-text, name, {check : false})
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
    )
]

generate-test-files(TESTS)
