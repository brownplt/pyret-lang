#lang pyret

import ast as A
import json as J
import file as F
import directory as D
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

data TestSection:
  | test-section(name :: String, test-cases :: list.List<TestCase>)
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

fun generate-test-files(tests :: list.List<TestSection>):
  tests-file = F.output-file(TESTS-PATH, false)
  tests-file.display("var TESTS = {};\n")
  for list.each(section from tests):
    tests-file.display(format("TESTS['~a'] = {};\n", [section.name]))
    for list.each(test from section.test-cases):
      cases (TestCase) test:
        | str-test-case(name, program-text, pred) =>
          env = N.whalesong-env.{test-print: nothing}
          program = A.parse-tc(program-text, name, {check : false, env: env})
          tests-file.display(
              format(
                  "TESTS['~a']['~a'] = ~a;\n",
                  [section.name, name, make-test(name, program, pred)]
                )
            )
      end
    end
  end
end

MISC = [
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

fun out-file-of(filename): filename + ".out";

fun generate-output(filename):
  in = F.input-file(filename)
  stdout = F.output-file(out-file-of(filename), false)
  var the-output = ""
  fun capturing-print(val):
    the-output := the-output + torepr(val) + "\n"
    val
  end
  print("generating for: " + filename)
  env = N.whalesong-env.{test-print: capturing-print}
  program = A.parse(in.read-file(), "test", {check : false})
  value = E.eval(A.to-native(program.pre-desugar), env, {})
  stdout.display(the-output)
  stdout.display(torepr(value))
  stdout.close-file()
end


fun all-tests(path):
  dir = D.dir(path)
  for list.each(f from dir.list()):
    new-path = path + "/" + f
    if D.dir(new-path).exists():
      all-tests(new-path)
    else:
      l = new-path.length()
      when new-path.substring(l - 4, l) == ".arr":
        generate-output(new-path)
      end
    end
  end
end

# one level of sections for now
fun get-dir-sections(path):
  dir = D.dir(path)
  for list.fold(sections from [], f from dir.list()):
    new-path = path + "/" + f
    if D.dir(new-path).exists():
      tests = for list.fold(ts from [], test-file from D.dir(new-path).list()):
        file-path = new-path + "/" + test-file
        l = file-path.length()
        if file-path.substring(l - 4, l) == ".arr":
          [str-test-case(
              test-file,
              F.input-file(file-path).read-file(),
              test-print(F.input-file(out-file-of(file-path)).read-file())
            )] + ts
        else:
          ts
        end
      end
      [test-section(new-path, tests)] + sections
    else:
      sections
    end
  end
end

all-tests("tests")
FILE-TESTS = get-dir-sections("tests")

generate-test-files([test-section("misc", MISC)] + FILE-TESTS)


