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

USE-CPS = true

JS-ENV = N.library-env.{
  equiv: true,
  data-equals: true,
  data-to-repr: true
}
TEST-ENV = N.whalesong-env.{
  equiv: true,
  data-equals: true,
  data-to-repr: true
}


fun toplevel-to-js(ast :: A.Program):
  free-in-prog = A.free-ids(A.to-native(ast))
  P.program-to-js(ast, free-in-prog)
end

fun toplevel-to-cps-js(ast :: A.Program):
  free-in-prog = A.free-ids(A.to-native(ast))
  P.program-to-cps-js(ast, free-in-prog)
end

data TestPredicate:
  | test-print(correct-output :: String, correct-error :: String, cps :: Bool)
  | test-lib(lib :: A.Program, correct-output :: String, correct-error :: String, cps :: Bool)
  | equal-to(result :: String)
  | predicate(pred-fun :: String)
end

data TestCase:
  | str-test-case(name :: String, program :: String, pred :: TestPredicate)
end

data TestSection:
  | test-section(name :: String, test-cases :: list.List<TestCase>)
end

fun make-test(test-name :: String, program :: String, pred :: TestPredicate):
  cases (TestPredicate) pred:
    | equal-to(result) =>
      env = N.whalesong-env.{test-print: nothing}
      ast = A.parse-tc(program, test-name, {check : false, env: env})
      result-program = A.parse-tc(result, "test-equals", {check : false, env : N.whalesong-env})
      format("testEquals('~a', ~a, ~a)", [test-name, toplevel-to-js(ast), toplevel-to-js(result-program)])
    | predicate(pred-fun) =>
      env = N.whalesong-env.{test-print: nothing}
      ast = A.parse-tc(program, test-name, {check : false, env: env})
      format("testPred('~a', ~a, ~a)", [test-name, toplevel-to-js(ast), pred-fun])
    | test-print(correct-output, err-output, cps) =>
      env = N.whalesong-env.{test-print: nothing}
      ast = A.parse-tc(program, test-name, {check : false, env: env})
      compiler = if not cps: toplevel-to-js else: toplevel-to-cps-js;
      format("testPrint('~a', ~a, ~a, ~a)", [test-name, compiler(ast),
        J.stringify({expected-out: correct-output, expected-err: err-output}), cps])
    | test-lib(lib, correct-output, err-output, cps) =>
      compiler = if not cps: P.program-to-js else: P.program-to-cps-js;
      ids = P.toplevel-ids(lib)
      env = for fold(the-env from TEST-ENV.{test-print: true}, id from ids):
        the-env.{[id]: true}
      end
      ast = A.parse-tc(program, test-name, {check : false, env: env})
      free-in-lib = A.free-ids(A.to-native(lib))
      free-in-prog = A.free-ids(A.to-native(ast))
      format("testWithLib('~a', ~a, ~a, ~a, ~a)", [
          test-name,
          compiler(lib, free-in-lib),
          compiler(ast, free-in-prog),
          J.stringify({expected-out: correct-output, expected-err: err-output}),
          cps])
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
          tests-file.display(
              format(
                  "TESTS['~a']['~a'] = ~a;\n",
                  [section.name, name, make-test(name, program-text, pred)]
                )
            )
      end
    end
  end
  tests-file.close-file()
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
    ),
  str-test-case("lib-test",
      "test_field",
      test-lib(
          A.parse-tc(
              "test_field = 22
               checkers = {}",
              "lib-test",
              {
                check : false,
                env : N.library-env
              }
            ),
          "22",
          "",
          false
        )
    )
]

fun out-file-of(filename): filename + ".out";
fun err-file-of(filename): filename + ".err";

fun error-to-json(e):
  if builtins.has-field(e, "message"):
    e.message
  else:
    tostring(e)
  end
end

fun generate-output(filename):
  in = F.input-file(filename)
  stdout = F.output-file(out-file-of(filename), false)
  stderr = F.output-file(err-file-of(filename), false)
  var the-output = ""
  fun capturing-print(val):
    the-output := the-output + torepr(val) + "\n"
    val
  end
  print("generating for: " + filename)
  env = N.whalesong-env.{test-print: capturing-print}
  program = A.parse(in.read-file(), "test", {check : false})
  data EvalResult:
    | success(val)
    | exn(error)
  end
  value :: EvalResult = try:
    success(E.eval(A.to-native(program.pre-desugar), env, {}))
  except(e):
    exn(e)
  end
  cases(EvalResult) value:
    | success(v) =>
      stdout.display(the-output)
      stdout.display(torepr(v))
    | exn(e) => 
      stdout.display(the-output)
      stderr.display(error-to-json(e))
  end
  stdout.close-file()
  stderr.close-file()
  in.close-file()
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

fun read-then-close(path):
  file = F.input-file(path)
  contents = file.read-file()
  file.close-file()
  contents
end
            
# one level of sections for now
fun get-dir-sections(path, create-test):
  dir = D.dir(path)
  for list.fold(sections from [], f from dir.list()):
    new-path = path + "/" + f
    if D.dir(new-path).exists():
      tests = for list.fold(ts from [], test-file from D.dir(new-path).list()):
        file-path = new-path + "/" + test-file
        l = file-path.length()
        if file-path.substring(l - 4, l) == ".arr":
          file-contents = read-then-close(file-path)
          out-contents = read-then-close(file-path + ".out")
          err-contents = read-then-close(file-path + ".err")
          [create-test(test-file, file-contents, out-contents, err-contents)] + ts
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

fun create-print-test(name, program, out, err):
  print("Registering basic test: " + name)
  str-test-case(name, program, test-print(out, err, USE-CPS))
end


moorings-ast = A.parse-tc(
    read-then-close("libs/moorings.arr"),
    "moorings.arr",
     { check : false, env : JS-ENV }
  )
fun create-moorings-test(name, program, out, err):
  print("Registering moorings test: " + name)
  str-test-case(name, program, test-lib(moorings-ast, out, err, USE-CPS))
end

list-lib-ast = A.parse-tc(
    read-then-close("libs/just-list.arr"),
    "just-list.arr",
     { check : false, env : JS-ENV }
  )
fun create-list-test(name, program, out, err):
  print("Registering list test: " + name)
  str-test-case(name, program, test-lib(list-lib-ast, out, err, USE-CPS))
end

all-tests("tests")
all-tests("class")
#all-tests("moorings-tests")
all-tests("list-lib-tests")

BASIC-TESTS = get-dir-sections("tests", create-print-test)
CLASS-TESTS = get-dir-sections("class", create-print-test)
#MOORINGS-TESTS = get-dir-sections("moorings-tests", create-moorings-test)
LIST-LIB-TESTS = get-dir-sections("list-lib-tests", create-list-test)

generate-test-files(
#    [test-section("misc", MISC)] +
    BASIC-TESTS 
    #CLASS-TESTS 
    #MOORINGS-TESTS 
    #LIST-LIB-TESTS 
  )


