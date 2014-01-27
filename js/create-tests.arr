#lang pyret

import ast as A
import json as J
import file as F
import directory as D
import pyret-eval as E
import format as FMT
import namespaces as N
import io as IO
import "compile.arr" as P
import "count-nodes.arr" as C
import "compile-structs.arr" as CS

format = FMT.format

TESTS-PATH = "test-runner/tests.js"

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
    str = if is-string(val): val else: torepr(val);
    the-output := the-output + str + "\n"
    val
  end
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
  for list.each(f from dir.list()):
    new-path = path + "/" + f
    when D.dir(new-path).exists():
      for list.each(test-file from D.dir(new-path).list()):
        file-path = new-path + "/" + test-file
        l = file-path.length()
        when file-path.substring(l - 4, l) == ".arr":
          file-contents = read-then-close(file-path)
          out-contents = read-then-close(file-path + ".out")
          err-contents = read-then-close(file-path + ".err")
          create-test(new-path, test-file, file-contents, out-contents, err-contents)
        end
      end
    end
  end
end

fun create-simple-test(path, name, program, out, err):
  create-jasmine-test(path, name, program, CS.no-builtins, out, err)
end

var tests-with-errors = []

fun create-jasmine-test(path, name, program, libs, expected-out, expected-err):
  print("Compiling: " + name)
  compiled = P.compile-runnable-js(program, name, libs, { check-mode : false, extra-ids: ["test-print"] })
  cases(CS.CompileResult) compiled:
    | err(message) => tests-with-errors := (tests-with-errors + [{name: name, compiled: compiled}])
    | ok(code) =>
      contents = format("
\"use strict\";
var r = require('requirejs');
r(['../../../runtime-anf'], function(R) {
  describe('~a', function() {
    it('should work', function(done) {
      var expectedOutput = ~s;
      var expectedError = ~s;
      var output = '';
      var rt = R.makeRuntime({
        stdout: function(str) {
          output += str;
        }
      });
      var theProgram = null;
      var define = function(ignored, program) {
        theProgram = program();
      };
      ~a;

      rt.run(theProgram, rt.namespace, function(result) {
        if (rt.isSuccessResult(result)) {
          expect(output).toEqual(expectedOutput);
          done();
        } else if (rt.isFailureResult(result)) {
          expect(output).toEqual(expectedOutput);
          if(expectedError.length <= 0) {
              expect(\"An error occured, when pyret had none: \" + result.exn.message).toBe(\"no error message\");
              expect(\"Stack: \" + result.exn.stack).toBe(\"no error stack\");
          }
          done();
        }
      });
    });
  });
});
    ", [name, expected-out, expected-err, code])

      gen-path = "generated-tests/" + path + "/"
      gdir = D.dir(gen-path)
      when not gdir.exists(): gdir.create();
      f = F.output-file(gen-path + "/" + name + "-spec.js", false)
      f.display(contents)
      f.close-file()
  end
end

#BASIC-TESTS = get-dir-sections("tests", create-print-test)
#CLASS-TESTS = get-dir-sections("class", create-print-test)
#MOORINGS-TESTS = get-dir-sections("moorings-tests", create-moorings-test)
#LIST-LIB-TESTS = get-dir-sections("list-lib-tests", create-list-test)

#generate-test-files(
#    [test-section("misc", MISC)] +
#    BASIC-TESTS +
#    CLASS-TESTS +
#    MOORINGS-TESTS +
#    LIST-LIB-TESTS
     #, []
#     ,
#     [moorings-lib,
#     list-lib]
#  )


#all-tests("tests")
#all-tests("class")
#all-tests("moorings-tests")
#all-tests("list-lib-tests")

all-tests("anf-tests")
get-dir-sections("anf-tests", create-simple-test)

when tests-with-errors.length() > 0:
  print(tests-with-errors)
end
