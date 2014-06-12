define(["js/eval-lib", "../runtime/matchers", "js/ffi-helpers"], function(e, matchers, ffiLib) {
  var count = 0;
  function makeEvalCheckers(jasmine, runtime) {
    matchers.addPyretMatchers(jasmine);
    function gf(o, s) { return runtime.getField(o, s); }
    var tests = [];
    var ffi = ffiLib(runtime, runtime.namespace);

    function pushTest(test) {
      tests.push(test);
    }
    function wait(done) {
      if (tests.length === 0) {
        done();
      }
      else if (tests.length > 0) {
        tests.pop()(function() { wait(done); });
      }
    }

    function checkEvalsTo(str, answer) {
      pushTest(function(after) {
        e.evalPyret(runtime, str, {}, function(result) {
          expect(result).toBeSuccess(runtime);
          if(runtime.isSuccessResult(result)) {
            var actual = gf(result.result, "answer");
            expect(actual).toBeSameAs(runtime, answer);
          }
          after();
        });
      });
    }
    function checkEvalTests(str, pred) {
      pushTest(function(after) {
        e.evalPyret(runtime, str, {}, function(result) {
          expect(result).toBeSuccess(runtime);
          if(runtime.isSuccessResult(result)) {
            var checks = gf(result.result, "checks");
            expect(checks).toPassPredicate(pred);
          }
          after();
        });
      });
    }
    function checkEvalPred(str, pred) {
      pushTest(function(after) {
        e.evalPyret(runtime, str, {}, function(result) {
          expect(result).toBeSuccess(runtime);
          if (runtime.isSuccessResult(result)) {
            var actual = gf(result.result, "answer");
            expect(actual).toPassPredicate(pred);
          }
          after();
        });
      });
    }
    function checkError(str, exnPred) {
      pushTest(function(after) {
        e.evalPyret(runtime, str, {}, function(result) {
          expect(result).toBeFailure(runtime);
          if (runtime.isFailureResult(result)) {
            var exn = result.exn;
            try {
              expect(exn).toPassPredicate(exnPred);
            } catch(e) {
              console.error("Error while running predicate on program for errors: ", s, e);
              after();
            }
          }
          after();
        });
      });
    }
    function checkCompileErrorMsg(str, exnMsg) {
      function findInArray(arr) {
        for (var i = 0; i < arr.length; i++) {
          if(runtime.hasField(arr[i], "msg")) {
            var actMsg = runtime.getField(arr[i], "msg");
            if (runtime.isString(actMsg)) {
              expect(actMsg).toContain(exnMsg);
              if (runtime.unwrap(actMsg).indexOf(exnMsg) !== -1)
                return true;
            }
          } else {
            // NOTE(joe): only works when runing sync.
            var answer = runtime.runThunk(function() {
              return runtime.unwrap(runtime.toReprJS(arr[i], "tostring"));
            }, function(result) {
              if(result.result && result.result.indexOf(exnMsg) === -1) {
                console.error(result.result + " did not contain " + exnMsg);
              }
              if(typeof result.result === "string") {
                expect(result.result).toContain(exnMsg);
              }
              return true;
            });
            return true;
          }
        }
        return true;
      }
      return checkCompileError(str, findInArray);
    }
    function checkCompileError(str, exnPred) {
      pushTest(function(after) {
        e.compileSrcPyret(runtime, str, {}, function(result) {
          expect(result).toBeFailure(runtime);
          var problems = result.exn;
          // Compiling returns a string or an array of 
          // Pyret objects detailing problems
          expect(problems).toBeInstanceOf(Array);
          if (problems instanceof Array) {
            expect(problems).toPassPredicate(exnPred);
          }
          after();
        });
      });
    }
    return {
      checkCompileError: checkCompileError,
      checkCompileErrorMsg: checkCompileErrorMsg,
      checkEvalsTo: checkEvalsTo,
      checkEvalTests: checkEvalTests,
      checkEvalPred: checkEvalPred,
      checkError: checkError,
      wait: wait
    };
  }

  return {
    makeEvalCheckers: makeEvalCheckers
  };

});
