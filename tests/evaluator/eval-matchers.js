define(["js/eval-lib", "../runtime/matchers", "js/ffi-helpers", "trove/render-error-display"], function(e, matchers, ffiLib, rendererrorLib) {
  var count = 0;
  function makeEvalCheckers(jasmine, runtime) {
    matchers.addPyretMatchers(jasmine);
    function gf(o, s) { return runtime.getField(o, s); }
    var tests = [];
    var ffi = ffiLib(runtime, runtime.namespace);
    var rendererror = undefined;

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
    function waitForModules(done) {
      runtime.loadModulesNew(runtime.namespace, [rendererrorLib], function(rendererrorLib) {
        rendererror = runtime.getField(rendererrorLib, "values");
        wait(done);
      });
    }
      

    function checkEvalsTo(str, answer) {
      pushTest(function(after) {
        e.runEvalPyret(runtime, str, {}, function(result) {
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
        e.runEvalPyret(runtime, str, {}, function(result) {
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
        e.runEvalPyret(runtime, str, {}, function(result) {
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
        e.runEvalPyret(runtime, str, {}, function(result) {
          expect(result).toBeFailure(runtime);
          if (runtime.isFailureResult(result)) {
            var exn = result.exn;
            try {
              expect(exn).toPassPredicate(exnPred);
            } catch(e) {
              console.error("Error while running predicate on program for errors: ", s, e);
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
              return runtime.getField(arr[i], "render-reason").app();
            }, function(result) {
              if (runtime.isSuccessResult(result)) {
                return runtime.runThunk(function() {
                  return runtime.getField(rendererror, "display-to-string").app(result.result,
                                                                                runtime.namespace.get("torepr"),
                                                                                runtime.ffi.makeList([]));
                }, function(result) {
                  if(result.exn) {
                    console.error("Failed to tostring excepton ", arr, result);
                    return false;
                  }
                  if(result.result && result.result.indexOf(exnMsg) === -1) {
                    console.error(">>>" + result.result + "<<< did not contain " + exnMsg);
                  }
                  if(typeof result.result === "string") {
                    expect(result.result).toContain(exnMsg);
                  }
                  return true;
                });
                return true;
              }
              return true;
            });
          }
        }
        return true;
      }
      return checkCompileError(str, findInArray);
    }
    function checkNoCompileError(str, exnPred) {
      pushTest(function(after) {
        e.runCompileSrcPyret(runtime, str, {}, function(result) {
          expect(result).toBeSuccess(runtime);
          after();
        });
      });
    }
    function checkCompileError(str, exnPred) {
      pushTest(function(after) {
        e.runCompileSrcPyret(runtime, str, {}, function(result) {
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
      checkNoCompileError: checkNoCompileError,
      checkCompileErrorMsg: checkCompileErrorMsg,
      checkEvalsTo: checkEvalsTo,
      checkEvalTests: checkEvalTests,
      checkEvalPred: checkEvalPred,
      checkError: checkError,
      wait: waitForModules
    };
  }

  return {
    makeEvalCheckers: makeEvalCheckers
  };

});
