define(["trove/arrays",
"trove/error",
"trove/lists",
"trove/option",
"trove/sets",
"trove/checker",
"js/secure-loader",
"js/runtime-anf"
], function(arrays, errors, lists, option, sets, checkerLib, loader, rtLib) {
  

  var modValFlag = {};
  return function(runtime, namespace) {
    var brandModule = runtime.namedBrander("module");
    var brandModuleResult = runtime.namedBrander("module-result");

    var annModule = runtime.makeBranderAnn(brandModule, "Module");
    var annModuleResult = runtime.makeBranderAnn(brandModuleResult, "ModuleResult");
    function applyBrand(brand, val) {
      return runtime.getField(brand, "brand").app(val);
    }

    function makeModule(runtimeForModule, moduleFun) {
      var m = runtime.makeOpaque({
        runtime: runtimeForModule,
        moduleFun: moduleFun
      });
      return m;
    }

    function makeModuleResult(runtimeForModule, result) {
      return runtime.makeOpaque({
        runtime: runtimeForModule,
        result: result
      });
    }

    function checkSuccess(mr, field) {
      if(!(mr.val.runtime.isSuccessResult(mr.val.result))) {
        runtime.ffi.throwMessageException("Tried to get " + field + " of non-successful module execution.");
      }
    }
    function isSuccessResult(mr) {
      return mr.val.runtime.isSuccessResult(mr.val.result);
    }
    function isFailureResult(mr) {
      return mr.val.runtime.isFailureResult(mr.val.result);
    }
    function isPrimitive(rt, ans) {
      return rt.isNumber(ans) || rt.isString(ans) || rt.isBoolean(ans);
    }
    function getAnswerForPyret(mr) {
      var a = getModuleResultAnswer(mr);
      if(isPrimitive(mr.val.runtime, a)) { return mr.val.runtime.ffi.makeSome(a); }
      else {
        return mr.val.runtime.ffi.makeNone();
      }
    }
    function getModuleResultRuntime(mr) {
      return mr.val.runtime;
    }
    function getModuleResultResult(mr) {
      return mr.val.result;
    }
    function getModuleResultValues(mr) {
      checkSuccess(mr, "values");
      return mr.val.runtime.getField(mr.val.runtime.getField(mr.val.result.result, "provide-plus-types"), "values").dict;
    }
    function getModuleResultTypes(mr) {
      checkSuccess(mr, "types");
      return mr.val.runtime.getField(mr.val.runtime.getField(mr.val.result.result, "provide-plus-types"), "types");
    }
    function getModuleResultChecks(mr) {
      checkSuccess(mr, "checks");
      return mr.val.runtime.getField(mr.val.result.result, "checks");
    }
    function getModuleResultAnswer(mr) {
      checkSuccess(mr, "answer");
      return mr.val.runtime.getField(mr.val.result.result, "answer");
    }
    function createLoader() {


      var loadRuntime = rtLib.makeRuntime({});
      return loadRuntime.loadModulesNew(loadRuntime.namespace, [checkerLib], function(checkerLib) {
        function load(compileResult, listOfMods) {
          // TODO(joe): check for compileResult annotation
          runtime.checkList(listOfMods);
          var modArr = runtime.ffi.toArray(listOfMods);
          var dependencies = modArr.map(function(m) { return runtime.getField(m, "modval").val.moduleFun; });
          return runtime.safeCall(function() {
            return runtime.getField(compileResult, "pyret-to-js-runnable").app();
          }, function(toExec) {
            runtime.pauseStack(function(restart) {
              var fullDeps = [arrays, errors, lists, option, sets].concat(dependencies);
              var loaded = loader.loadSingle(loadRuntime, toExec, fullDeps);
              loaded.fail(function(err) {
                console.log("Failed to load: ", err);
                restart.error(runtime.ffi.makeMessageException(String(err)));
              });
              loaded.then(function(modVal) {
                console.log("Loaded");
                restart.resume(makeModule(loadRuntime, modVal));
              });
            });
          });
        }
        var cca = [];
        function setCommandLineArgs(args) {
          cca = runtime.ffi.toArray(args);
        }
        var checkAll = false;
        function setCheckAll(newCheckAll) {
          checkAll = newCheckAll;
        }
        function wrapResult(execRt, callingRt, r) {
          if(execRt.isSuccessResult(r)) {
            var pyretResult = r.result;
            return callingRt.makeObject({
                "result": callingRt.makeOpaque(r),
                "success": callingRt.makeBoolean(true),
                "render-check-results": callingRt.makeFunction(function() {
                  var toCall = execRt.getField(checker, "render-check-results");
                  var checks = execRt.getField(pyretResult, "checks");
                  callingRt.pauseStack(function(restarter) {
                      execRt.run(function(rt, ns) {
                          return toCall.app(checks);
                        }, execRt.namespace, {sync: true},
                        function(printedCheckResult) {
                          if(execRt.isSuccessResult(printedCheckResult)) {
                            if(execRt.isString(printedCheckResult.result)) {
                              restarter.resume(callingRt.makeString(execRt.unwrap(printedCheckResult.result)));
                            }
                          }
                          else if(execRt.isFailureResult(printedCheckResult)) {
                            restarter.resume(callingRt.makeString("There was an exception while formatting the check results"));
                          }
                        });
                    });
                })
              });
          }
          else if(execRt.isFailureResult(r)) {
            return callingRt.makeObject({
                "result": callingRt.makeOpaque(r),
                "success": callingRt.makeBoolean(false),
                "failure": r.exn.exn,
                "render-error-message": callingRt.makeFunction(function() {
                  callingRt.pauseStack(function(restarter) {
                    execRt.runThunk(function() {
                      if(execRt.isPyretVal(r.exn.exn)) {
                        return execRt.string_append(
                          execRt.toReprJS(r.exn.exn, "tostring"),
                          execRt.makeString("\n" +
                                            execRt.printPyretStack(r.exn.pyretStack)));
                      } else {
                        return String(r.exn + "\n" + r.exn.stack);
                      }
                    }, function(v) {
                      if(execRt.isSuccessResult(v)) {
                        return restarter.resume(v.result)
                      } else {
                        console.error("There was an exception while rendering the exception: ", r.exn, v.exn);
                      }
                    })
                  });
                })
              });
          }
        }
        function run(modval, modname) {
          loadRuntime.setParam("command-line-arguments", cca);
          var checker = loadRuntime.getField(checkerLib, "values");
          var currentChecker = loadRuntime.getField(checker, "make-check-context").app(loadRuntime.makeString(modname), loadRuntime.makeBoolean(checkAll));
          loadRuntime.setParam("current-checker", currentChecker);
          runtime.pauseStack(function(restarter) {
            loadRuntime.run(modval.val.moduleFun, loadRuntime.namespace, {}, function(result) {
              restarter.resume(makeModuleResult(loadRuntime, result));
            });
          });
        }
        return runtime.makeObject({
          load: runtime.makeFunction(load),
          run: runtime.makeFunction(run)
        });
      });
    }
    return runtime.makeObject({
      "provide-plus-types": runtime.makeObject({
        values: runtime.makeObject({
          "make-loader": runtime.makeFunction(createLoader),
          "is-success-result": runtime.makeFunction(isSuccessResult),
          "is-failure-result": runtime.makeFunction(isFailureResult),
          "get-result-answer": runtime.makeFunction(getAnswerForPyret)
        }),
        types: {
          Module: annModule,
          ModuleResult: annModuleResult
        },
        internal: {
          getModuleResultAnswer: getModuleResultAnswer,
          getModuleResultChecks: getModuleResultChecks,
          getModuleResultTypes: getModuleResultTypes,
          getModuleResultValues: getModuleResultValues,
          getModuleResultRuntime: getModuleResultRuntime,
          getModuleResultResult: getModuleResultResult
        }
      })
    });
  };
});

