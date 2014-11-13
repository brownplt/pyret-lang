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
    function createLoader() {
      var loadRuntime = rtLib.makeRuntime({});
      return loadRuntime.loadModulesNew(loadRuntime.namespace, [checkerLib], function(checkerLib) {
        function load(compileResult, listOfMods) {
          // TODO(joe): check for compileResult annotation
          runtime.checkList(listOfMods);
          var modArr = runtime.ffi.toArray(listOfMods);
          var dependencies = modArr.map(function(m) { return runtime.getField(m, "modval").val.module; });
          return runtime.safeCall(function() {
            return runtime.getField(compileResult, "pyret-to-js-runnable").app();
          }, function(toExec) {
            runtime.pauseStack(function(restart) {
              var fullDeps = [arrays, errors, lists, option, sets].concat(dependencies);
              var loaded = loader.loadSingle(loadRuntime, toExec, fullDeps);
              loaded.fail(function(err) {
                restart.error(runtime.ffi.makeMessageException(String(err)));
              });
              loaded.then(function(modVal) {
                restart.resume(runtime.makeOpaque({ module: modVal }));
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
            loadRuntime.run(modval.val.module, loadRuntime.namespace, {}, function(result) {
              if(loadRuntime.isSuccessResult(result)) {
                restarter.resume(wrapResult(loadRuntime, runtime, result));
              }
              else {
                restarter.resume(wrapResult(runtime, loadRuntime, result));
              }
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
      provide: runtime.makeObject({
        "make-loader": runtime.makeFunction(createLoader)
      })
    });
  };
});

