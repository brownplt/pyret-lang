define(["js/secure-loader", "js/runtime-util", "trove/runtime-lib"], function(loader, util, rt) {

  var mb = util.modBuiltin;
  
  return util.definePyretModule("load-lib",
    [
      rt
    ],
    [
      mb("runtime-lib")
    ],
    {
      values:
        [
          "make-loader",
          "is-success-result",
          "is-failure-result",
          "get-result-answer",
          "get-result-compile-env",
          "get-result-provides",
          "render-check-results",
          "render-error-message"
        ],
      types: [
        "Module",
        "ModuleResult"
      ]
    },
    function(runtime, namespace, runtimeLib) {
      var brandModule = runtime.namedBrander("module");
      var brandModuleResult = runtime.namedBrander("module-result");

      var annModule = runtime.makeBranderAnn(brandModule, "Module");
      var annModuleResult = runtime.makeBranderAnn(brandModuleResult, "ModuleResult");
      function applyBrand(brand, val) {
        return runtime.getField(brand, "brand").app(val);
      }

      function makeModule(runtimeForModule, moduleFun, namespace) {
        var m = runtime.makeOpaque({
          runtime: runtimeForModule,
          moduleFun: moduleFun,
          namespace: namespace
        });
        return m;
      }

      function makeModuleResult(runtimeForModule, result, compileResult) {
        return runtime.makeOpaque({
          runtime: runtimeForModule,
          result: result,
          compileResult: compileResult
        });
      }

      function checkSuccess(mr, field) {
        if(!(mr.val.runtime.isSuccessResult(mr.val.result))) {
          console.error(mr.val.result);
          console.error(mr.val.result.exn);
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
        if(isPrimitive(mr.val.runtime, a)) { return runtime.ffi.makeSome(a); }
        else {
          return runtime.ffi.makeNone();
        }
      }
      function getResultCompileResult(mr) {
        return mr.val.compileResult;
      }
      function getResultProvides(mr) {
        return mr.val.provides;
      }
      function getModuleResultRuntime(mr) {
        return mr.val.runtime;
      }
      function getModuleResultResult(mr) {
        return mr.val.result;
      }
      function getModuleResultNamespace(mr) {
        return mr.val.runtime.getField(mr.val.result.result, "namespace");
      }
      function getModuleResultDefinedValues(mr) {
        var rt = mr.val.runtime;
        if(rt.hasField(mr.val.result.result, "defined-values")) {
          return mr.val.runtime.getField(mr.val.result.result, "defined-values");
        }
        else {
          return {};
        }
      }
      function getModuleResultDefinedTypes(mr) {
        var rt = mr.val.runtime;
        if(rt.hasField(mr.val.result.result, "defined-types")) {
          return mr.val.runtime.getField(mr.val.result.result, "defined-types");
        }
        else {
          return {};
        }
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
      function renderCheckResults(mr) {
        var res = getModuleResultResult(mr);
        var execRt = mr.val.runtime;
        execRt.loadBuiltinModules([util.modBuiltin("checker")], "load-lib-render", function(checker) {
          var toCall = execRt.getField(execRt.getField(checker, "values"), "render-check-results");
          var checks = getModuleResultChecks(mr);
          runtime.pauseStack(function(restarter) {
              execRt.runThunk(function() { return toCall.app(checks); },
                function(printedCheckResult) {
                  if(execRt.isSuccessResult(printedCheckResult)) {
                    if(execRt.isString(printedCheckResult.result)) {
                      restarter.resume(runtime.makeString(execRt.unwrap(printedCheckResult.result)));
                    }
                  }
                  else if(execRt.isFailureResult(printedCheckResult)) {
                    restarter.resume(runtime.makeString("There was an exception while formatting the check results"));
                  }
                });
            });
         });
      }
      function renderErrorMessage(mr) {
        var res = getModuleResultResult(mr);
        var execRt = mr.val.runtime;
        runtime.pauseStack(function(restarter) {
          return execRt.loadBuiltinModules([
            mb("render-error-display")],
            "load-lib",
            function(rendererror) {
            var gf = execRt.getField;
            execRt.runThunk(function() {
                if(execRt.isPyretVal(res.exn.exn) 
                   && execRt.isObject(res.exn.exn) 
                   && execRt.hasField(res.exn.exn, "render-reason")) {
                  return execRt.safeCall(
                    function() { 
                      return execRt.getColonField(res.exn.exn, "render-reason").full_meth(res.exn.exn);
                    }, function(reason) {
                      return execRt.safeCall(
                        function() { 
                          return gf(gf(rendererror, "values"), "display-to-string").app(
                            reason, 
                            execRt.namespace.get("torepr"), 
                            execRt.ffi.makeList(res.exn.pyretStack.map(execRt.makeSrcloc)));
                        }, function(str) {
                          return execRt.string_append(
                            str,
                            execRt.makeString("\nStack trace:\n" +
                                              execRt.printPyretStack(res.exn.pyretStack)));
                        }, "errordisplay->to-string");
                    }, "error->display");
                } else {
                  return String(res.exn + "\n" + res.exn.stack);
                }
              }, function(v) {
                if(execRt.isSuccessResult(v)) {
                  return restarter.resume(v.result)
                } else {
                  console.error("load error");
                  console.error("There was an exception while rendering the exception: ",  r.exn, v.exn);

                }
              })
          });
        });
      }
      function getModuleResultAnswer(mr) {
        checkSuccess(mr, "answer");
        return mr.val.runtime.getField(mr.val.result.result, "answer");
      }
      function makeLoader(loadRuntimePyret) {
        runtime.checkArity(1, arguments, "make-loader");
        runtime.getField(runtimeLib, "internal").checkRuntime(loadRuntimePyret);
        var loadRuntime = runtime.getField(loadRuntimePyret, "runtime").val;
        return loadRuntime.loadBuiltinModules([
          mb("checker")],
          "load-lib",
          function(checkerLib) {
            function load(compileResult, listOfMods, namespace) {
              // TODO(joe): check for compileResult annotation
              runtime.checkList(listOfMods);
              var modArr = runtime.ffi.toArray(listOfMods);
              var dependencies = modArr.map(function(m) { return runtime.getField(m, "modval").val.moduleFun; });
              return runtime.safeCall(function() {
                if (runtime.hasField(compileResult, "result-printer")) {
                  var gf = runtime.getField;
                  return gf(gf(gf(compileResult, "result-printer"), "code"), "pyret-to-js-runnable").app();
                }
                else {
                  return runtime.getField(compileResult, "internal-mod");
                }
              }, function(toExec) {
                runtime.pauseStack(function(restart) {
                  if (typeof toExec === "string") {
                    var loaded = loader.loadSingle(loadRuntime, toExec, dependencies);
                  }
                  else {
                    var loaded = loader.loadClosure(loadRuntime, toExec.val, dependencies);
                  }
                  loaded.fail(function(err) {
                    restart.error(runtime.ffi.makeMessageException(String(err)));
                  });
                  loaded.then(function(modVal) {
                    restart.resume(makeModule(loadRuntime, modVal, runtime.getField(namespace, "namespace").val));
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
            function run(modval, compileResult, modname) {
              loadRuntime.setParam("command-line-arguments", cca);
              var checker = loadRuntime.getField(checkerLib, "values");
              var currentChecker = loadRuntime.getField(checker, "make-check-context").app(loadRuntime.makeString(modname), loadRuntime.makeBoolean(checkAll));
              loadRuntime.setParam("current-checker", currentChecker);
              runtime.pauseStack(function(restarter) {
                // NOTE(joe): this will b removed once polyglot is done
//                console.error("Here: ", modval);
//                if(modval.val.oldDependencies) {
//                  modval.val.moduleFun = modval.val.moduleFun.apply(null, modval.val.oldDependencies); 
//                }
                console.error("Here: ", modval);
                loadRuntime.run(modval.val.moduleFun, modval.val.namespace, {}, function(result) {
                  var modResult = makeModuleResult(loadRuntime, result, compileResult);
                  restarter.resume(modResult);
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
            "make-loader": runtime.makeFunction(makeLoader),
            "is-success-result": runtime.makeFunction(isSuccessResult),
            "is-failure-result": runtime.makeFunction(isFailureResult),
            "get-result-answer": runtime.makeFunction(getAnswerForPyret),
            "get-result-compile-result": runtime.makeFunction(getResultCompileResult),
            "render-check-results": runtime.makeFunction(renderCheckResults),
            "render-error-message": runtime.makeFunction(renderErrorMessage)
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
            getModuleResultResult: getModuleResultResult,
            getModuleResultNamespace: getModuleResultNamespace,
            getModuleResultDefinedTypes: getModuleResultDefinedTypes,
            getModuleResultDefinedValues: getModuleResultDefinedValues
          }
        })
      });
    });
  });

