define(["trove/arrays",
"trove/error",
"trove/lists",
"trove/option",
"trove/sets",
"trove/checker",
"trove/runtime-lib",
"js/secure-loader",
"js/runtime-anf"
], function(arrays, errors, lists, option, sets, checkerLib, runtimeLib, loader, rtLib) {
  

  var modValFlag = {};
  return function(runtime, namespace) {
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
      if(isPrimitive(mr.val.runtime, a)) { return runtime.ffi.makeSome(a); }
      else {
        return runtime.ffi.makeNone();
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
    function makeLoader(loadRuntimePyret) {
      runtime.checkArity(1, arguments, "make-loader");
      runtime.loadModulesNew(runtime.namespace, [runtimeLib], function(runtimeLib) {
        runtime.getField(runtimeLib, "internal").checkRuntime(loadRuntimePyret);
      });
      var loadRuntime = runtime.getField(loadRuntimePyret, "runtime").val;
      return loadRuntime.loadModulesNew(loadRuntime.namespace, [checkerLib, runtimeLib], function(checkerLib, runtimeLib) {
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
              return runtime.getField(compileResult, "pre-loaded");
            }
          }, function(toExec) {
            runtime.pauseStack(function(restart) {
              if (typeof toExec === "string") {
                // For now, a statically-determined list of defaults
                var fullDeps = [arrays, errors, lists, option, sets].concat(dependencies);
                var loaded = loader.loadSingle(loadRuntime, toExec, fullDeps);
              }
              else {
                // Don't auto-include the builtins for preloaded stuff
                var loaded = loader.loadClosure(loadRuntime, toExec, dependencies);
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
        function run(modval, modname) {
          loadRuntime.setParam("command-line-arguments", cca);
          var checker = loadRuntime.getField(checkerLib, "values");
          var currentChecker = loadRuntime.getField(checker, "make-check-context").app(loadRuntime.makeString(modname), loadRuntime.makeBoolean(checkAll));
          loadRuntime.setParam("current-checker", currentChecker);
          runtime.pauseStack(function(restarter) {
            loadRuntime.run(modval.val.moduleFun, modval.val.namespace, {}, function(result) {
              console.log("Success: ", result);
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
          "make-loader": runtime.makeFunction(makeLoader),
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

