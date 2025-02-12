({
  requires: [
    { "import-type": "builtin", name: "runtime-lib" }
  ],
  nativeRequires: ["pyret-base/js/post-load-hooks", "pyret-base/js/exn-stack-parser", "pyret-base/js/secure-loader"],
  provides: {
    values: {
      "run-program": "tany",
      "is-success-result": "tany",
      "is-failure-result": "tany",
      "get-result-answer": "tany",
      "get-result-realm": "tany",
      "get-result-compile-result": "tany",
      "get-result-stacktrace": "tany",
      "render-check-results": "tany",
      "render-error-message": "tany",
      "empty-realm": "tany",
      "is-exit": "tany",
      "is-exit-quiet": "tany",
      "get-exit-code": "tany"
    },
    types: {
      Module: "tany",
      ModuleResult: "tany",
      Realm: "tany"
    }
  },
  theModule: function(runtime, namespace, uri, runtimeLib, loadHooksLib, stackLib, loader) {
    var EXIT_SUCCESS = 0;
    var EXIT_ERROR = 1;
    var EXIT_ERROR_RENDERING_ERROR = 2;
    var EXIT_ERROR_DISPLAYING_ERROR = 3;
    var EXIT_ERROR_CHECK_FAILURES = 4;
    var EXIT_ERROR_JS = 5;
    var EXIT_ERROR_UNKNOWN = 6;


    var brandModule = runtime.namedBrander("module", ["load-lib: module brander"]);
    var brandModuleResult = runtime.namedBrander("module-result", ["load-lib: module-result brander"]);
    var brandRealm = runtime.namedBrander("realm", ["load-lib: realm brander"]);

    var annModule = runtime.makeBranderAnn(brandModule, "Module");
    var annModuleResult = runtime.makeBranderAnn(brandModuleResult, "ModuleResult");
    var annRealm = runtime.makeBranderAnn(brandRealm, "Realm");

    function applyBrand(brand, val) {
      return runtime.getField(brand, "brand").app(val);
    }

    function makeRealm(dynamicModules) {
      return applyBrand(brandRealm, runtime.makeObject({
        "realm": runtime.makeOpaque(dynamicModules)
      }));
    }

    function emptyRealm() {
      return applyBrand(brandRealm, runtime.makeObject({
        "realm": runtime.makeOpaque({ instantiated: {}, static: {}})
      }));
    }

    function makeModule(runtimeForModule, moduleFun, namespace) {
      var m = runtime.makeOpaque({
        runtime: runtimeForModule,
        moduleFun: moduleFun,
        namespace: namespace
      });
      return m;
    }

    function makeModuleResult(runtimeForModule, result, realm, compileResult, program) {
      return runtime.makeOpaque({
        runtime: runtimeForModule,
        result: result,
        realm: realm,
        compileResult: compileResult,
        program: program
      });
    }

    function getModuleResultProgram(result) {
      return result.val.program;
    }

    function enrichStack(exn, realm) {
      return stackLib.convertExceptionToPyretStackTrace(exn, realm);
    }

    function checkSuccess(mr, field) {
      if(!mr.val) {
        console.error(mr);
        runtime.ffi.throwMessageException("Tried to get " + field + " of non-successful module compilation.");
      }
      if(!(mr.val.runtime.isSuccessResult(mr.val.result))) {
        console.error(mr.val.result);
        console.error(mr.val.result.exn);
        runtime.ffi.throwMessageException("Tried to get " + field + " of non-successful module execution.");
      }
    }
    function checkExn(mr) {
      if (!(mr.val.runtime.isFailureResult(mr.val.result))) {
        runtime.ffi.throwMessageException("Tried to get exn of non-failing module result.");
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
    function getRealm(mr) {
      return mr.val.realm;
    }
    function getModuleResultRealm(mr) {
      return runtime.getField(getRealm(mr), "realm").val;
    }
    function getResultCompileResult(mr) {
      return mr.val.compileResult;
    }
    function getResultProvides(mr) {
      return mr.val.provides;
    }
    function getResultStackTrace(mr) {
      var runtime = mr.val.runtime;
      if (!runtime.isFailureResult(mr.val.result)) {
        runtime.ffi.throwMessageException("Tried to get stacktrace of non-failing module result.");
      }

      var res = getModuleResultResult(mr);
      var stackString = runtime.printPyretStack(res.exn.pyretStack);
      var stackFrames = stackString.split("\n");
      var pyretStackFrames = stackFrames.map(function(frameString){
        return runtime.makeString(frameString.trim());
      });

      return runtime.makeArray(pyretStackFrames);
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
      var checks = mr.val.runtime.getField(mr.val.result.result, "checks");
      if(mr.val.runtime.ffi.isList(checks)) { return checks; }
      else { return mr.val.runtime.ffi.makeList([]); }
    }
    function getModuleResultExn(mr) {
      checkExn(mr);
      return mr.val.result.exn.exn;
    }
    function getErrorModField(mr, field) {
      var execRt = mr.val.runtime;
      var errorMod = execRt.modules["builtin://error"];
      var error = execRt.getField(errorMod, "provide-plus-types");
      var errorValues = execRt.getField(error, "values")
      return execRt.getField(errorValues, field);
    }
    function isExit(mr) {
      checkExn(mr);
      var exn = mr.val.result.exn.exn;
      var toCall = getErrorModField(mr, "is-exit");
      return runtime.unwrap(toCall.app(exn));
    }
    function isExitQuiet(mr) {
      checkExn(mr);
      var exn = mr.val.result.exn.exn;
      var toCall = getErrorModField(mr, "is-exit-quiet");
      return runtime.unwrap(toCall.app(exn));
    }
    function getExitCode(mr) {
      checkExn(mr);
      var exn = mr.val.result.exn.exn;
      return mr.val.runtime.getField(exn, "code");
    }
    function renderCheckResults(mr) {
      return runtime.pauseStack(function(restarter) {
        var res = getModuleResultResult(mr);
        var execRt = mr.val.runtime;
        var checkerMod = execRt.modules["builtin://checker"];
        var checker = execRt.getField(checkerMod, "provide-plus-types");
        var toCall = execRt.getField(execRt.getField(checker, "values"), "render-check-results-stack");
        var getStack = function(err) {
          // console.error("The error is: ", err);
          var locArray = err.val.pyretStack.map(execRt.makeSrcloc);
          var locList = execRt.ffi.makeList(locArray);
          return locList;
        };
        var getStackP = execRt.makeFunction(getStack, "get-stack");
        var checks = getModuleResultChecks(mr);
        const checksFormat = getModuleResultProgram(mr).runtimeOptions['checksFormat'] || "text";
        execRt.runThunk(function() { return toCall.app(checks, getStackP, checksFormat); },
          function(renderedCheckResults) {
            var resumeWith = {
              message: "Unknown error!",
              'exit-code': EXIT_ERROR_UNKNOWN
            };

            if(execRt.isSuccessResult(renderedCheckResults)) {
              resumeWith.message = execRt.unwrap(execRt.getField(renderedCheckResults.result, "message"));
              var errs = execRt.getField(renderedCheckResults.result, "errored");
              var failed = execRt.getField(renderedCheckResults.result, "failed");
              if(errs !== 0 || failed !== 0) {
                resumeWith["exit-code"] = EXIT_ERROR_CHECK_FAILURES;
              } else {
                resumeWith["exit-code"] = EXIT_SUCCESS;
              }
            }
            else if(execRt.isFailureResult(renderedCheckResults)) {
              console.error(renderedCheckResults.exn);
              resumeWith.message = "There was an exception while formatting the check results";
              resumeWith["exit-code"] = EXIT_ERROR_RENDERING_ERROR;
            }

            restarter.resume(runtime.makeObject({
              message: runtime.makeString(resumeWith.message),
              'exit-code': runtime.makeNumber(resumeWith["exit-code"])
            }));
          });
      });
    }
    function renderErrorMessage(mr) {
      var res = getModuleResultResult(mr);
      var execRt = mr.val.runtime;
      return runtime.pauseStack(function(restarter) {
        // TODO(joe): This works because it's a builtin and already loaded on execRt.
        // In what situations may this not work?
        var rendererrorMod = execRt.modules["builtin://render-error-display"];
        var rendererror = execRt.getField(rendererrorMod, "provide-plus-types");
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
            restarter.resume(runtime.makeObject({
              message: v.result,
              'exit-code': runtime.makeNumber(EXIT_ERROR)
            }));
          } else {
            console.error(v.exn);
            restarter.resume(runtime.makeObject({
              message: runtime.makeString("Load error: there was an exception while rendering the exception."),
              'exit-code': runtime.makeNumber(EXIT_ERROR_RENDERING_ERROR)
            }));
          }
        })
      });
    }
    function getModuleResultAnswer(mr) {
      checkSuccess(mr, "answer");
      return mr.val.runtime.getField(mr.val.result.result, "answer");
    }
    /* ProgramString is a staticModules/depMap/toLoad tuple as a string */
    // TODO(joe): this should take natives as an argument, as well, and requirejs them
    function runProgram(otherRuntimeObj, realmObj, programString, options, commandLineArguments) {
      var checks = runtime.getField(options, "checks");
      if(!checks) { checks = "main"; }
      var otherRuntime = runtime.getField(otherRuntimeObj, "runtime").val;
      otherRuntime.setParam("command-line-arguments", runtime.ffi.toArray(commandLineArguments));
      var realm = {
        instantiated: Object.create(runtime.getField(realmObj, "realm").val.instantiated),
        static: Object.create(runtime.getField(realmObj, "realm").val.static)
      };
      var program = loader.safeEval("return " + programString, {});
      var staticModules = program.staticModules;
      var depMap = program.depMap;
      var toLoad = program.toLoad;
      var uris = program.uris;

      var main = toLoad[toLoad.length - 1];
      runtime.setParam("currentMainURL", main);

      if(realm.instantiated["builtin://checker"]) {
        // NOTE(joe): This is the place to add checkAll
        if (checks !== "none") {
          var checker = otherRuntime.getField(otherRuntime.getField(realm.instantiated["builtin://checker"], "provide-plus-types"), "values");
          var currentChecker = otherRuntime.getField(checker, "make-check-context").app(otherRuntime.makeString(main), checks);
          otherRuntime.setParam("current-checker", currentChecker);
        }
      }

      var postLoadHooks = loadHooksLib.makeDefaultPostLoadHooks(otherRuntime, {main: main, checks });

      return runtime.pauseStack(function(restarter) {
        var mainReached = false;
        var mainResult = "Main result unset: should not happen";
        postLoadHooks[main] = function(answer) {
          mainReached = true;
          mainResult = answer;
        }
        return otherRuntime.runThunk(function() {
          otherRuntime.modules = realm.instantiated;
          return otherRuntime.runStandalone(staticModules, realm, depMap, toLoad, postLoadHooks);
        }, function(result) {
          if(!mainReached) {
            // NOTE(joe): we should only reach here if there was an error earlier
            // on in the chain of loading that stopped main from running
            result.exn.pyretStack = stackLib.convertExceptionToPyretStackTrace(result.exn, realm);

            restarter.resume(makeModuleResult(otherRuntime, result, makeRealm(realm), runtime.nothing, program));
          }
          else {
            var finalResult = otherRuntime.makeSuccessResult(mainResult);
            finalResult.stats = result.stats;
            restarter.resume(makeModuleResult(otherRuntime, finalResult, makeRealm(realm), runtime.nothing, program));
          }
        });
      });

    }
    var vals = {
      "run-program": runtime.makeFunction(runProgram, "run-program"),
      "is-success-result": runtime.makeFunction(isSuccessResult, "is-success-result"),
      "is-failure-result": runtime.makeFunction(isFailureResult, "is-failure-result"),
      "get-result-answer": runtime.makeFunction(getAnswerForPyret, "get-result-answer"),
      "get-result-realm": runtime.makeFunction(getRealm, "get-result-realm"),
      "get-result-compile-result": runtime.makeFunction(getResultCompileResult, "get-result-compile-result"),
      "get-result-stacktrace": runtime.makeFunction(getResultStackTrace, "get-result-stacktrace"),
      "render-check-results": runtime.makeFunction(renderCheckResults, "render-check-results"),
      "render-error-message": runtime.makeFunction(renderErrorMessage, "render-error-message"),
      "empty-realm": runtime.makeFunction(emptyRealm, "empty-realm"),
      "is-exit": runtime.makeFunction(isExit, "is-exit"),
      "is-exit-quiet": runtime.makeFunction(isExitQuiet, "is-exit-quiet"),
      "get-exit-code": runtime.makeFunction(getExitCode, "get-exit-code"),
    };
    var types = {
      Module: annModule,
      ModuleResult: annModuleResult,
      Realm: annRealm
    };
    return runtime.makeObject({
      'defined-values': vals,
      'defined-types': types,
      "provide-plus-types": runtime.makeObject({
        values: runtime.makeObject(vals),
        types: types,
        internal: {
          makeRealm: makeRealm,
          enrichStack: enrichStack,
          getModuleResultProgram: getModuleResultProgram,
          getModuleResultAnswer: getModuleResultAnswer,
          getModuleResultChecks: getModuleResultChecks,
          getModuleResultTypes: getModuleResultTypes,
          getModuleResultValues: getModuleResultValues,
          getModuleResultRuntime: getModuleResultRuntime,
          getModuleResultRealm: getModuleResultRealm,
          getModuleResultResult: getModuleResultResult,
          getModuleResultNamespace: getModuleResultNamespace,
          getModuleResultDefinedTypes: getModuleResultDefinedTypes,
          getModuleResultDefinedValues: getModuleResultDefinedValues
        }
      })
    });
  }
})
