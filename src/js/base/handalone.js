require = require("requirejs");
require(["pyret-base/js/runtime", "program"], function(runtimeLib, program) {

  var staticModules = program.staticModules;
  var depMap = program.depMap;
  var toLoad = program.toLoad;

  var main = toLoad[toLoad.length - 1];

  var runtime = runtimeLib.makeRuntime({
    stdout: function(s) { process.stdout.write(s); },
    stderr: function(s) { process.stderr.write(s); } 
  });

  var EXIT_SUCCESS = 0;
  var EXIT_ERROR = 1;
  var EXIT_ERROR_RENDERING_ERROR = 2;
  var EXIT_ERROR_DISPLAYING_ERROR = 3;
  var EXIT_ERROR_JS = 4;
  var EXIT_ERROR_UNKNOWN = 5;

  runtime.setParam("command-line-arguments", process.argv.slice(1));

  var postLoadHooks = {
    "builtin://srcloc": function(srcloc) {
      runtime.srcloc = runtime.getField(runtime.getField(srcloc, "provide-plus-types"), "values");
    },
    "builtin://ffi": function(ffi) {
      ffi = ffi.jsmod;
      runtime.ffi = ffi;
      runtime["throwMessageException"] = ffi.throwMessageException;
      runtime["throwNoBranchesMatched"] = ffi.throwNoBranchesMatched;
      runtime["throwNoCasesMatched"] = ffi.throwNoCasesMatched;
      runtime["throwNonBooleanCondition"] = ffi.throwNonBooleanCondition;
      runtime["throwNonBooleanOp"] = ffi.throwNonBooleanOp;
      runtime["throwUnfinishedTemplate"] = ffi.throwUnfinishedTemplate;

      var checkList = runtime.makeCheckType(ffi.isList, "List");
      runtime["checkList"] = checkList;

      runtime["checkEQ"] = runtime.makeCheckType(ffi.isEqualityResult, "EqualityResult");
    },
    "builtin://checker": function(checker) {
      checker = runtime.getField(runtime.getField(checker, "provide-plus-types"), "values");
      // NOTE(joe): This is the place to add checkAll
      var currentChecker = runtime.getField(checker, "make-check-context").app(runtime.makeString(main), true);
      runtime.setParam("current-checker", currentChecker);
    }
  };
  postLoadHooks[main] = function(answer) {
    var checkerLib = runtime.modules["builtin://checker"];
    var checker = runtime.getField(runtime.getField(checkerLib, "provide-plus-types"), "values");
    var getStack = function(err) {
      console.error("The error is: ", err);
      var locArray = err.val.pyretStack.map(runtime.makeSrcloc);
      var locList = runtime.ffi.makeList(locArray);
      return locList;
    };
    var getStackP = runtime.makeFunction(getStack);
    var toCall = runtime.getField(checker, "render-check-results-stack");
    var checks = runtime.getField(answer, "checks");
    runtime.safeCall(function() {
      return toCall.app(checks, getStackP);
    }, function(printedCheckResult) {
      if(runtime.isString(printedCheckResult)) {
        process.stdout.write(printedCheckResult);
        process.stdout.write("\n");
      }
    });
  }

  function renderErrorMessageAndExit(execRt, res) {
    if (execRt.isPyretException(res.exn)) {
      var rendererrorMod = execRt.modules["builtin://render-error-display"];
      var rendererror = execRt.getField(rendererrorMod, "provide-plus-types");
      var gf = execRt.getField;
      var exnStack = res.exn.stack;
      var pyretStack = res.exn.pyretStack;
      execRt.runThunk(
        function() {
          if (execRt.isObject(res.exn.exn) && execRt.hasField(res.exn.exn, "render-reason")) {
            return execRt.getColonField(res.exn.exn, "render-reason").full_meth(res.exn.exn);
          } else {
            return execRt.ffi.edEmbed(res.exn.exn);
          }
        }, 
        function(reasonResult) {
          if (execRt.isFailureResult(reasonResult)) {
            console.error("While trying to report that Pyret terminated with an error:\n" + JSON.stringify(res)
                          + "\nPyret encountered an error rendering that error:\n" + JSON.stringify(reasonResult)
                          + "\nStack:\n" + JSON.stringify(exnStack)
                          + "\nPyret stack:\n" + execRt.printPyretStack(pyretStack, true));
            process.exit(EXIT_ERROR_RENDERING_ERROR);
          } else {
            execRt.runThunk(
              function() {
                return gf(gf(rendererror, "values"), "display-to-string").app(
                  reasonResult.result, 
                  execRt.namespace.get("torepr"), 
                  execRt.ffi.makeList(res.exn.pyretStack.map(execRt.makeSrcloc)));
              }, 
              function(printResult) {
                if(execRt.isSuccessResult(printResult)) {
                  console.error(printResult.result);
                  console.error("\nPyret stack:\n" + execRt.printPyretStack(res.exn.pyretStack));
                  process.exit(EXIT_ERROR);
                } else {
                  console.error(
                      "While trying to report that Pyret terminated with an error:\n" + JSON.stringify(res)
                      + "\ndisplaying that error produced another error:\n" + JSON.stringify(printResult)
                      + "\nStack:\n" + JSON.stringify(exnStack)
                      + "\nPyret stack:\n" + execRt.printPyretStack(pyretStack, true));
                  process.exit(EXIT_ERROR_DISPLAYING_ERROR);
                }
              }, "errordisplay->to-string");
          }
        }, "error->display");
    } else if (res.exn && res.exn.stack) {
      console.error("Abstraction breaking: Uncaught JavaScript error:\n", res.exn);
      console.error("Stack trace:\n", res.exn.stack);
      process.exit(EXIT_ERROR_JS);
    } else {
      console.error("Unknown error result: ", res.exn);
      process.exit(EXIT_ERROR_UNKNOWN);
    }
  }

  function onComplete(result) {
    if(runtime.isSuccessResult(result)) {
      //console.log("The program completed successfully");
      //console.log(result);
      process.exit(EXIT_SUCCESS);
    }
    else if (runtime.isFailureResult(result)) {
      console.error("The run ended in error:");
      try {
        renderErrorMessageAndExit(runtime, result);
      } catch(e) {
        console.error("EXCEPTION!", e);
      }
    } else {
      console.error("The run ended in an unknown error: ", result);
      console.error(result.exn.stack);
      process.exit(EXIT_ERROR_UNKNOWN);
    }
  }

  return runtime.runThunk(function() {
    runtime.modules = {};
    return runtime.runStandalone(staticModules, runtime.modules, depMap, toLoad, postLoadHooks);
  }, onComplete);
});
