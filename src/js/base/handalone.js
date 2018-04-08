/*
TODO(joe): see how the lack of this interacts with CPO

if(typeof window === 'undefined') {
var require = require("requirejs");
}
require(["pyret-base/js/runtime", "pyret-base/js/exn-stack-parser", "program"], function(runtimeLib, stackLib, program) {

*/
// TODO: Change to myrequire
requirejs(["pyret-base/js/runtime", "pyret-base/js/post-load-hooks", "pyret-base/js/exn-stack-parser", "program"], function(runtimeLib, loadHooksLib, stackLib, program) {

  var staticModules = program.staticModules;
  var depMap = program.depMap;
  var toLoad = program.toLoad;
  var uris = program.uris;

  var main = toLoad[toLoad.length - 1];

  var runtime = runtimeLib.makeRuntime({
    stdout: function(s) { process.stdout.write(s); },
    stderr: function(s) { process.stderr.write(s); }
  });

  var EXIT_SUCCESS = 0;
  var EXIT_ERROR = 1;
  var EXIT_ERROR_RENDERING_ERROR = 2;
  var EXIT_ERROR_DISPLAYING_ERROR = 3;
  var EXIT_ERROR_CHECK_FAILURES = 4;
  var EXIT_ERROR_JS = 5;
  var EXIT_ERROR_UNKNOWN = 6;

  runtime.setParam("command-line-arguments", process.argv.slice(1));

  var postLoadHooks = loadHooksLib.makeDefaultPostLoadHooks(runtime, {main: main, checkAll: true});
  postLoadHooks[main] = function(answer) {
    var profile = runtime.getProfile();
    if (profile.length > 0) {
      profile.forEach(function(entry) { process.stderr.write(JSON.stringify(entry) + "\n"); });
    }
    var checkerLib = runtime.modules["builtin://checker"];
    var checker = runtime.getField(runtime.getField(checkerLib, "provide-plus-types"), "values");
    var getStack = function(err) {

      err.val.pyretStack = stackLib.convertExceptionToPyretStackTrace(err.val, program);

      var locArray = err.val.pyretStack.map(runtime.makeSrcloc);
      var locList = runtime.ffi.makeList(locArray);
      return locList;
    };
    var getStackP = runtime.makeFunction(getStack, "get-stack");
    var toCall = runtime.getField(checker, "render-check-results-stack");
    var checks = runtime.getField(answer, "checks");
    return runtime.safeCall(function() {
      return toCall.app(checks, getStackP);
    }, function(summary) {
      if(runtime.isObject(summary)) {
        process.stdout.write(runtime.getField(summary, "message"));
        process.stdout.write("\n");
        var errs = runtime.getField(summary, "errored");
        var failed = runtime.getField(summary, "failed");
        if(errs !== 0 || failed !== 0) {
          process.exit(EXIT_ERROR_CHECK_FAILURES);
        }
        else {
          process.exit(EXIT_SUCCESS);
        }
      }
    }, "postLoadHooks[main]:render-check-results-stack");
  }

  function renderErrorMessageAndExit(execRt, res) {
    if (execRt.isPyretException(res.exn)) {
      var rendererrorMod = execRt.modules["builtin://render-error-display"];
      var rendererror = execRt.getField(rendererrorMod, "provide-plus-types");
      var gf = execRt.getField;
      var exnStack = res.exn.stack;

      res.exn.pyretStack = stackLib.convertExceptionToPyretStackTrace(res.exn, program);

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
                          + "\nPyret stack:\n" + execRt.printPyretStack(res.exn.pyretStack, true));
            process.exit(EXIT_ERROR_RENDERING_ERROR);
          } else {
            execRt.runThunk(
              function() {
                var cliRender = execRt.makeFunction(function(val) { 
                  return execRt.toReprJS(val, execRt.ReprMethods["$cli"]); 
                }, "cliRender");
                return gf(gf(rendererror, "values"), "display-to-string").app(
                  reasonResult.result,
                  cliRender,
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
                      + "\nPyret stack:\n" + execRt.printPyretStack(res.exn.pyretStack, true));
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

  function isExit(execRt, result) {
    var exn = result.exn.exn;
    return execRt.ffi.isExit(exn) || execRt.ffi.isExitQuiet(exn);
  }

  function processExit(execRt, exn) {
    var exitCode = execRt.getField(exn, "code");
    if (execRt.ffi.isExit(exn)) {
      var message = "Exited with code " + exitCode.toString() + "\n";
      process.stdout.write(message);
    }
    process.exit(exitCode);
  }

  function onComplete(result) {
    if(runtime.isSuccessResult(result)) {
      //console.log("The program completed successfully");
      //console.log(result);
      process.exit(EXIT_SUCCESS);
    }
    else if (runtime.isFailureResult(result)) {

      if (runtime.isPyretException(result.exn) && isExit(runtime, result)) {
        processExit(runtime, result.exn.exn);
      }
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
    var realm = { instantiated: runtime.modules, static: {}}
    return runtime.runStandalone(staticModules, realm, depMap, toLoad, postLoadHooks);
  }, onComplete);
});
