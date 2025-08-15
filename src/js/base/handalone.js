/*
TODO(joe): see how the lack of this interacts with CPO

if(typeof window === 'undefined') {
var require = require("requirejs");
}
require(["pyret-base/js/runtime", "pyret-base/js/exn-stack-parser", "program"], function(runtimeLib, stackLib, program) {

*/
// TODO: Change to myrequire
if(typeof window === 'undefined') {
  var window = this;
}
requirejs(["pyret-base/js/runtime", "pyret-base/js/post-load-hooks", "pyret-base/js/exn-stack-parser", "program"], function(runtimeLib, loadHooksLib, stackLib, program) {

  var staticModules = program.staticModules;
  var depMap = program.depMap;
  var toLoad = program.toLoad;
  var uris = program.uris;
  var realm = { instantiated: {}, static: {}};
  var util = require('util'); // This lets us mimic console.log of fancy objects, while using process.stderr.write

  var main = toLoad[toLoad.length - 1];

  var runtime = runtimeLib.makeRuntime({
    stdout: function(s) { process.stdout.write(s); },
    stderr: function(s) { process.stderr.write(s); },
    // TODO: this is semantically different. Is this ok?
    stdin: process.stdin,
  });

  var EXIT_SUCCESS = 0;
  var EXIT_ERROR = 1;
  var EXIT_ERROR_RENDERING_ERROR = 2;
  var EXIT_ERROR_DISPLAYING_ERROR = 3;
  var EXIT_ERROR_CHECK_FAILURES = 4;
  var EXIT_ERROR_JS = 5;
  var EXIT_ERROR_UNKNOWN = 6;

  runtime.setParam("command-line-arguments", process.argv.slice(1));

  function checkFlag(name) {
    return program.runtimeOptions && program.runtimeOptions[name];
  }

  if(checkFlag("disableAnnotationChecks")) {
    runtime.checkArgsInternal1 = function() {};
    runtime.checkArgsInternal2 = function() {};
    runtime.checkArgsInternal3 = function() {};
    runtime._checkAnn = function() {};
  }

  var postLoadHooks = loadHooksLib.makeDefaultPostLoadHooks(runtime, {main: main, checks: checkFlag("checks"), checksFormat: checkFlag("checksFormat")});
  postLoadHooks[main] = function(answer) {
    var checks = checkFlag("checks");
    if(checks && checks === "none") { process.exit(EXIT_SUCCESS); }
    var checkerLib = runtime.modules["builtin://checker"];
    var checker = runtime.getField(runtime.getField(checkerLib, "provide-plus-types"), "values");
    var getStack = function(err) {

      err.val.pyretStack = stackLib.convertExceptionToPyretStackTrace(err.val, realm);

      var locArray = err.val.pyretStack.map(runtime.makeSrcloc);
      var locList = runtime.ffi.makeList(locArray);
      return locList;
    };
    var getStackP = runtime.makeFunction(getStack, "get-stack");
    var toCall = runtime.getField(checker, "render-check-results-stack");
    var checks = runtime.getField(answer, "checks");
    return runtime.safeCall(function() {
      return toCall.app(checks, getStackP, checkFlag("checksFormat") || "text");
    }, function(summary) {
      // We're technically on the Pyret stack right now, but don't need to be.
      // So, pause the stack and switch off Pyret stack management so that the
      // use of the callbacks to write (and therefore lack of Pyret return value)
      // doesn't screw up Pyret's runtime.
      return runtime.pauseStack(function(resumer) {
        if(runtime.isObject(summary)) {
          var errs = runtime.getField(summary, "errored");
          var failed = runtime.getField(summary, "failed");
          var exitCode = (errs !== 0 || failed !== 0) ? EXIT_ERROR_CHECK_FAILURES : EXIT_SUCCESS;
          process.stdout.write(util.format(runtime.getField(summary, "message")));
          process.stdout.write("\n",
                               function() { process.exit(exitCode); });
        }
        // NOTE: Never calls resumer.resume, because there's nothing to do here beside exit
      });
    }, "postLoadHooks[main]:render-check-results-stack");
  }

  function renderErrorMessageAndExit(execRt, res) {
    if (execRt.isPyretException(res.exn)) {
      var rendererrorMod = execRt.modules["builtin://render-error-display"];
      var rendererror = execRt.getField(rendererrorMod, "provide-plus-types");
      var gf = execRt.getField;
      var exnStack = res.exn.stack;

      res.exn.pyretStack = stackLib.convertExceptionToPyretStackTrace(res.exn, realm);

      execRt.runThunk(
        function() {
          if (execRt.isObject(res.exn.exn) && execRt.hasField(res.exn.exn, "render-reason")) {
            return execRt.getColonField(res.exn.exn, "render-reason").full_meth(res.exn.exn);
          } else {
            return execRt.ffi.edEmbed(res.exn.exn);
          }
        },
        function(reasonResult) {
          // This callback is *not* on the Pyret stack, so no need to pause
          if (execRt.isFailureResult(reasonResult)) {
            process.stderr.write(
              "While trying to report that Pyret terminated with an error:\n" + JSON.stringify(res)
                + "\nPyret encountered an error rendering that error:\n" + JSON.stringify(reasonResult)
                + "\nStack:\n" + JSON.stringify(exnStack)
                + "\nPyret stack:\n" + execRt.printPyretStack(res.exn.pyretStack, true) + "\n",
              function() { process.exit(EXIT_ERROR_RENDERING_ERROR); });
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
                // This callback is *not* on the Pyret stack, so no need to pause
                if(execRt.isSuccessResult(printResult)) {
                  process.stderr.write(util.format(printResult.result));
                  process.stderr.write("\nPyret stack:\n" + execRt.printPyretStack(res.exn.pyretStack) + "\n",
                                       function() { process.exit(EXIT_ERROR); });
                } else {
                  process.stderr.write(
                      "While trying to report that Pyret terminated with an error:\n" + JSON.stringify(res)
                      + "\ndisplaying that error produced another error:\n" + JSON.stringify(printResult)
                      + "\nStack:\n" + JSON.stringify(exnStack)
                      + "\nPyret stack:\n" + execRt.printPyretStack(res.exn.pyretStack, true) + "\n",
                    function() { process.exit(EXIT_ERROR_DISPLAYING_ERROR); });
                }
              }, "errordisplay->to-string");
          }
        }, "error->display");
    } else if (res.exn && res.exn.stack) {
      process.stderr.write("Abstraction breaking: Uncaught JavaScript error:\n" + util.format(res.exn));
      process.stderr.write("Stack trace:\n" + util.format(res.exn.stack) + "\n",
                           function() { process.exit(EXIT_ERROR_JS); });
    } else {
      process.stderr.write("Unknown error result: " + util.format(res.exn) + "\n",
                           function() { process.exit(EXIT_ERROR_UNKNOWN); });
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
    process.stdout.write("", function() { process.exit(exitCode); });
  }

  function onComplete(result) {
    // This function is *not* on the Pyret stack, so no need to pause
    if(runtime.isSuccessResult(result)) {
      //console.log("The program completed successfully");
      //console.log(result);
      process.exit(EXIT_SUCCESS);
    }
    else if (runtime.isFailureResult(result)) {
      if (runtime.isPyretException(result.exn) && isExit(runtime, result)) {
        processExit(runtime, result.exn.exn);
      } else {
        process.stderr.write("The run ended in error:\n");
        try {
          renderErrorMessageAndExit(runtime, result);
        } catch(e) {
          process.stderr.write("EXCEPTION!\n" + util.format(e) + "\n",
                               function() { process.exit(EXIT_ERROR_JS); });
        }
      }
    } else {
      process.stderr.write("The run ended in an unknown error:\n" + util.format(result) + "\n");
      process.stderr.write(result.exn.stack,
                           function() { process.exit(EXIT_ERROR_UNKNOWN); });
    }
  }

  return runtime.runThunk(function() {
    runtime.modules = realm.instantiated;
    return runtime.runStandalone(staticModules, realm, depMap, toLoad, postLoadHooks);
  }, onComplete);
});
