/*
TODO(joe): see how the lack of this interacts with CPO

if(typeof window === 'undefined') {
var require = require("requirejs");
}
require(["pyret-base/js/runtime", "pyret-base/js/exn-stack-parser", "program"], function(runtimeLib, stackLib, program) {

*/
// TODO: Change to myrequire
requirejs(["pyret-base/js/runtime", "pyret-base/js/exn-stack-parser", "program"], function(runtimeLib, stackLib, program) {

  var staticModules = program.staticModules;
  var depMap = program.depMap;
  var toLoad = program.toLoad;
  var uris = program.uris;

  var main = toLoad[toLoad.length - 1];

  // The evaluation of the runtime should never suspend.
  $__T.getRTS().delimitDepth = 2;
  var runtime = runtimeLib.makeRuntime({
    stdout: function(s) { process.stdout.write(s); },
    stderr: function(s) { process.stderr.write(s); }
  });
  $__T.getRTS().delimitDepth = 0;

  var EXIT_SUCCESS = 0;
  var EXIT_ERROR = 1;
  var EXIT_ERROR_RENDERING_ERROR = 2;
  var EXIT_ERROR_DISPLAYING_ERROR = 3;
  var EXIT_ERROR_CHECK_FAILURES = 4;
  var EXIT_ERROR_JS = 5;
  var EXIT_ERROR_UNKNOWN = 6;

  runtime.setParam("command-line-arguments", process.argv.slice(1));

  var postLoadHooks = {
    "builtin://srcloc": function(srcloc) {
      runtime.srcloc = runtime.getField(
        runtime.getField(srcloc, "provide-plus-types"), "values");
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
      runtime["throwInvalidTableColumn"] = ffi.throwInvalidTableColumn;
      runtime["toArray"] = ffi.toArray;

      var checkList = runtime.makeCheckType(ffi.isList, "List");
      runtime["checkList"] = checkList;

      runtime["checkEQ"] = runtime.makeCheckType(ffi.isEqualityResult, "EqualityResult");
    },
    "builtin://table": function(table) {
      table = table.jsmod;
      runtime["makeTable"] = table.makeTable;
      runtime["openTable"] = table.openTable;
      runtime["checkTable"] = runtime.makeCheckType(table.isTable, "Table");
      runtime["isTable"] = table.isTable;
      runtime["checkWrapTable"] = function(val) {
        runtime.checkTable(val);
        return val;
      };
      runtime.makePrimAnn("Table", table.isTable);
    },
    "builtin://data-source": function(ds) {
      ds = runtime.getField(runtime.getField(ds, "provide-plus-types"), "values");
      // Variadic convenience function for desugaring use.
      // 'type' corresponds to a loader option in `data-source.arr`

      runtime["asLoaderOption"] = function(type) {
        switch(type) {
        case "sanitizer":
            return runtime
              .getField(ds, "sanitize-col")
              .app(arguments[1], arguments[2]);
        default:
            runtime.ffi.throwMessageException(
              "Internal error: Invalid loader option type: " + type);
        }
      };
      // Convenience function for JS library use
      runtime["extractLoaderOption"] = function(opt) {
        var isSanitizer = runtime.getField(ds, "is-sanitize-col");
        return thisRuntime.safeCall(function () {
          return isSanitizer.app(opt)
        }, function (result) {
          if (runtime.unwrap(result)) {
            return {
              type: "sanitizer",
              col: runtime.getField(opt, "col"),
              sanitizer: runtime.getField(opt, "sanitizer")
            };
          } else {
            runtime.ffi.throwMessageException(
              "Internal error: Cannot coerce non-loader option");
          }
        })
      }
      runtime["builtin_sanitizers"] = {
        option : runtime.getField(ds, "option-sanitizer"),
        string : runtime.getField(ds, "string-sanitizer"),
        num : runtime.getField(ds, "num-sanitizer"),
        bool: runtime.getField(ds, "bool-sanitizer"),
        strict_num : runtime.getField(ds, "strict-num-sanitizer"),
        strings_only : runtime.getField(ds, "strings-only"),
        numbers_only : runtime.getField(ds, "numbers-only"),
        booleans_only : runtime.getField(ds, "booleans-only"),
        empty_only : runtime.getField(ds, "empty-only")
      };

      runtime["makeCStr"] = runtime.getField(ds, "c-str").app;
      runtime["makeCNum"] = runtime.getField(ds, "c-num").app;
      runtime["makeCBool"] = runtime.getField(ds, "c-bool").app;
      runtime["makeCCustom"] = runtime.getField(ds, "c-custom").app;
      runtime["makeCEmpty"] = function() { return runtime.getField(ds, "c-empty"); };

      runtime["isCStr"] = function(v) { return runtime.unwrap(runtime.getField(ds, "is-c-str").app(v)); };
      runtime["isCNum"] = function(v) { return runtime.unwrap(runtime.getField(ds, "is-c-num").app(v)); };
      runtime["isCBool"] = function(v) { return runtime.unwrap(runtime.getField(ds, "is-c-bool").app(v)); };
      runtime["isCCustom"] = function(v) { return runtime.unwrap(runtime.getField(ds, "is-c-custom").app(v)); };
      runtime["isCEmpty"] = function(v) { return runtime.unwrap(runtime.getField(ds, "is-c-empty").app(v)); };

      runtime["unwrapCellContent"] = function(v) {
        if (runtime.isCStr(v)) {
          return {type: "str", value: runtime.getField(v, "s")};
        } else if (runtime.isCNum(v)) {
          return {type: "num", value: runtime.getField(v, "n")};
        } else if (runtime.isCBool(v)) {
          return {type: "bool", value: runtime.getField(v, "b")};
        } else if (runtime.isCCustom(v)) {
          return {type: "custom", value: runtime.getField(v, "datum")};
        } else if (runtime.isCEmpty(v)) {
          return {type: "empty"};
        } else {
          runtime.ffi.throwMessageException("Internal error: Cannot unwrap non-cell content");
        }
      };

      runtime["makeLoadedTable"] = function(headers, contents) {
        // Headers can either be [name, sanitizer] arrays or
        // {name: name, sanitizer: sanitizer} objects
        headers = headers.map(function(h) {
          if (h.sanitizer) {
            return runtime.makeTuple([h.name, h.sanitizer]);
          } else {
            return runtime.makeTuple(h);
          }
        });
        return runtime.makeTuple([headers, contents]);
      };
      runtime["checkCellContent"] = runtime.makeCheckType(
        runtime.getField(ds, "is-CellContent").app, "CellContent");
    },
    "builtin://reactors": function(reactor) {
      var r = runtime.getField(
        runtime.getField(reactor, "provide-plus-types"), "values");
      runtime.setParam("makeReactor", runtime.getField(r, "make-reactor").app);
    },
    "builtin://checker": function(checker) {
      checker = runtime.getField(
        runtime.getField(checker, "provide-plus-types"), "values");
      // NOTE(joe): This is the place to add checkAll
      return runtime.safeCall(function() {
        return runtime.
          getField(checker, "make-check-context").
          app(runtime.makeString(main), true);
      }, function (currentChecker) {
        runtime.setParam("current-checker", currentChecker);
      })
    }
  };
  // last thing to run
  postLoadHooks[main] = function(answer) {
    var checkerLib = runtime.modules["builtin://checker"];
    var checker = runtime.getField(
      runtime.getField(checkerLib, "provide-plus-types"), "values");
    var getStack = function(err) {
      err.val.pyretStack =
        stackLib.convertExceptionToPyretStackTrace(err.val, program);

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

      // NOTE(rachit): No need to save the stack at this point since we know
      // there will be an error.
      $__T.getRTS().delimitDepth = 2
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

  var toRun = function() {
    runtime.modules = {};
    // staticModules contains the stopified code
    return runtime.runStandalone(
      staticModules, runtime.modules, depMap, toLoad, postLoadHooks);
  }

  $__T.getRTS().delimit(() => runtime.runThunk(toRun, onComplete))
});
