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

  function renderErrorMessage(execRt, res) {
    var rendererrorMod = execRt.modules["builtin://render-error-display"];
    var rendererror = execRt.getField(rendererrorMod, "provide-plus-types");
    var gf = execRt.getField;
    return execRt.runThunk(function() {
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
        console.log(v.result);
      } else {
        console.error("There was an exception while rendering the exception: ", v.exn);
      }
    });
  }

  function onComplete(result) {
    if(runtime.isSuccessResult(result)) {
      //console.log("The program completed successfully");
      //console.log(result);
    }
    else if (runtime.isFailureResult(result)) {
      console.error("The run ended in error:");
      renderErrorMessage(runtime, result);
      process.exit(1);
    } else {
      console.error("The run ended in an unknown error: ", result);
      console.error(result.exn.stack);
      process.exit(1);
    }
  }

  return runtime.runThunk(function() {
    return runtime.runStandalone(staticModules, depMap, toLoad, postLoadHooks);
  }, onComplete);

/*
  loadModulesNew(thisRuntime.namespace, [require("trove/image-lib")], function(i) {
    thisRuntime["imageLib"] = getField(i, "internal");
  });
*/
});
