require = require("requirejs");
require(["js/runtime", "program"], function(runtimeLib, program) {

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

      var checkList = runtime.makeCheckType(ffi.isList, "List");
      runtime["checkList"] = checkList;

      runtime["checkEQ"] = runtime.makeCheckType(ffi.isEqualityResult, "EqualityResult");
    },
    "builtin://checker": function(checker) {
      checker = runtime.getField(runtime.getField(checker, "provide-plus-types"), "values");
      // NOTE(joe): This is the place to add checkAll
      var currentChecker = runtime.getField(checker, "make-check-context").app(runtime.makeString(main), false);
      runtime.setParam("current-checker", currentChecker);
    }
  };
  postLoadHooks[main] = function(answer) {
    var checkerLib = runtime.modules["builtin://checker"];
    var checker = runtime.getField(runtime.getField(checkerLib, "provide-plus-types"), "values");
    var toCall = runtime.getField(checker, "render-check-results");
    var checks = runtime.getField(answer, "checks");
    runtime.safeCall(function() {
      return toCall.app(checks);
    }, function(printedCheckResult) {
      if(runtime.isString(printedCheckResult)) {
        process.stdout.write(printedCheckResult);
      }
    });
  }

  function onComplete(result) {
    if(runtime.isSuccessResult(result)) {
      //console.log("The program completed successfully");
      //console.log(result);
    }
    else {
      console.error("The run ended in error: ", result);
      console.error(result.exn.stack);
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
