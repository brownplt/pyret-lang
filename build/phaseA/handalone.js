require = require("requirejs");
require(["runtime", "program"], function(runtimeLib, program) {

  var staticModules = program.staticModules;
  var depMap = program.depMap;
  var toLoad = program.toLoad;

  var main = toLoad[toLoad.length - 1];

  var runtime = runtimeLib.makeRuntime({
    stdout: function(s) { console.log(s); } 
  });

  var postLoadHooks = {
    "builtin://srcloc": function(srcloc) {
      runtime.srcloc = runtime.getField(runtime.getField(srcloc, "provide-plus-types"), "values");
    },
    "builtinjs://ffi": function(ffi) {
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
  };
  postLoadHooks[main] = function(answer) {
    console.log("Final answer: ", answer);
  }

  function onComplete(result) {
    if(runtime.isSuccessResult(result)) {
      console.log("The program completed successfully");
      console.log(result);
    }
    else {
      console.error("The run ended in error: ", result);
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
