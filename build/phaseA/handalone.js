require = require("requirejs");
require(["runtime"], function(runtimeLib) {

  var staticModules = {
    "builtin://option": {{{ OPTION }}},
    "builtinjs://just-option": {{{ JUSTOPTION }}}
  };

  var depMap = {
    "builtinjs://just-option": {
      "builtin(option)": "builtin://option"
    }
  };

  var runtime = runtimeLib.makeRuntime({});

  return runtime.loadBaseModules(staticModules, depMap, ["builtin://option"],  function(/* empty */) {
    console.log("Loaded option");
    return runtime.loadBaseModules(staticModules, depMap, ["builtinjs://just-option"],  function(justOpt) {
      console.log("Loaded justopt: ", justOpt);
    });
  })

/*
  loadJSModules(thisRuntime.namespace, [require("js/ffi-helpers")], function(f) {
    thisRuntime["ffi"] = ffi;
  });
  loadModulesNew(thisRuntime.namespace,
    [require("trove/srcloc")],
    function(srclocLib) {
      thisRuntime.srcloc = getField(srclocLib, "values");
    });
  loadModulesNew(thisRuntime.namespace, [require("trove/image-lib")], function(i) {
    thisRuntime["imageLib"] = getField(i, "internal");
  });

  // NOTE(joe): set a few of these explicitly to work with s-prim-app
  thisRuntime["throwMessageException"] = ffi.throwMessageException;
  thisRuntime["throwNoBranchesMatched"] = ffi.throwNoBranchesMatched;
  thisRuntime["throwNoCasesMatched"] = ffi.throwNoCasesMatched;
  thisRuntime["throwNonBooleanCondition"] = ffi.throwNonBooleanCondition;
  thisRuntime["throwNonBooleanOp"] = ffi.throwNonBooleanOp;

  var checkList = makeCheckType(ffi.isList, "List");
  thisRuntime["checkList"] = checkList;

  thisRuntime["checkEQ"] = makeCheckType(ffi.isEqualityResult, "EqualityResult");
*/
});
