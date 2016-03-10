require = require("requirejs");
require(["runtime"], function(runtimeLib) {

  var staticModules = {
    "builtin://arrays": {{{ arrays_js }}},
    "builtin://contracts": {{{ contracts_js }}},
    "builtin://either": {{{ either_js }}},
    "builtin://equality": {{{ equality_js }}},
    "builtin://error": {{{ error_js }}},
    "builtin://error-display": {{{ error_display_js }}},
    "builtin://lists": {{{ lists_js }}},
    "builtin://option": {{{ option_js }}},
    "builtin://pick": {{{ pick_js }}},
    "builtin://sets": {{{ sets_js }}},
    "builtin://srcloc": {{{ srcloc_js }}},
    "builtin://valueskeleton": {{{ valueskeleton_js }}},
    "builtinjs://ffi": {{{ ffi_js }}}
  };

  var depMap = {
    "builtin://pick": {},
    "builtin://error-display": {},
    "builtin://either": {},
    "builtin://option": {},
    "builtin://valueskeleton": {},
    "builtin://srcloc": {
      "builtin(valueskeleton)": "builtin://valueskeleton"
    },
    "builtin://arrays": {
      "builtin(valueskeleton)": "builtin://valueskeleton",
      "builtin(lists)": "builtin://lists"
    },
    "builtin://contracts": {
      "builtin(error-display)": "builtin://error-display",
      "builtin(lists)": "builtin://lists"
    },
    "builtin://error": {
      "builtin(error-display)": "builtin://error-display"
    },
    "builtin://equality": {
      "builtin(error)": "builtin://error"
    },
    "builtin://lists": {
      "builtin(equality)": "builtin://equality",
      "builtin(option)": "builtin://option",
      "builtin(either)": "builtin://either",
      "builtin(valueskeleton)": "builtin://valueskeleton"
    },
    "builtin://sets": {
      "builtin(arrays)": "builtin://arrays",
      "builtin(pick)": "builtin://pick",
      "builtin(option)": "builtin://option",
      "builtin(equality)": "builtin://equality",
      "builtin(error)": "builtin://error",
      "builtin(valueskeleton)": "builtin://valueskeleton",
      "builtin(lists)": "builtin://lists"
    },
    "builtinjs://ffi": {
      "builtin(option)": "builtin://option",
      "builtin(sets)": "builtin://sets",
      "builtin(lists)": "builtin://lists",
      "builtin(either)": "builtin://either",
      "builtin(equality)": "builtin://equality",
      "builtin(error)": "builtin://error",
      "builtin(srcloc)": "builtin://srcloc",
      "builtin(contracts)": "builtin://contracts",
      "builtin(error-display)": "builtin://error-display",
      "builtin(valueskeleton)": "builtin://valueskeleton"
    }
  };

  var runtime = runtimeLib.makeRuntime({});

  var baseToLoad = [
    // 0 dependencies
    "builtin://option",
    "builtin://pick",
    "builtin://either",
    "builtin://valueskeleton",
    "builtin://error-display",

    // 1 or more deps
    "builtin://srcloc",
    "builtin://error",
    "builtin://equality",
    "builtin://lists",
    "builtin://contracts",

    "builtin://arrays",
    "builtin://sets"
  ];

  return runtime.loadBaseModules(staticModules, depMap, baseToLoad,  function(/* empty */) {
    console.log("Loaded option");
    return runtime.loadBaseModules(staticModules, depMap, ["builtinjs://ffi"],  function(ffi) {
      console.log("Loaded justopt: ", ffi);
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
