define(["trove/arrays",
"trove/error",
"trove/lists",
"trove/option",
"trove/sets",
"js/secure-loader"], function(arrays, errors, lists, option, sets, loader) {
  

  var modValFlag = {};
  return function(runtime, namespace) {
    function load(compileResult, listOfMods) {
      // TODO(joe): check for compileResult annotation
      runtime.checkList(listOfMods);
      var modArr = runtime.ffi.toArray(listOfMods);
      var dependencies = modArr.map(function(m) { return runtime.getField(m, "modval").val.module; });
      return runtime.safeCall(function() {
        return runtime.getField(compileResult, "pyret-to-js-runnable").app();
      }, function(toExec) {
        console.log("About to eval: ", toExec.slice(0, 100));
        console.log("About to pause");
        runtime.pauseStack(function(restart) {
          var fullDeps = [arrays, errors, lists, option, sets].concat(dependencies);
          var loaded = loader.loadSingle(runtime, toExec, fullDeps);
          console.error("inside pause ");
          loaded.fail(function(err) {
            console.error("failed, ", err);
            restart.error(runtime.ffi.makeMessageException(String(err)));
          });
          loaded.then(function(modVal) {
            restart.resume(runtime.makeOpaque({ module: modVal }));
          });
        });
      });
    }
    function run(modval) {
      // TODO(joe): wrap in a subruntime
      console.log(modval);
      return runtime.safeTail(function() { return modval.val.module(runtime, runtime.namespace); });
    }
    return runtime.makeObject({
      provide: runtime.makeObject({
        load: runtime.makeFunction(load),
        run: runtime.makeFunction(run)
      })
    });
  };
});

