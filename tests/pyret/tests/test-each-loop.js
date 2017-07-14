({
  requires: [],
  provides: {},
  nativeRequires: [],
  theModule: function(runtime, _, _) {
    function runEachLoop(f, start, stop, initialGas, initialRunGas) {
      return runtime.pauseStack(function(restarter) {
        return runtime.runThunk(function() {
          return runtime.eachLoop(f, start, stop);
        },
        function(result) {
          restarter.resume(runtime.makeObject({
            stats: runtime.makeObject(result.stats)
          }));
        },
        {
          initialGas: initialGas,
          initialRunGas: initialRunGas
        })
      });
    }
    return runtime.makeModuleReturn({
      "run-each-loop": runtime.makeFunction(runEachLoop, "run-each-loop")
    },
    {});
  }
})
