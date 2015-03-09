define([], function() {
  return function(runtime, ns) {
    var F = runtime.makeFunction;
    function getBuiltinLocator(name) {
      runtime.pauseStack(function(restarter) {
        // NOTE(joe): This is a bit of requireJS hackery that assumes a
        // certain layout for builtin modules
        require(["trove/" + name], function(m) {
          restarter.resume(runtime.makeObject({
            "get-raw-dependencies":
              F(function() {
                return m.dependencies;
              }),
            "get-raw-provides":
              F(function() {
                return m.provides;
              }),
            "get-raw-compiled":
              F(function() {
                return runtime.makeOpaque(m.theModule);
              })
          }));
        });

      });
    }
    var O = runtime.makeObject;
    return O({
      "provide-plus-types": O({
        types: { },
        values: O({
          "builtin-raw-locator": runtime.makeFunction(getBuiltinLocator)
        })
      }),
      "answer": runtime.nothing
    });
  };
});
