define([], function() {
  return function(runtime, ns) {
    var F = runtime.makeFunction;
    function getLegacyPath(name) {
      runtime.pauseStack(function(restarter) {
        // NOTE(joe): This is a bit of requireJS hackery that assumes a
        // certain layout for builtin modules
        require([name], function(m) {
          restarter.resume(runtime.makeObject({
            "get-raw-dependencies":
              F(function() {
                return [];
              }),
            "get-raw-provides":
              F(function() {
                return [];
              }),
            "get-raw-compiled":
              F(function() {
                return runtime.makeOpaque(function() { return m; });
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
          "legacy-path-raw-locator": runtime.makeFunction(getLegacyPath)
        })
      }),
      "answer": runtime.nothing
    });
  };
});
