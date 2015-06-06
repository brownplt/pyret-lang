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
                if(m.dependencies) {
                  return m.dependencies.map(function(m) { return runtime.makeObject(m); });
                } else {
                  return [];
                }
              }),
            "get-raw-type-provides":
              F(function() {
                if(m.provides) {
                  return m.provides.types;
                }
                else {
                  return [];
                }
              }),
            "get-raw-value-provides":
              F(function() {
                if (m.provides) {
                  return m.provides.values;
                }
                else {
                  return [];
                }
              }),
            "get-raw-compiled":
              F(function() {
                if(m.theModule) {
                  return runtime.makeOpaque(m.theModule);
                }
                else {
                  return runtime.makeOpaque(function() { return m; });
                }
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
