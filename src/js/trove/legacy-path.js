({
  requires: [],
  nativeRequires: [],
  provides: {},
  theModule: function(RUNTIME, NAMESPACE) {
    var F = RUNTIME.makeFunction;
    function getLegacyPath(name) {
      return RUNTIME.pauseStack(function(restarter) {
        // NOTE(joe): This is a bit of requireJS hackery that assumes a
        // certain layout for builtin modules
        return require([name], function(m) {
          return restarter.resume(RUNTIME.makeObject({
            "get-raw-dependencies":
            F(function() {
              return [];
            }, "get-raw-dependencies"),
            "get-raw-provides":
            F(function() {
              return [];
            }, "get-raw-provides"),
            "get-raw-compiled":
            F(function() {
              return RUNTIME.makeOpaque(function() { return m; });
            }, "get-raw-compiled")
          }));
        });

      });
    }
    var O = RUNTIME.makeObject;
    return O({
      'defined-values': {
          "legacy-path-raw-locator": RUNTIME.makeFunction(getLegacyPath, "legacy-path-raw-locator")
        },
      'defined-types': {},
      "provide-plus-types": O({
        types: { },
        values: O({
          "legacy-path-raw-locator": RUNTIME.makeFunction(getLegacyPath, "legacy-path-raw-locator")
        })
      }),
      "answer": RUNTIME.nothing
    });
  }
})
