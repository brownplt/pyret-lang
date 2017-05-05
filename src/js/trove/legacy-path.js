({
  requires: [],
  nativeRequires: [],
  provides: {},
  theModule: function(RUNTIME, NAMESPACE) {
    var F = RUNTIME.makeFunction;
    function getLegacyPath(name) {
      RUNTIME.pauseStack(function(restarter) {
        // NOTE(joe): This is a bit of requireJS hackery that assumes a
        // certain layout for builtin modules
        require([name], function(m) {
          restarter.resume(RUNTIME.makeObject({
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
      'defined-modules': {},
      "provide-plus-types": O({
        types: { },
        modules: {},
        values: O({
          "legacy-path-raw-locator": RUNTIME.makeFunction(getLegacyPath, "legacy-path-raw-locator")
        })
      }),
      "answer": RUNTIME.nothing
    });
  }
})
