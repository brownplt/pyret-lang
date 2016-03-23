define(["js/runtime-util"], function(util) {
  return util.definePyretModule("legacy-path",
    [],
    [],
    {},
    function(RUNTIME, NAMESPACE) {
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
                }),
              "get-raw-provides":
                F(function() {
                  return [];
                }),
              "get-raw-compiled":
                F(function() {
                  return RUNTIME.makeOpaque(function() { return m; });
                })
            }));
          });

        });
      }
      var O = RUNTIME.makeObject;
      return O({
        "provide-plus-types": O({
          types: { },
          values: O({
            "legacy-path-raw-locator": RUNTIME.makeFunction(getLegacyPath)
          })
        }),
        "aNAMESPACEwer": RUNTIME.nothing
      });
    });
})
