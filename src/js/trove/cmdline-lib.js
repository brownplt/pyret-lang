define(["js/runtime-util", "js/ffi-helpers"], function(util, ffi) {

  return util.definePyretModule("cmdline-lib",
    [],
    [],
    {},
    function(RUNTIME, NAMESPACE) {
      return RUNTIME.loadJSModules(NAMESPACE, [ffi], function(F) {
        return RUNTIME.makeObject({
          provide: RUNTIME.makeObject({
            "command-line-arguments": RUNTIME.makeFunction(function() {
              return F.makeList(RUNTIME.getParam("command-line-arguments").map(RUNTIME.makeString));
            }),
          }),
          answer: NAMESPACE.get("nothing")
        });
      });
    });
});

