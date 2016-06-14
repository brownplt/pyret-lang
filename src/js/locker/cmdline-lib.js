define(["js/runtime-util"], function(util) {

  return util.definePyretModule("cmdline-lib",
    [],
    [],
    {},
    function(RUNTIME, NAMESPACE) {
      return RUNTIME.makeObject({
        'provide-plus-types': RUNTIME.makeObject({
          'values': RUNTIME.makeObject({
            "command-line-arguments": RUNTIME.makeFunction(function() {
              return RUNTIME.ffi.makeList(RUNTIME.getParam("command-line-arguments").map(RUNTIME.makeString));
            }),
          }),
          'types': {}
        }),
        answer: NAMESPACE.get("nothing")
      });
  });
});

