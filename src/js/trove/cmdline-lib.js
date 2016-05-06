define(["js/runtime-util"], function(util) {

  return util.memoModule("cmdline-lib", function(RUNTIME, NAMESPACE) {
      return RUNTIME.makeObject({
        provide: RUNTIME.makeObject({
          "command-line-arguments": RUNTIME.makeFunction(function() {
            return RUNTIME.ffi.makeList(RUNTIME.getParam("command-line-arguments").map(RUNTIME.makeString));
          }),
        }),
        answer: NAMESPACE.get("nothing")
      });
  });
});

