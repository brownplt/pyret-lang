define(["js/runtime-util", "js/ffi-helpers"], function(util, ffi) {

  return util.memoModule("cmdline-lib", function(RUNTIME, NAMESPACE) {
    var F = ffi(RUNTIME, NAMESPACE);

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

