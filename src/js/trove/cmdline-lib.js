({
  requires: [],
  provides: {},
  nativeRequires: [],
  theModule: function(RUNTIME, NAMESPACE, uri) {
    return RUNTIME.makeObject({
      'provide-plus-types': RUNTIME.makeObject({
        'values': RUNTIME.makeObject({
          "command-line-arguments": RUNTIME.makeFunction(function() {
            return RUNTIME.ffi.makeList(RUNTIME.getParam("command-line-arguments").map(RUNTIME.makeString));
          }, "command-line-arguments"),
        }),
        'types': {}
      }),
      answer: NAMESPACE.get("nothing")
    });
  }
})
