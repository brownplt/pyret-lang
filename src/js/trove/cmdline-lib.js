({
  requires: [],
  provides: {
    values: {
      "command-line-arguments": "tany"
    },
    aliases: {},
    datatypes: {}
  },
  nativeRequires: [],
  theModule: function(runtime, _, uri) {
    return runtime.makeModuleReturn(
      {
        "command-line-arguments": runtime.makeFunction(function() {
          return runtime.ffi.makeList(runtime.getParam("command-line-arguments").map(runtime.makeString));
          }, "command-line-arguments"),
      },
      {});
  }
})
