({
  requires: [],
  provides: {
    values: { "command-line-arguments": ["arrow", [], ["List", "String"]] } 
  },
  nativeRequires: [],
  theModule: function(runtime, _, uri) {
    return runtime.makeModuleReturn(
      {
          "command-line-arguments": runtime.makeFunction(function() {
              var args = runtime.getParam("command-line-arguments");
              if (args.length == 0) {
                  args = ["<browser>"];
              }

              return runtime.ffi.makeList(args.map(runtime.makeString));
          }, "command-line-arguments"),
      },
      {});
  }
})
