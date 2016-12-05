{
  provides: {},
  requires: [],
  nativeRequires: [],
  theModule: function(runtime, _) {
    return runtime.makeModuleReturn({
      "munge-fun-name": runtime.makeFunction(runtime.mungeFunName, "munge-fun-name"),
      "unmunge-fun-name": runtime.makeFunction(runtime.unmungeFunName, "unmunge-fun-name")
    }, {});
  }
}
