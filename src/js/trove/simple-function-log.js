({
  requires: [
    { "import-type": "builtin", name: "ffi" }
  ],
  nativeRequires: [],
  provides: {},
  theModule: function(runtime, namespace, uri, ffi) {
    var trace_len = 0;
    var my_token = -1;

    // packet :: ( "push", name :: String, formalArgs :: List<String>, actualArgs :: List<Expressions> )
    var onFunctionPush = runtime.makeFunction(function(packet) {
      indentation = getIndentation(trace_len);
      console.log(indentation + "push", packet[1], packet[2], packet[3]);
    });

    // packet :: ("pop", return_val :: Expression)
    onFunctionPop = runtime.makeFunction(function(packet) {
      indentation = getIndentation(trace_len);
      console.log(indentation + "pop", packet[1]);
    });

    getIndentation = runtime.makeFunction(function(len) {
      return Array(len).join("  ")
    });

    var subscribe = runtime.makeFunction(function() {
      my_token = ffi.subscribeToFunctionTraces(onFunctionPush, onFunctionPop);
    });

    // do we actually want to make a module?
    // Really, I just want to hook into ffi's subscribe
    // and then print things from there...
    return runtime.makeModuleReturn({
      subscribe: subscribe
    },
      {});
  }
})
