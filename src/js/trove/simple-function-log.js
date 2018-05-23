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
    var onFunctionPush = function(packet) {
      indentation = getIndentation(trace_len);
      console.log(indentation + "push", packet[1], packet[2], packet[3]);
    };

    // packet :: ("pop", return_val :: Expression)
    var onFunctionPop = function(packet) {
      indentation = getIndentation(trace_len);
      console.log(indentation + "pop", packet[1]);
    };

    var getIndentation = function(len) {
      return Array(len).join("  ")
    };

    var subscribe = function() {
      my_token = ffi.subscribeToFunctionTraces(onFunctionPush, onFunctionPop);
    };

    // do we actually want to make a module?
    // Really, I just want to hook into ffi's subscribe
    // and then print things from there...
    return runtime.makeObject({
    'provide-plus-types': runtime.makeObject({
      types: runtime.makeObject({}),
      values: runtime.makeObject({
        'subscribe': runtime.makeFunction(subscribe),
      })
    })
  });
  }
})
