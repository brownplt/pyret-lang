({
  requires: [
    { "import-type": "builtin", name: "ffi" }
  ],
  nativeRequires: [],
  provides: {},
  theModule: function(runtime, namespace, uri, ffi) {
    var trace_len = 0;

    // packet :: ( "push", name :: String, formalArgs :: List<String>, actualArgs :: List<Expressions> )
    function onFunctionPush(packet) {
      indentation = getIndentation(trace_len);
      console.log(indentation + "push", packet[1], packet[2], packet[3]);
    }

    // packet :: ("pop", return_val :: Expression)
    function onFunctionPop(packet) {
      indentation = getIndentation(trace_len);
      console.log(indentation + "pop", packet[1]);
    }

    function getIndentation(len) {
      return Array(len).join("  ")
    }

    // do we actually want to make a module?
    // Really, I just want to hook into ffi's subscribe
    // and then print things from there...
    return runtime.makeJSModuleReturn({
      TableAnn : annTable,
      RowAnn : annRow,
      makeTable: makeTable,
      makeRow: makeRow,
      makeRowFromArray: makeRowFromArray,
      openTable: openTable,
      isTable: isTable,
      isRow: isRow
    },
      {});
  }
})
