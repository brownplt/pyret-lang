({
  requires: [
    { "import-type": "builtin", name: "ffi" }
  ],
  nativeRequires: [],
  provides: {},
  theModule: function(runtime, namespace, uri, ffi) {
    

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
