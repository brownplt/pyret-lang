({
  requires: [
  ],
  nativeRequires: [
    'stopify'
  ],
  provides: {},
  theModule: function (RUNTIME, NAMESPACE, uri, Stopify) {

    function stopify(code) {
      return Stopify.compileFunction(code)
    }


    return RUNTIME.makeObject({
      'provide-plus-types': RUNTIME.makeObject({
        types: RUNTIME.makeObject({}),
        values: RUNTIME.makeObject({
          stopify: RUNTIME.makeFunction(stopify, "stopify")
        })
      })
    })
  }
})
