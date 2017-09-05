({
  requires: [
  ],
  nativeRequires: [
    //'stopify'
  ],
  provides: {},
  theModule: function (RUNTIME, NAMESPACE, uri, CLIB) {

    function stopify(code) {
      console.log(code)
      return code
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
