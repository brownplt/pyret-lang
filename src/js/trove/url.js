({
    provides: {
        values: {
          resolve: ["arrow", ["String", "String"], "String"]
        },
        types: {},
    },
    requires: [ ],
    nativeRequires: [],
    theModule: function(runtime, _, uri) {
      function resolve(path, base) {
        runtime.checkArgsInternal2("url", "resolve", path, runtime.String, base, runtime.String);
        try {
          return new URL(path, base).href;
        }
        catch(e) {
          runtime.ffi.throwMessageException("url.resolve() failed: " + String(e));
        }
      }
      return runtime.makeModuleReturn({
        resolve: runtime.makeFunction(resolve)
      }, {});
    }
})
