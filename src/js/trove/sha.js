({
  requires: [],
  nativeRequires: ["crypto"],
  provides: {},
  // NOTE(joe): when moving this to troveA, add uri and crypto
  theModule: function(runtime, namespace, uri, crypto) {
    return runtime.makeObject({
      "provide-plus-types": runtime.makeObject({
        types: {},
        values: runtime.makeObject({
          sha256: runtime.makeFunction(function(str) {
            runtime.checkArity(1, arguments, ["sha"]);
              runtime.checkString(str);
            var hash = crypto.createHash('sha256');
            hash.update(str);
            return hash.digest('hex');
          })
        })
      })
    });
  }
})
