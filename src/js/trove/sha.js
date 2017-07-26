({
  requires: [],
  nativeRequires: ["js-sha256"],
  provides: {},
  theModule: function(runtime, namespace, uri, sha) {
    return runtime.makeModuleReturn({
          sha256: runtime.makeFunction(function(str) {
            runtime.checkArity(1, arguments, ["sha"], false);
            runtime.checkString(str);
            var hash = sha.create();
            hash.update(str);
            return hash.hex();
          }, "sha256")
        },
        {});
  }
})
