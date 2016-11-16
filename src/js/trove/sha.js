({
  requires: [],
  nativeRequires: ["js-sha256"],
  provides: {
    values: {
      "sha256": "tany"
    },
    aliases: {},
    datatypes: {}
  },
  theModule: function(runtime, namespace, uri, sha) {
  // NOTE(joe): when moving this to troveA, add uri and crypto
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
