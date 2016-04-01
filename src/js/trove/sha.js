define(["crypto", "js/runtime-util"], function(crypto, util) {
  return util.definePyretModule("sha", [], [], {},
    function(runtime, namespace, _) {
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
    });
})
