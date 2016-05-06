define(["js/runtime-util", "js/runtime-anf"], function(util, runtimeLib) {
  return util.definePyretModule("runtime-lib", [], {values: ["make-runtime"], types: ["Runtime"]}, function(runtime, ns) {
    var get = runtime.getField;
    function applyBrand(brand, val) {
      return get(brand, "brand").app(val);
    }

    var brandRuntime = runtime.namedBrander("runtime", ["runtime-lib: runtime brander"]);
    var annRuntime = runtime.makeBranderAnn(brandRuntime, "Runtime");
    var checkRuntime = function(v) { runtime._checkAnn(["runtime"], annRuntime, v); };

    function makeRuntime() {
      return applyBrand(brandRuntime, runtime.makeObject({
        "runtime": runtime.makeOpaque(runtimeLib.makeRuntime({
          stdout: runtime.stdout
        }))
      }));
    }
    return runtime.makeObject({
      "provide-plus-types": runtime.makeObject({
        types: {
          Runtime: annRuntime
        },
        values: runtime.makeObject({
          "make-runtime": runtime.makeFunction(makeRuntime)
        }),
        internal: {
          makeRuntime: makeRuntime,
          checkRuntime: checkRuntime,
          brandRuntime: brandRuntime
        }
      })
    });
  });
});

