define(["js/runtime-util", "js/namespace", "trove/load-lib"], function(util, namespace, loadLib) {
  return util.memoModule("namespace-lib.js", function(runtime, ns) {
    var get = runtime.getField;
    function applyBrand(brand, val) {
      return get(brand, "brand").app(val);
    }

    var brandNamespace = runtime.namedBrander("namespace");
    var annNamespace = runtime.makeBranderAnn(brandNamespace, "Namespace");
    var checkNamespace = runtime.makePrimitiveAnn(annNamespace);

    return runtime.loadModulesNew(ns, [loadLib], function(load) {
//      var checkModuleResult = function(v) { return runtime._checkAnn(["namespace"], runtime.getField(load, "types").ModuleResult, v); };
      var loadInternalAPI = runtime.getField(load, "internal");

      function makeNamespace(runtimeNamespaceIsFor, ns) {
        
        function mergeAll(self, answer) {
//          checkModuleResult(answer);
          var runtimeOfAnswer = loadInternalAPI.getModuleResultRuntime(answer);
          if(runtimeOfAnswer !== runtimeNamespaceIsFor) {
            runtime.ffi.throwMessageException("Namespace got values from different runtimes.");
          }
          var typesOfAnswer = loadInternalAPI.getModuleResultTypes(answer);
          var valuesOfAnswer = loadInternalAPI.getModuleResultValues(answer);
          var newNamespace = ns;
          Object.keys(typesOfAnswer).forEach(function(k) {
            newNamespace = newNamespace.setType(k, typesOfAnswer[k]);
          });
          Object.keys(valuesOfAnswer).forEach(function(k) {
            newNamespace = newNamespace.set(k, valuesOfAnswer[k]);
          });
          return makeNamespace(runtimeNamespaceIsFor, newNamespace);
        }

        var obj = runtime.makeObject({
          "merge-all": runtime.makeMethodFromFun(mergeAll),
          "namespace": runtime.makeOpaque(ns)
        });
        return applyBrand(brandNamespace, obj);
      }

      return runtime.makeObject({
        "provide-plus-types": runtime.makeObject({
          types: {
            Namespace: annNamespace
          },
          values: runtime.makeObject({
            "make-empty-namespace": runtime.makeFunction(function(nsRuntime) {
              return makeNamespace(get(nsRuntime, "runtime").val, namespace.namespace({}));
            }),
            "make-base-namespace": runtime.makeFunction(function(nsRuntime) {
              var r = get(nsRuntime, "runtime").val;
              return makeNamespace(r, r.namespace);
            })
          }),
          internal: {
            makeNamespace: makeNamespace
          }
        })
      });
    });
  });
});
