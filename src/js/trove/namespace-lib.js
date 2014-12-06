define(["js/namespace", "trove/load-lib"], function(namespace, loadLib) {
  return util.memoModule("namespace-lib.js", function(runtime, ns) {
    function applyBrand(brand, val) {
      return get(brand, "brand").app(val);
    }

    var brandNamespace = runtime.namedBrander("namespace");
    var annNamespace = runtime.makeBranderAnn(brandNamespace, "Namespace");
    var checkNamespace = runtime.makePrimitiveAnn(annNamespace);

    return runtime.loadModulesNew(ns, [loadLib], function(load) {
      var checkModuleResult = runtime.makePrimitiveAnn(runtime.getField(load, "types").ModuleResult);
      var loadInternalAPI = runtime.getField(load, "internal");

      function makeNamespace(runtimeNamespaceIsFor, ns) {
        
        function mergeAll(self, answer) {
          checkModuleResult(answer);
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

        var obj = rt.makeObject({
          "merge-all": rt.makeMethodFromFun(mergeAll)
        });
        return applyBrand(brandNamespace, obj);
      }

      return rt.makeObject({
        "provide-plus-types": rt.makeObject({
          types: {
            Namespace: annNamespace
          },
          values: {
            "empty-namespace": makeNamespace(runtime, namespace.namespace({})),
            "base-namespace": makeNamespace(runtime, runtime.namespace)
          },
          internal: {
            makeNamespace: makeNamespace
          }
        })
      });
    });
  });
});
