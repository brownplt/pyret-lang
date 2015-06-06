define(["js/runtime-util", "js/namespace"], function(util, namespace) {
return util.definePyretModule("namespace-lib",
  [util.modBuiltin("load-lib")],
  {
    values: ["make-empty-namespace", "make-base-namespace"],
    types: ["Namespace"]
  },
  function(runtime, ns, load) {
    var get = runtime.getField;
    function applyBrand(brand, val) {
      return get(brand, "brand").app(val);
    }

    var brandNamespace = runtime.namedBrander("namespace");
    var annNamespace = runtime.makeBranderAnn(brandNamespace, "Namespace");
    var checkNamespace = runtime.makePrimitiveAnn(annNamespace);
//      var checkModuleResult = function(v) { return runtime._checkAnn(["namespace"], runtime.getField(load, "types").ModuleResult, v); };
    var loadInternalAPI = runtime.getField(load, "internal");

    function makeNamespace(runtimeNamespaceIsFor, ns) {
      var obj = runtime.makeObject({
        "namespace": runtime.makeOpaque(ns)
      });
      return applyBrand(brandNamespace, obj);
    }

    function makeNamespaceFromResult(answer) {
      var runtimeOfAnswer = loadInternalAPI.getModuleResultRuntime(answer);
      var namespaceOfAnswer = loadInternalAPI.getModuleResultNamespace(answer);
      var typesOfAnswer = loadInternalAPI.getModuleResultDefinedTypes(answer);
      var valuesOfAnswer = loadInternalAPI.getModuleResultDefinedValues(answer);
      var newNamespace = namespaceOfAnswer;
      Object.keys(typesOfAnswer).forEach(function(k) {
        newNamespace = newNamespace.setType(k, typesOfAnswer[k]);
      });
      Object.keys(valuesOfAnswer).forEach(function(k) {
        newNamespace = newNamespace.set(k, valuesOfAnswer[k]);
      });
      return makeNamespace(runtimeOfAnswer, newNamespace);
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
          }),
          "make-namespace-from-result": runtime.makeFunction(makeNamespaceFromResult)
        }),
        internal: {
          makeNamespace: makeNamespace
        }
      })
    });
  });
});
