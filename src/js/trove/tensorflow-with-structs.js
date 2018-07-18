({
  requires: [
    { "import-type": "builtin", name: "tensorflow-structs" }
  ],
  nativeRequires: ["@tensorflow/tfjs"],
  provides: {
  },
  theModule: function(runtime, namespace, uri, tfStructs, tf) {

    /**
     * Runtime Helpers
     */
    var O = runtime.makeObject;
    var F = runtime.makeFunction;
    var arity = runtime.checkArity;
    var get = runtime.getField;
    var unwrap = runtime.unwrap;

    var sVals = get(tfStructs, "values");
    var sTyps = get(tfStructs, "types");


    var values = {
      "tensor": get(sVals, "tensor"),

      "generic": get(sVals, "generic"),
      "sequential": get(sVals, "sequential"),

      "activation": get(sVals, "activation"),
      "dense": get(sVals, "dense"),
      "dropout": get(sVals, "dropout"),
      "embedding": get(sVals, "embedding"),
      "flatten": get(sVals, "flatten"),
      "repeat-vector": get(sVals, "repeat-vector"),
      "reshape": get(sVals, "reshape"),

      "elu": get(sVals, "elu"),
      "hard-sigmoid": get(sVals, "hard-sigmoid"),
      "linear": get(sVals, "linear"),
      "relu": get(sVals, "relu"),
      "relu6": get(sVals, "relu6"),
      "selu": get(sVals, "selu"),
      "sigmoid": get(sVals, "sigmoid"),
      "softmax": get(sVals, "softmax"),
      "softplus": get(sVals, "softplus"),
      "softsign": get(sVals, "softsign"),
      "tanh": get(sVals, "tanh"),
      "other": get(sVals, "other"),
    };
    var types = {
      "Tensor": sTyps["Tensor"],
      "Model": sTyps["Model"],
      "Layer": sTyps["Layer"],
      "ActivationFunction": sTyps["ActivationFunction"],
    };

    return runtime.makeModuleReturn(values, types);
  }
})
