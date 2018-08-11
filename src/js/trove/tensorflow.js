({
  requires: [
    { "import-type": "builtin", name: "valueskeleton" }
  ],
  nativeRequires: ["@tensorflow/tfjs"],
  provides: {
    shorthands: {
      "Tensor": ["local", "Tensor"],
      "TensorBuffer": ["local", "TensorBuffer"],
      "Model": ["local", "Model"],
      "Sequential": ["local", "Sequential"],
      "SymbolicTensor": ["local", "SymbolicTensor"],
      "Layer": ["local", "Layer"],
      "Optimizer": ["local", "Optimizer"],
      "VS": {
        tag: "name",
        origin: { "import-type": "uri", uri: "builtin://valueskeleton" },
        name: "ValueSkeleton"
      },
      "Object": {
        tag: "name",
        origin: { "import-type": "uri", uri: "builtin://global" },
        name: "Object"
      },
      "NumInteger": {
        tag: "name",
        origin: { "import-type": "uri", uri: "builtin://global" },
        name: "NumInteger"
      },
      "NumPositive": {
        tag: "name",
        origin: { "import-type": "uri", uri: "builtin://global" },
        name: "NumPositive"
      },
      "Roughnum": {
        tag: "name",
        origin: { "import-type": "uri", uri: "builtin://global" },
        name: "Roughnum"
      },
      "TensorBinOp": ["arrow", [["local", "Tensor"], ["local", "Tensor"]], ["local", "Tensor"]],
      "TensorUnOp": ["arrow", [["local", "Tensor"]], ["local", "Tensor"]],
    },
    values: {
      // Tensors
      "is-tensor": ["arrow", ["Any"], "Boolean"],
      "list-to-tensor": ["arrow", [["List", "Number"]], "Tensor"],
      "make-scalar": ["arrow", ["Number"], "Tensor"],
      "fill": ["arrow", [["List", "NumInteger"], "Number"], "Tensor"],
      "tensor": ["Maker", "Number", ["local", "Tensor"]],
      "linspace": ["arrow", ["Number", "Number", "Number"], "Tensor"],
      "ones": ["arrow", [["List", "NumInteger"]], "Tensor"],
      "zeros": ["arrow", [["List", "NumInteger"]], "Tensor"],
      "multinomial": ["arrow", ["Tensor", "Number", ["Option", "Number"], "Boolean"], "Tensor"],
      "random-normal": ["arrow", [["List", "NumInteger"], ["Option", "Number"], ["Option", "Number"]], "Tensor"],
      "random-uniform": ["arrow", [["List", "NumInteger"], ["Option", "Number"], ["Option", "Number"]], "Tensor"],
      "make-variable": ["arrow", ["Tensor"], "Tensor"],

      // TensorBuffers
      "is-tensor-buffer": ["arrow", ["Any"], "Boolean"],
      "make-buffer": ["arrow", [["List", "NumInteger"]], "TensorBuffer"],

      // Tensor Operations (Arithmetic)
      "add-tensors": "TensorBinOp",
      "subtract-tensors": "TensorBinOp",
      "multiply-tensors": "TensorBinOp",
      "divide-tensors": "TensorBinOp",
      "floor-divide-tensors": "TensorBinOp",
      "tensor-max": "TensorBinOp",
      "tensor-min": "TensorBinOp",
      "tensor-modulo": "TensorBinOp",
      "tensor-expt": "TensorBinOp",
      "squared-difference": "TensorBinOp",
      "strict-add-tensors": "TensorBinOp",
      "strict-subtract-tensors": "TensorBinOp",
      "strict-multiply-tensors": "TensorBinOp",
      "strict-divide-tensors": "TensorBinOp",
      "strict-tensor-max": "TensorBinOp",
      "strict-tensor-min": "TensorBinOp",
      "strict-tensor-modulo": "TensorBinOp",
      "strict-tensor-expt": "TensorBinOp",
      "strict-squared-difference": "TensorBinOp",

      // Tensor Operations (Basic Math)
      "tensor-abs": "TensorUnOp",
      "tensor-acos": "TensorUnOp",
      "tensor-acosh": "TensorUnOp",
      "tensor-asin": "TensorUnOp",
      "tensor-asinh": "TensorUnOp",
      "tensor-atan": "TensorUnOp",
      "tensor-atan2": "TensorBinOp",
      "tensor-atanh": "TensorUnOp",
      "tensor-ceil": "TensorUnOp",
      "clip-by-value": ["arrow", ["Tensor", "Number", "Number"], "Tensor"],
      "tensor-cos": "TensorUnOp",
      "tensor-cosh": "TensorUnOp",
      "elu": "TensorUnOp",
      "exponential-linear-units": "TensorUnOp",
      "erf": "TensorUnOp",
      "gauss-error": "TensorUnOp",
      "tensor-exp": "TensorUnOp",
      "tensor-exp-min1": "TensorUnOp",
      "tensor-floor": "TensorUnOp",
      "leaky-relu": ["arrow", ["Tensor", "Number"], "Tensor"],
      "tensor-log": "TensorUnOp",
      "tensor-log-plus1": "TensorUnOp",
      "log-sigmoid": "TensorUnOp",
      "tensor-negate": "TensorUnOp",
      "parametric-relu": ["arrow", ["Tensor", "Number"], "Tensor"],
      "tensor-reciprocal": "TensorUnOp",
      "relu": "TensorUnOp",
      "tensor-round": "TensorUnOp",
      "reciprocal-sqrt": "TensorUnOp",
      "scaled-elu": "TensorUnOp",
      "sigmoid": "TensorUnOp",
      "signed-ones": "TensorUnOp",
      "tensor-sin": "TensorUnOp",
      "tensor-sinh": "TensorUnOp",
      "softplus": "TensorUnOp",
      "tensor-sqrt": "TensorUnOp",
      "tensor-square": "TensorUnOp",
      "step": "TensorUnOp",
      "tensor-tan": "TensorUnOp",
      "tensor-tanh": "TensorUnOp",

      // Operations (Reduction)
      "reduce-all": ["arrow", ["Tensor", ["Option", "Number"]], "Tensor"],
      "reduce-any": ["arrow", ["Tensor", ["Option", "Number"]], "Tensor"],
      "arg-max": ["arrow", ["Tensor", ["Option", "Number"]], "Tensor"],
      "arg-min": ["arrow", ["Tensor", ["Option", "Number"]], "Tensor"],
      "log-sum-exp": ["arrow", ["Tensor", ["Option", "Number"]], "Tensor"],
      "reduce-max": ["arrow", ["Tensor", ["Option", "Number"]], "Tensor"],
      "reduce-mean": ["arrow", ["Tensor", ["Option", "Number"]], "Tensor"],
      "reduce-min": ["arrow", ["Tensor", ["Option", "Number"]], "Tensor"],
      "reduce-sum": ["arrow", ["Tensor", ["Option", "Number"]], "Tensor"],

      // Operations (Slicing and Joining)
      "reshape": ["arrow", ["Tensor", ["List", "NumInteger"]], "Tensor"],
      "concatenate": ["arrow", [["List", "Tensor"], ["Option", "Number"]], "Tensor"],
      "gather": ["arrow", ["Tensor", "Tensor", ["Option", "Number"]], "Tensor"],
      "reverse": ["arrow", ["Tensor", ["Option", ["List", "Number"]]], "Tensor"],
      "slice": ["arrow", ["Tensor", ["List", "Number"], ["Option", ["List", "Number"]]], "Tensor"],
      "split": ["arrow", ["Tensor", ["List", "Number"], ["Option", "Number"]], "Tensor"],
      "stack": ["arrow", [["List", "Tensor"], ["Option", ["List", "Number"]]], "Tensor"],
      "tile": ["arrow", ["Tensor", ["List", "Number"]], "Tensor"],
      "unstack": ["arrow", ["Tensor", ["Option", ["List", "Number"]]], "Tensor"],
      "strided-slice": ["arrow", ["Tensor", ["List", "NumInteger"], ["List", "NumInteger"], ["List", "Number"]], "Tensor"],

      // Models (Generic)
      "is-model": ["arrow", ["Any"], "Boolean"],
      "make-model": ["arrow", ["Object"], "Model"],

      // Models (Sequential)
      "is-sequential": ["arrow", ["Any"], "Boolean"],
      "make-sequential": ["arrow", ["Sequential"], "Model"],

      // Models (Inputs / SymbolicTensors)
      "is-symbolic-tensor": ["arrow", ["Any"], "Boolean"],
      "make-input": ["arrow", [["List", ["Option", "Number"]]], "SymbolicTensor"],
      "make-batch-input": ["arrow", [["List", ["Option", "Number"]]], "SymbolicTensor"],

      // Layers
      "is-layer": ["arrow", ["Any"], "Boolean"],

      // Optimizers
      "is-optimizer": ["arrow", ["Any"], "Boolean"],
      "train-sgd": ["arrow", ["Number"], "Optimizer"],
      "train-momentum": ["arrow", ["Number", "Number"], "Optimizer"],
      "train-adagrad": ["arrow", ["Number", ["Option", "NumPositive"]], "Optimizer"],
      "train-adadelta": ["arrow", [["Option", "Number"], ["Option", "Number"], ["Option", "Number"]], "Optimizer"],
      "train-adam": ["arrow", [["Option", "Number"], ["Option", "Number"], ["Option", "Number"], ["Option", "Number"]], "Optimizer"],
      "train-adamax": ["arrow", [["Option", "Number"], ["Option", "Number"], ["Option", "Number"], ["Option", "Number"], ["Option", "Number"]], "Optimizer"],
      "train-rmsprop": ["arrow", ["Number", ["Option", "Number"], ["Option", "Number"], ["Option", "Number"], "Boolean"], "Optimizer"],
    },
    aliases: {
      "Tensor": {
        tag: "name",
        origin: { "import-type": "$ELF" },
        name: "Tensor"
      },
      "TensorBuffer": {
        tag: "name",
        origin: { "import-type": "$ELF" },
        name: "TensorBuffer"
      },
      "Model": {
        tag: "name",
        origin: { "import-type": "$ELF" },
        name: "Model"
      },
      "Sequential": {
        tag: "name",
        origin: { "import-type": "$ELF" },
        name: "Sequential"
      },
      "SymbolicTensor": {
        tag: "name",
        origin: { "import-type": "$ELF" },
        name: "SymbolicTensor"
      },
      "Layer": {
        tag: "name",
        origin: { "import-type": "$ELF" },
        name: "Layer"
      },
      "Optimizer": {
        tag: "name",
        origin: { "import-type": "$ELF" },
        name: "Optimizer"
      },
    },
    datatype: {
      "Tensor": ["data", "Tensor", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
        "size": ["arrow", [], "Number"],
        "shape": ["arrow", [], ["List", "NumInteger"]],
        "flatten": ["arrow", [], "Tensor"],
        "as-scalar": ["arrow", [], "Tensor"],
        "as-1d": ["arrow", [], "Tensor"],
        "as-2d": ["arrow", ["NumInteger", "NumInteger"], "Tensor"],
        "as-3d": ["arrow", ["NumInteger", "NumInteger", "NumInteger"], "Tensor"],
        "as-4d": ["arrow", ["NumInteger", "NumInteger", "NumInteger", "NumInteger"], "Tensor"],
        "as-type": ["arrow", ["String"], "Tensor"],
        "data-now": ["arrow", [], ["List", "Roughnum"]],
        "to-float": ["arrow", [], "Tensor"],
        "to-int": ["arrow", [], "Tensor"],
        "to-bool": ["arrow", [], "Tensor"],
        "to-buffer": ["arrow", [], "Tensor"],
        "to-variable": ["arrow", [], "Tensor"],
        "reshape": ["arrow", [["List", "NumInteger"]], "Tensor"],
        "expand-dims": ["arrow", [["Option", "NumInteger"]], "Tensor"],
        "squeeze": ["arrow", [["Option", ["List", "NumInteger"]]], "Tensor"],
        "clone": ["arrow", [], "Tensor"],
        "add": ["arrow", ["Tensor"], "Tensor"],
        "subtract": ["arrow", ["Tensor"], "Tensor"],
        "multiply": ["arrow", ["Tensor"], "Tensor"],
        "divide": ["arrow", ["Tensor"], "Tensor"],
        "floor-divide": ["arrow", ["Tensor"], "Tensor"],
        "max": ["arrow", ["Tensor"], "Tensor"],
        "min": ["arrow", ["Tensor"], "Tensor"],
        "modulo": ["arrow", ["Tensor"], "Tensor"],
        "expt": ["arrow", ["Tensor"], "Tensor"],
        "squared-difference": ["arrow", ["Tensor"], "Tensor"],
      }],
      "TensorBuffer": ["data", "TensorBuffer", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
        "size": ["arrow", [], "Number"],
        "shape": ["arrow", [], ["List", "NumInteger"]],
        "set-now": ["arrow", ["Number", ["List", "NumInteger"]], "TensorBuffer"],
        "get-now": ["arrow", [["List", "NumInteger"]], "TensorBuffer"],
        "get-all-now": ["arrow", [], ["List", "Roughnum"]],
        "to-tensor": ["arrow", [], "Tensor"],
      }],
      "Model": ["data", "Model", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
      }],
      "Sequential": ["data", "Sequential", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
        "add": ["arrow", ["Layer"], "Nothing"],
        "compile": ["arrow", ["Object"], "Nothing"],
        "evaluate": ["arrow", ["Tensor", "Tensor", "Object"], "Tensor"],
        "predict": ["arrow", ["Tensor", "Object"], "Tensor"],
        "predict-on-batch": ["arrow", ["Tensor"], "Tensor"],
        "fit": ["arrow", ["Tensor", "Tensor", "Object", ["arrow", ["Number", "Object"], "Nothing"]], "Nothing"],
      }],
      "SymbolicTensor": ["data", "SymbolicTensor", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
        "shape": ["arrow", [], ["List", ["Option", "Number"]]],
      }],
      "Layer": ["data", "Layer", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
      }],
      "Optimizer": ["data", "Optimizer", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
        "minimize": ["arrow", [["arrow", [], "Tensor"], ["List", "Tensor"]], "Tensor"],
      }],
    }
  },
  theModule: function(runtime, namespace, uri, VSlib, tf) {

    /**
     * Tensorflow Brands and Annotations
     */

    /**
     * The Pyret version of a TensorFlow.js Tensor.
     * @typedef {Object} PyretTensor
     */
    const brandTensor = runtime.namedBrander("tensor", ["tensor: tensor brander"]);
    const annTensor = runtime.makeBranderAnn(brandTensor, "Tensor");

    /**
     * The Pyret version of a TensorFlow.js TensorBuffer.
     * @typedef {Object} PyretTensorBuffer
     */
    const brandTensorBuffer = runtime.namedBrander(
      "tensor-buffer",
      ["tensor-buffer: tensor-buffer brander"]);
    const annTensorBuffer = runtime.makeBranderAnn(brandTensorBuffer, "TensorBuffer");

    /**
     * The Pyret version of a TensorFlow.js Model.
     * @typedef {Object} PyretModel
     */
    const brandModel = runtime.namedBrander("model", ["model: model brander"]);
    const annModel = runtime.makeBranderAnn(brandModel, "Model");

    /**
     * The Pyret version of a TensorFlow.js Sequential.
     * @typedef {Object} PyretSequential
     */
    const brandSequential = runtime.namedBrander(
      "sequential",
      ["sequential: sequential brander"]);
    const annSequential = runtime.makeBranderAnn(brandSequential, "Sequential");

    /**
     * The Pyret version of a TensorFlow.js SymbolicTensor.
     * @typedef {Object} PyretSymbolicTensor
     */
    const brandSymbolicTensor = runtime.namedBrander(
      "symbolic-tensor",
      ["symbolic-tensor: symbolic-tensor brander"]);
    const annSymbolicTensor = runtime.makeBranderAnn(brandSymbolicTensor, "SymbolicTensor");

    /**
     * The Pyret version of a TensorFlow.js Layer.
     * @typedef {Object} PyretLayer
     */
    const brandLayer = runtime.namedBrander("layer", ["layer: layer brander"]);
    const annLayer = runtime.makeBranderAnn(brandLayer, "Layer");

    /**
     * The Pyret version of a TensorFlow.js Optimizer.
     * @typedef {Object} PyretOptimizer
     */
    const brandOptimizer = runtime.namedBrander(
      "optimizer",
      ["optimizer: optimizer brander"]);
    const annOptimizer = runtime.makeBranderAnn(brandOptimizer, "Optimizer");

    /**
     * Runtime Helpers
     */
    const O = runtime.makeObject;
    const F = runtime.makeFunction;
    const arity = runtime.checkArity;
    const get = runtime.getField;
    const unwrap = runtime.unwrap;

    const VS = get(VSlib, "values");

    function applyBrand(brand, val) {
      return get(brand, "brand").app(val);
    }
    function hasBrand(brand, val) {
      return get(brand, "test").app(val);
    }
    function assertOption(val) {
      if (!runtime.ffi.isOption(val)) {
        runtime.ffi.throwTypeMismatch(val, "Option");
      }
    }

    /**
     * Helpers
     */

    const unwrapObject = (obj) => { return unwrap(obj).dict; };

    const checkTensor = (val) => { runtime._checkAnn(["tensor"], annTensor, val); };
    const checkTensorBuffer = (val) => { runtime._checkAnn(["tensor-buffer"], annTensorBuffer, val); };
    const checkSequential = (val) => { runtime._checkAnn(["sequential"], annSequential, val); };
    const checkModel = (val) => { runtime._checkAnn(["model"], annModel, val); };
    const checkSymbolicTensor = (val) => { runtime._checkAnn(["symbolic-tensor"], annSymbolicTensor, val); };
    const checkLayer = (val) => { runtime._checkAnn(["layer"], annLayer, val); };
    const checkOptimizer = (val) => { runtime._checkAnn(["optimizer"], annOptimizer, val); };

    function checkMethodArity(arity, args, methodName) {
      if (args.length !== arity) {
        let $a = new Array(args.length);
        for (let $i = 0; $i < args.length; $i++) {
          $a[$i] = args[$i];
        }
        throw runtime.ffi.throwArityErrorC([methodName], arity, $a, true);
      }
    }

    /**
     * Helper function to unwrap a Option<Number>. If `some`, then returns a
     * fixnum variant of the value of the Option. If `none`, returns undefined.
     * The undefined return value is required for certain TensorFlow.js
     * functions.
     * @param {Option<Number>} option The Option to unwrap
     * @returns {Number|undefined} The fixnum variant of the Option's value
     *  or undefined
     */
    function unwrapFixnumOption(option) {
      return runtime.ffi.cases(runtime.ffi.isOption, "is-Option", option, {
        some: (v) => { return runtime.num_to_fixnum(v); },
        none: () => { return undefined; }
      });
    }

    /**
     * Helper function to convert a List<PyretTensor> to an Array[TFTensor].
     * Raises an error if any element in the input list is not a PyretTensor,
     * or if the input is not a List.
     * @param {List<PyretTensor>} listOfTensors The tensors to unwrap
     * @returns {Array[TFTensor]} The underlying TensorFlow.js representations
     *  of each of the input Tensors
     */
    function checkAndUnwrapListToArray(pyretList, checkType, converter) {
      runtime.checkList(pyretList);
      const array = runtime.ffi.toArray(pyretList);
      return array.map(x => {
        checkType(x);
        return converter(x);
      });
    }

    /**
     * Helper function to convert a List<PyretTensor> to an Array[TFTensor].
     * Raises an error if any element in the input list is not a PyretTensor,
     * or if the input is not a List.
     * @param {List<PyretTensor>} listOfTensors The tensors to unwrap
     * @returns {Array[TFTensor]} The underlying TensorFlow.js representations
     *  of each of the input Tensors
     */
    function unwrapListOfTensorsToArray(listOfTensors) {
      const typeChecker = checkTensor;
      const converter   = unwrapTensor;
      return checkAndUnwrapListToArray(listOfTensors, checkTensor, unwrapTensor);
    }

    /**
     * Helper function to convert a List<PyretLayer> to an Array[TFLayer].
     * Raises an error if any element in the input list is not a PyretLayer,
     * or if the input is not a List.
     * @param {List<PyretLayer>} listOfTensors The tensors to unwrap
     * @returns {Array[TFLayer]} The underlying TensorFlow.js representations
     *  of each of the input Layers
     */
    function unwrapListOfLayersToArray(listOfLayers) {
      const typeChecker = checkLayer;
      const converter   = unwrapLayer;
      return checkAndUnwrapListToArray(v, checkLayer, unwrapLayer);
    }

    /**
     * Helper function to convert a List<NumInteger> to an Array[Number].
     * Raises an error if any element in the input list is not a NumInteger,
     * or if the input is not a List.
     * @param {List<PyretTensor>} listOfIntegers The integers to unwrap
     * @param {Function(PyretNumber):JSNumber} [typeChecker=runtime.checkNumber]
     *  Optional parameter to specify which type checking function should be
     *  called on every element of the input list
     * @returns {Array[Number]} An array of numbers
     */
    function unwrapListOfNumbersToArray(listOfIntegers, typeChecker) {
      if (!typeChecker) {
        typeChecker = runtime.checkNumber;
      }
      const converter = runtime.num_to_fixnum;
      return checkAndUnwrapListToArray(listOfIntegers, typeChecker, converter);
    }

    /**
     * Helper function to convert a List<Option<Number>> to an Array. Raises
     * an error if any element in the input list is not an Option, or if the
     * input is not a List.
     * @param {List<PyretTensor>} listOfOptions The Options to unwrap
     * @param {Any} noneValue The value to include in the final array when an
     *  Option is of the `none` variant
     * @returns {Array[Number|Any]} An array of numbers (and possibly
     *  `noneValue`s)
     */
    function checkAndUnwrapListOfOptionNumbers(listOfOptions, noneValue) {
      return checkAndUnwrapListToArray(listOfOptions, _ => {}, option => {
        let num = unwrapFixnumOption(option);
        if (num === undefined) {
          num = noneValue;
        }
        return num;
      });
    }

    /**
     * Tensors
     */

    /**
     * Returns PyretTrue if the input `obj` is a PyretTensor; otherwise,
     * returns PyretFalse.
     * @param {Any} obj Some Pyret value
     * @returns {PBoolean} A Pyret object representing true or false
     */
    function isTensor(obj) {
      arity(1, arguments, "is-tensor", false);
      return runtime.makeBoolean(hasBrand(brandTensor, obj));
    }

    /**
     * Consumes a PyretTensor and returns its underlying TensorFlow.js
     * Tensor.
     * @param {PyretTensor} pyretTensor
     * @returns {TFTensor} The underlying TensorFlow.js Tensor of the
     *  input PyretTensor
     */
    function unwrapTensor(pyretTensor) {
      return pyretTensor.$underlyingTensor;
    }

    /**
     * Helper function to type-check and unwrap a PyretTensor to a
     * TFTensor. Raises an error if the input is not a PyretTensor.
     * @param {PyretTensor} pyretTensor
     * @returns {TFTensor} The underlying TensorFlow.js Tensor of the
     *  input PyretTensor
     */
    function checkAndUnwrapTensor(pyretTensor) {
      checkTensor(pyretTensor);
      return unwrapTensor(pyretTensor);
    }

    /**
     * Consumes a TFTensor and wraps it in a PyretObject to make it a
     * PyretTensor.
     * @param {TFTensor} underlyingBuffer A TensorFlow.js Tensor
     * @returns {PyretTensor} A new PyretTensor with the input as its
     *  underlying TFTensor
     */
    function buildTensorObject(underlyingTensor) {
      let obj = O({
        "_output": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "_output");
          // value-skeleton functions:
          const vsValue      = get(VS, "vs-value");
          const vsCollection = get(VS, "vs-collection");
          // Extract tensor information:
          const selfTensor = unwrapTensor(self);
          const tensorData = Array.from(selfTensor.dataSync());
          // Create an array of value-skeleton values for each data point in
          // this tensor:
          let tensorElts = [];
          for (let i = 0; i < tensorData.length; i++) {
            const pyretNum = runtime.num_to_roughnum(tensorData[i]);
            tensorElts.push(vsValue.app(pyretNum));
          }
          // Output value-skeleton collection:
          const tensorName = runtime.makeString("tensor");
          const eltList    = runtime.ffi.makeList(tensorElts);
          return vsCollection.app(tensorName, eltList);
        }),
        "size": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "size");
          const selfTensor = unwrapTensor(self);
          return runtime.makeNumber(selfTensor.size);
        }),
        "shape": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "shape");
          const selfTensor = unwrapTensor(self);
          return runtime.ffi.makeList(selfTensor.shape);
        }),
        "flatten": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "flatten");
          const selfTensor = unwrapTensor(self);
          return buildTensorObject(selfTensor.flatten());
        }),
        "as-scalar": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "as-scalar");
          const selfTensor = unwrapTensor(self);
          if (selfTensor.size !== 1) {
            runtime.ffi.throwMessageException("Tensor was size-" +
              selfTensor.size + " but `as-scalar` requires the " +
              "tensor to be size-1");
          }
          return buildTensorObject(selfTensor.asScalar());
        }),
        "as-1d": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "as-1d");
          const selfTensor = unwrapTensor(self);
          return buildTensorObject(selfTensor.as1D());
        }),
        "as-2d": runtime.makeMethod2(function(self, rows, columns) {
          checkMethodArity(3, arguments, "as-2d");
          const shapeList = runtime.ffi.makeList([rows, columns]);
          return reshapeTensor(self, shapeList);
        }),
        "as-3d": runtime.makeMethod3(function(self, rows, columns, depth) {
          checkMethodArity(4, arguments, "as-3d");
          const shapeList = runtime.ffi.makeList([rows, columns, depth]);
          return reshapeTensor(self, shapeList);
        }),
        "as-4d": runtime.makeMethod4(function(self, rows, columns, depth1, depth2) {
          checkMethodArity(5, arguments, "as-4d");
          const shapeList = runtime.ffi.makeList([rows, columns, depth1, depth2]);
          return reshapeTensor(self, shapeList);
        }),
        "as-type": runtime.makeMethod1(function(self, datatype) {
          checkMethodArity(2, arguments, "as-type");
          runtime.checkString(datatype);
          const type = unwrap(datatype);
          if (type !== "float32" && type !== "int32" && type !== "bool") {
            runtime.ffi.throwMessageException("Attempted to cast tensor to " +
              "invalid type ('" + type + "'); valid types are 'float32', " +
              "'int32', or 'bool'");
          }
          const selfTensor = unwrapTensor(self);
          return buildTensorObject(selfTensor.asType(type));
        }),
        "to-buffer": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-buffer");
          const selfTensor = unwrapTensor(self);
          const newBuffer  = selfTensor.buffer();
          return buildTensorBufferObject(newBuffer);
        }),
        "data-now": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "data-now");
          const selfTensor = unwrapTensor(self);
          // .dataSync returns a TypedArray, so convert it to a normal JSArray
          // so we can then convert it to a Pyret List:
          const tensorData = Array.from(selfTensor.dataSync());
          // Convert to Roughnums, since the numbers returned from a Tensor are
          // floating point:
          const roughnumData = tensorData.map(x => runtime.num_to_roughnum(x));
          return runtime.ffi.makeList(roughnumData);
        }),
        "to-float": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-float");
          const selfTensor = unwrapTensor(self);
          return buildTensorObject(selfTensor.toFloat());
        }),
        "to-int": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-int");
          const selfTensor = unwrapTensor(self);
          return buildTensorObject(selfTensor.toInt());
        }),
        "to-bool": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-bool");
          const selfTensor = unwrapTensor(self);
          return buildTensorObject(selfTensor.toBool());
        }),
        "to-variable": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-variable");
          return makeVariable(self);
        }),
        "reshape": runtime.makeMethod0(function(self, newShape) {
          checkMethodArity(2, arguments, "reshape");
          return reshapeTensor(self, newShape);
        }),
        "expand-dims": runtime.makeMethod1(function(self, axis) {
          checkMethodArity(2, arguments, "expand-dims");
          const selfTensor = unwrapTensor(self);
          const jsAxis     = unwrapFixnumOption(axis);
          // The rank of the tensor is equal to its dimensions:
          const tensorRank = selfTensor.shape.length;
          // Check that, if the axis was specified, that the input axis is less
          // than or equal to the dimensions of the current tensor:
          if (jsAxis && jsAxis > tensorRank) {
            runtime.ffi.throwMessageException("Cannot expand dimensions " +
              "because the input axis must be less than or equal to the rank " +
              "of the tensor (tensor was rank-" + tensorRank + " but the " +
              "input axis was " + jsAxis + ")");
          }
          return buildTensorObject(selfTensor.expandDims(jsAxis));
        }),
        "squeeze": runtime.makeMethod1(function(self, axes) {
          checkMethodArity(2, arguments, "squeeze");
          const selfTensor  = unwrapTensor(self);
          const tensorShape = selfTensor.shape;
          const tensorRank  = tensorShape.length;
          const jsAxes =
            runtime.ffi.cases(runtime.ffi.isOption, "is-Option", axes, {
              some: (v) => {
                runtime.checkList(v);
                return runtime.ffi.toArray(v).map((x) => {
                  runtime.checkNumInteger(x);
                  const axis = runtime.num_to_fixnum(x);
                  // Axes are zero-indexed, so offset by 1:
                  if ((axis + 1) > tensorRank) {
                    runtime.ffi.throwMessageException("Cannot squeeze axis " +
                      axis + " since the axis does not exist in a tensor of " +
                      "rank " + tensorRank);
                  }
                  else if (tensorShape[axis] !== 1) {
                    runtime.ffi.throwMessageException("Cannot squeeze axis " +
                      axis + " since the dimension of that axis is " +
                      tensorShape[axis] + ", not 1");
                  }
                  return axis;
                });
              },
              none: () => { return undefined; }
            });
          return buildTensorObject(selfTensor.squeeze(jsAxes));
        }),
        "clone": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "clone");
          const selfTensor = unwrapTensor(self);
          return buildTensorObject(selfTensor.clone());
        }),
        "add": runtime.makeMethod1(function(self, b) {
          checkMethodArity(2, arguments, "add");
          return addTensors(self, b);
        }),
        "subtract": runtime.makeMethod1(function(self, b) {
          checkMethodArity(2, arguments, "subtract");
          return subtractTensors(self, b);
        }),
        "multiply": runtime.makeMethod1(function(self, b) {
          checkMethodArity(2, arguments, "multiply");
          return multiplyTensors(self, b);
        }),
        "divide": runtime.makeMethod1(function(self, b) {
          checkMethodArity(2, arguments, "divide");
          return divideTensors(self, b);
        }),
        "floor-divide": runtime.makeMethod1(function(self, b) {
          checkMethodArity(2, arguments, "floor-divide");
          return floorDivideTensors(self, b);
        }),
        "max": runtime.makeMethod1(function(self, b) {
          checkMethodArity(2, arguments, "max");
          return maxTensor(self, b);
        }),
        "min": runtime.makeMethod1(function(self, b) {
          checkMethodArity(2, arguments, "min");
          return minTensor(self, b);
        }),
        "modulo": runtime.makeMethod1(function(self, b) {
          checkMethodArity(2, arguments, "modulo");
          return moduloTensor(self, b);
        }),
        "expt": runtime.makeMethod1(function(self, b) {
          checkMethodArity(2, arguments, "expt");
          return exptTensor(self, b);
        }),
        "squared-difference": runtime.makeMethod1(function(self, b) {
          checkMethodArity(2, arguments, "squared-difference");
          return tensorSquaredDifference(self, b);
        }),
      });
      obj = applyBrand(brandTensor, obj);
      obj.$underlyingTensor = underlyingTensor;
      return obj;
    }

    /**
     * Creates a PyretTensor with the given values.
     * @param {JSArray<PyretNumber>} array An array of types
     * @returns {PyretTensor} A Tensor
     */
    function createTensorFromArray(array) {
      arity(1, arguments, "tensor", false);
      runtime.checkArray(array);
      if (array.length < 1) {
        runtime.throwMessageException("A tensor must contain at least 1 value");
      }
      // Verify that each element is a Number and convert it to its JavaScript
      // equivalent:
      const fixnums = array.map((x) => {
        runtime.checkNumber(x);
        return runtime.num_to_fixnum(x);
      });
      const underlyingTensor = tf.tensor(fixnums, null, null);
      return buildTensorObject(underlyingTensor);
    }

    /**
     * Creates a PyretTensor with no values.
     * @returns {PyretTensor} A PyretTensor with no values
     */
    function createTensor0() {
      arity(0, arguments, "tensor0", false);
      return createTensorFromArray([]);
    }

    /**
     * Creates a PyretTensor with one value.
     * @param {Number} a The first value to put in the Tensor
     * @returns {PyretTensor} A PyretTensor with the value a
     */
    function createTensor1(a) {
      arity(1, arguments, "tensor1", false);
      runtime.checkNumber(a);
      return createTensorFromArray([a]);
    }

    /**
     * Creates a PyretTensor with two values.
     * @param {Number} a The first value to put in the Tensor
     * @param {Number} b The second value to put in the Tensor
     * @returns {PyretTensor} A PyretTensor with the values a and b
     */
    function createTensor2(a, b) {
      arity(2, arguments, "tensor2", false);
      runtime.checkNumber(a);
      runtime.checkNumber(b);
      return createTensorFromArray([a, b]);
    }

    /**
     * Creates a PyretTensor with three values.
     * @param {Number} a The first value to put in the Tensor
     * @param {Number} b The second value to put in the Tensor
     * @param {Number} c The third value to put in the Tensor
     * @returns {PyretTensor} A PyretTensor with the values a, b, and c
     */
    function createTensor3(a, b, c) {
      arity(3, arguments, "tensor3", false);
      runtime.checkNumber(a);
      runtime.checkNumber(b);
      runtime.checkNumber(c);
      return createTensorFromArray([a, b, c]);
    }

    /**
     * Creates a PyretTensor with four values.
     * @param {Number} a The first value to put in the Tensor
     * @param {Number} b The second value to put in the Tensor
     * @param {Number} c The third value to put in the Tensor
     * @param {Number} d The fourth value to put in the Tensor
     * @returns {PyretTensor} A PyretTensor with the values a, b, c, and d
     */
    function createTensor4(a, b, c, d) {
      arity(4, arguments, "tensor4", false);
      runtime.checkNumber(a);
      runtime.checkNumber(b);
      runtime.checkNumber(c);
      runtime.checkNumber(d);
      return createTensorFromArray([a, b, c, d]);
    }

    /**
     * Creates a PyretTensor with five values.
     * @param {Number} a The first value to put in the Tensor
     * @param {Number} b The second value to put in the Tensor
     * @param {Number} c The third value to put in the Tensor
     * @param {Number} d The fourth value to put in the Tensor
     * @param {Number} e The fifth value to put in the Tensor
     * @returns {PyretTensor} A PyretTensor with the values a, b, c, d, e
     */
    function createTensor5(a, b, c, d, e) {
      arity(5, arguments, "tensor5", false);
      runtime.checkNumber(a);
      runtime.checkNumber(b);
      runtime.checkNumber(c);
      runtime.checkNumber(d);
      runtime.checkNumber(e);
      return createTensorFromArray([a, b, c, d, e]);
    }

    /**
     * Creates a PyretTensor with the values in the input List.
     * @param {List<Number>} values A List of values to put in the Tensor
     * @returns {PyretTensor} A PyretTensor with the values in the input List
     */
    function listToTensor(values) {
      arity(1, arguments, "list-to-tensor", false);
      runtime.checkList(values);
      const array = runtime.ffi.toArray(values);
      return createTensorFromArray(array);
    }

    /**
     * Creates a PyretTensor of rank-0 with the input value.
     * @param {Number} value The value to put into the Tensor
     * @returns {PyretTensor} A PyretTensor with the input value
     */
    function makeScalar(value) {
      arity(1, arguments, "make-scalar", false);
      runtime.checkNumber(value);
      const fixnum    = runtime.num_to_fixnum(value);
      const newScalar = tf.scalar(fixnum);
      return buildTensorObject(newScalar);
    }

    /**
     * Creates a PyretTensor filled with the input value.
     * @param {List<NumInteger>} shape The shape of the new Tensor
     * @param {Number} value The value to populate all entries of the new
     *  Tensor with
     * @returns {PyretTensor} A PyretTensor filled with the input value
     */
    function fill(shape, value) {
      arity(2, arguments, "fill", false);
      runtime.checkNumber(value);
      const jsShape = unwrapListOfNumbersToArray(shape, runtime.checkNumInteger);
      const jsValue = runtime.num_to_fixnum(value);
      return buildTensorObject(tf.fill(jsShape, jsValue));
    }

    /**
     * Returns a PyretTensor whose values are an evenly spaced sequence of
     * numbers over the given interval.
     * @param {Number} startRange The start value of the sequence
     * @param {Number} stopRange The end value of the sequence
     * @param {Number} numValues The number of values to generate
     * @returns {PyretTensor} A one-dimensional PyretTensor whose values are
     *  spread across the input range
     */
    function linspace(startRange, stopRange, numValues) {
      arity(3, arguments, "linspace", false);
      runtime.checkNumber(startRange);
      runtime.checkNumber(stopRange);
      runtime.checkNumber(numValues);
      const start = runtime.num_to_fixnum(startRange);
      const stop  = runtime.num_to_fixnum(stopRange);
      const nums  = runtime.num_to_fixnum(numValues);
      return buildTensorObject(tf.linspace(start, stop, nums));
    }

    /**
     * Returns a PyretTensor with the given shape where every entry is a 1.
     * @param {List<NumInteger>} shape The shape of the output tensor
     * @returns {PyretTensor} A PyretTensor of ones with the input shape
     */
    function ones(shape) {
      arity(1, arguments, "ones", false);
      const jsShape = unwrapListOfNumbersToArray(shape, runtime.checkNumInteger);
      return buildTensorObject(tf.ones(jsShape));
    }

    /**
     * Returns a PyretTensor with the given shape where every entry is a 0.
     * @param {List<NumInteger>} shape The shape of the output tensor
     * @returns {PyretTensor} A PyretTensor of zeros with the input shape
     */
    function zeros(shape) {
      arity(1, arguments, "zeros", false);
      const jsShape = unwrapListOfNumbersToArray(shape, runtime.checkNumInteger);
      return buildTensorObject(tf.zeros(jsShape));
    }

    /**
     * Creates a PyretTensor where all of the values are sampled from a
     * multinomial distribution.
     * @param {PyretTensor} logits An one-dimensional Tensor representing
     *  unnormalized log-probabilities
     * @param {NumPositive} numSamples The number of samples to draw for each row
     *  slice
     * @param {Option<Number>} seed The randomization seed to use; if none,
     *  uses TensorFlow.js defaults
     * @param {Boolean} normalized Whether the provided logits are normalized
     *  true probabilities (sum to 1)
     * @returns {PyretTensor}
     */
    function multinomial(logits, numSamples, randomSeed, normalized) {
      arity(4, arguments, "multinomial", false);
      runtime.checkNumPositive(numSamples);
      runtime.checkBoolean(normalized);
      const tensor  = checkAndUnwrapTensor(logits);
      const samples = runtime.num_to_fixnum(numSamples);
      const seed    = unwrapFixnumOption(randomSeed);
      const norm    = runtime.isPyretTrue(normalized);
      // Check if `logits` is 1D or 2D, as required by the `multinomial`
      // function:
      const dimensions = tensor.shape.length;
      if (dimensions !== 1 && dimensions !== 2) {
        runtime.ffi.throwMessageException("The `logits` argument must be a " +
          "one-dimensional or two-dimensional Tensor (had " + dimensions + " " +
          "dimensions).");
      }
      // The multinomial distribution needs at least two possible outcomes:
      if (tensor.size < 2) {
        runtime.ffi.throwMessageException("The `logits` argument must have " +
          "at least two possible outcomes (had " + tensor.size + ").");
      }
      return buildTensorObject(tf.multinomial(tensor, samples, seed, norm));
    }

    /**
     * Creates a PyretTensor with values sampled from a normal distribution.
     * @param {List<NumInteger>} shape A List<Number> defining the Tensor's shape
     * @param {Option<Number>} mean The mean of the normal distribution;
     *  if `none`, set to the TensorFlow.js default
     * @param {Option<Number>} standardDeviation The standard deviation of the
     *  normal distribution; if `none`, set to the TensorFlow.js default
     * @returns {PyretTensor} A PyretTensor with the input value
     */
    function randomNormal(shape, mean, standardDeviation) {
      arity(3, arguments, "random-normal", false);
      const jsShape  = unwrapListOfNumbersToArray(shape, runtime.checkNumInteger);
      const jsMean   = unwrapFixnumOption(mean);
      const jsStdDev = unwrapFixnumOption(standardDeviation);
      return buildTensorObject(tf.randomNormal(jsShape, jsMean, jsStdDev));
    }

    /**
     * Creates a PyretTensor with values sampled from a uniform distribution.
     * @param {List<NumInteger>} shape A List<Number> defining the Tensor's shape
     * @param {Option<Number>} minVal The lower bound on the range of random
     *  values to generate; if `none`, set to the TensorFlow.js default
     * @param {Option<Number>} maxVal The upper bound on the range
     *  of random values to generate; if `none`, set to the TensorFlow.js
     *  default
     * @returns {PyretTensor} A PyretTensor with the input value
     */
    function randomUniform(shape, minVal, maxVal) {
      arity(3, arguments, "random-uniform", false);
      const jsShape  = unwrapListOfNumbersToArray(shape, runtime.checkNumInteger);
      const jsMinVal = unwrapFixnumOption(minVal);
      const jsMaxVal = unwrapFixnumOption(maxVal)
      return buildTensorObject(tf.randomUniform(jsShape, jsMinVal, jsMaxVal));
    }

    /**
     * Creates a mutable PyretTensor from the input PyretTensor.
     * @param {PyretTensor} tensor A PyretTensor
     * @returns {PyretTensor} A mutable PyretTensor
     */
    function makeVariable(tensor) {
      arity(1, arguments, "make-variable", false);
      const jsTensor    = checkAndUnwrapTensor(tensor);
      const newVariable = tf.variable(jsTensor);
      return buildTensorObject(newVariable);
    }

    /**
     * TensorBuffers
     */

    /**
     * Returns PyretTrue if the input `obj` is a TensorBuffer; otherwise,
     * returns PyretFalse.
     * @param {Any} obj Some Pyret value
     * @returns {PBoolean} A Pyret object representing true or false
     */
    function isTensorBuffer(obj) {
      arity(1, arguments, "is-tensor-buffer", false);
      return runtime.makeBoolean(hasBrand(brandTensorBuffer, obj));
    }

    /**
     * Consumes a PyretTensorBuffer and returns its underlying TensorFlow.js
     * TensorBuffer.
     * @param {PyretTensorBuffer} pyretTensorBuffer
     * @returns {TFTensorBuffer} The underlying TensorFlow.js TensorBuffer
     *  of the input PyretTensorBuffer
     */
    function unwrapTensorBuffer(pyretTensorBuffer) {
      return pyretTensorBuffer.$underlyingBuffer;
    }

    /**
     * Consumes a TFTensorBuffer and wraps it in a PyretObject to make it a
     * PyretTensorBuffer.
     * @param {TFTensorBuffer} underlyingBuffer A TensorFlow.js TensorBuffer
     * @returns {PyretTensorBuffer} A new PyretTensorBuffer with the input
     *  as its underlying TFTensorBuffer
     */
    function buildTensorBufferObject(underlyingBuffer) {
      let obj = O({
        "_output": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "_output");
          const selfBuffer   = unwrapTensorBuffer(self);
          const bufferData   = Array.from(selfBuffer.values);
          const vsValue      = get(VS, "vs-value");
          const vsCollection = get(VS, "vs-collection");
          let elts = [];
          for (let i = 0; i < bufferData.length; i++) {
            const wrappedNum = runtime.num_to_roughnum(bufferData[i]);
            elts.push(vsValue.app(wrappedNum));
          }
          return vsCollection.app(
            runtime.makeString("tensor-buffer"),
            runtime.ffi.makeList(elts));
        }),
        "size": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "size");
          const selfBuffer = unwrapTensorBuffer(self);
          return runtime.makeNumber(selfBuffer.size);
        }),
        "shape": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "shape");
          const selfBuffer = unwrapTensorBuffer(self);
          return runtime.ffi.makeList(selfBuffer.shape);
        }),
        "set-now": runtime.makeMethod2(function(self, value, locs) {
          checkMethodArity(3, arguments, "set-now");
          runtime.checkNumber(value);
          const val         = runtime.num_to_fixnum(value);
          const locations   = unwrapListOfNumbersToArray(locs);
          const selfBuffer  = unwrapTensorBuffer(self);
          const bufferShape = selfBuffer.shape;
          const bufferRank  = bufferShape.length;
          // Check that the correct number of coordinates was supplied:
          if (bufferRank !== locations.length) {
            runtime.ffi.throwMessageException("The number of supplied " +
              "coordinates must match the rank of the TensorBuffer, but " +
              locations.length + " coordinates were specified and the number " +
              "of dimensions in the TensorBuffer was " + bufferRank + ".");
          }
          // Check that each coordinate in `locations` is within the bounds of
          // the buffer shape:
          for (let axis = 0; axis < bufferRank; axis++) {
            const locationIndex = locations[axis];
            const dimensionSize = bufferShape[axis];
            if (locationIndex < 0 || locationIndex >= dimensionSize) {
              runtime.ffi.throwMessageException("The coordinate at axis " +
                axis + " was " + locationIndex + ", but the size of that " +
                "dimension in the TensorBuffer was " + dimensionSize + ". " +
                "Coordinates must be within the bounds of the TensorBuffer's " +
                "shape.");
            }
          }
          // Actually set the values in the buffer:
          selfBuffer.set(val, ...locations);
          return runtime.makeNothing();
        }),
        "get-now": runtime.makeMethod1(function(self, locs) {
          checkMethodArity(2, arguments, "get-now");
          const locations   = unwrapListOfNumbersToArray(locs);
          const selfBuffer  = unwrapTensorBuffer(self);
          const bufferShape = selfBuffer.shape;
          const bufferRank  = bufferShape.length;
          // Check that the correct number of coordinates was supplied:
          if (bufferRank !== locations.length) {
            runtime.ffi.throwMessageException("The number of supplied " +
              "coordinates must match the rank of the TensorBuffer, but " +
              locations.length + " coordinates were specified and the number " +
              "of dimensions in the TensorBuffer was " + bufferRank + ".");
          }
          // Check that each coordinate in `locations` is within the bounds of
          // the buffer shape:
          for (let axis = 0; axis < bufferRank; axis++) {
            const locationIndex = locations[axis];
            const dimensionSize = bufferShape[axis];
            if (locationIndex < 0 || locationIndex >= dimensionSize) {
              runtime.ffi.throwMessageException("The coordinate at axis " +
                axis + " was " + locationIndex + ", but the size of that " +
                "dimension in the TensorBuffer was " + dimensionSize + ". " +
                "Coordinates must be within the bounds of the TensorBuffer's " +
                "shape.");
            }
          }
          // The ... spread/splat syntax is required as .get requires the
          // indices to be entered as separate arguments to the function:
          const result = selfBuffer.get(...locations);
          return runtime.makeNumber(result);
        }),
        "get-all-now": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "get-all-now");
          const selfBuffer = unwrapTensorBuffer(self);
          const bufferData = Array.from(selfBuffer.values);
          const roughnums  = bufferData.map(x => runtime.num_to_roughnum(x));
          return runtime.ffi.makeList(roughnums);
        }),
        "to-tensor": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-tensor");
          const selfBuffer = unwrapTensorBuffer(self);
          return buildTensorObject(selfBuffer.toTensor());
        })
      });
      obj = applyBrand(brandTensorBuffer, obj);
      obj.$underlyingBuffer = underlyingBuffer;
      return obj;
    }

    /**
     * Returns a new PyretTensorBuffer with the input shape.
     * @param {List<NumInteger>} shape The dimensions for the new TensorBuffer
     * @returns {PyretTensorBuffer}
     */
    function makeBuffer(shape) {
      arity(1, arguments, "make-buffer", false);
      runtime.checkList(shape);
      const jsShape = unwrapListOfNumbersToArray(shape, runtime.checkNumInteger);
      if (jsShape.length <= 0) {
        runtime.ffi.throwMessageException("A TensorBuffer's shape needs at " +
          "least 1 dimension, but the input shape List had zero elements.");
      }
      jsShape.forEach((dimensionSize, dimensionNum) => {
        if (dimensionSize <= 0) {
          runtime.ffi.throwMessageException("Cannot create TensorBuffer " +
            "since the size at dimension " + dimensionNum + " was less than  " +
            "or equal to zero; all dimensions must be at least 1.");
        }
      });
      return buildTensorBufferObject(tf.buffer(jsShape));
    }

    /**
     * Operations (Arithmetic)
     */

    /**
     * Applies the specified binary function to the given tensor. Used as a
     * helper function for math operations on Tensors since the format is
     * equivalent for many math operators.
     * @param {Function(TFTensor TFTensor):TFTensor} binaryOp The
     *  TensorFlow.js function to use as the binary operation
     * @param {PyretTensor} pyretTensorA The first tensor
     * @param {PyretTensor} pyretTensorB The second tensor
     * @returns {PyretTensor} The result
     */
    function applyBinaryOpToTensors(binaryOp, pyretTensorA, pyretTensorB) {
      // Unwrap Pyret values to JavaScript equivalents so TensorFlow.js can
      // recognize them:
      const jsTensorA = checkAndUnwrapTensor(pyretTensorA);
      const jsTensorB = checkAndUnwrapTensor(pyretTensorB);
      // Apply the binary function to the two tensors:
      const jsResult = binaryOp(jsTensorA, jsTensorB);
      return buildTensorObject(jsResult);
    }

    /**
     * Returns nothing if the PyretTensors `a` and `b` have the same shape;
     * otherwise, throws a Pyret runtime exception.
     *
     * This is intended to be a helper function for the "strict" arithmetic
     * operation methods.
     *
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     */
    function assertEqualShapes(a, b) {
      // Get the underlying TensorFlow.js representation of Tensor shapes:
      const aTensor = checkAndUnwrapTensor(a);
      const bTensor = checkAndUnwrapTensor(b);
      const aShape  = aTensor.shape;
      const bShape  = bTensor.shape;
      // Check if the shapes are the same length:
      if (aShape.length != bShape.length) { return false; }
      // Check element-wise equality:
      for (let i = 0; i < aShape.length; i++) {
        if (aShape[i] != bShape[i]) {
          runtime.ffi.throwMessageException("The first tensor does not have " +
            "the same shape as the second tensor");
        }
      }
    }

    /**
     * Returns nothing if the PyretTensors `a` and `b` have a combination of
     * shapes that a TensorFlow.js binary operation will successfully
     * process; otherwise, throws a Pyret runtime exception.
     *
     * This calculation is based on the `assertAndGetBroadcastShape` method
     * from TensorFlow.js (see https://github.com/tensorflow/tfjs-core/
     * blob/master/src/ops/broadcast_util.ts).
     *
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     */
    function assertValidShapeCombination(a, b) {
      // Get the underlying TensorFlow.js representation of Tensor shapes:
      const aTensor = checkAndUnwrapTensor(a);
      const bTensor = checkAndUnwrapTensor(b);
      const aShape  = aTensor.shape;
      const bShape  = bTensor.shape;
      const aRank   = aShape.length;
      const bRank   = bShape.length;
      // If at any axis, the size of that axis in both A and B are greater
      // than 1, they must be the same size:
      const maxLength = Math.max(aRank, bRank);
      for (let i = 0; i < maxLength; i++) {
        const a = aShape[aRank - i - 1] || 1;
        const b = bShape[bRank - i - 1] || 1;
        if ((a > 1) && (b > 1) && (a !== b)) {
          runtime.ffi.throwMessageException("Tensors could not be applied " +
            "as binary operation arguments with shapes " + aShape + " and " +
            bShape + ".");
        }
      }
    }

    /**
     * Returns nothing if the input PyretTensor does not contain the value
     * zero (as determined by a call to .dataSync); otherwise, throws a Pyret
     * runtime exception.
     *
     * This should be used to check for potential division-by-zero errors in
     * `divide-tensors` and `floor-divide-tensors`.
     *
     * @param {PyretTensor} pyretTensor
     */
    function assertTensorDoesNotContainZero(pyretTensor) {
      const jsTensor = checkAndUnwrapTensor(pyretTensor);
      const jsData   = jsTensor.dataSync();
      jsData.forEach(x => {
        if (x === 0) {
          runtime.ffi.throwMessageException("The second input Tensor " +
            "cannot contain 0 (to avoid division-by-zero errors).");
        }
      });
    }

    /**
     * Applies the input TensorFlow.js arithmetic operation to the input
     * Tensors a and b.
     *
     * This abstraction is used because the error checks used for almost all
     * of the TensorFlow.js arithmetic operations are the same.
     *
     * @param {Function(TFTensor TFTensor):TFTensor} arithmeticOp The
     *  arithmetic operation to apply
     * @param {PyretTensor} a The first argument to supply to arithmeticOp
     * @param {PyretTensor} b The second argument to supply to arithmeticOp
     * @returns {PyretTensor} The result of applying arithmeticOp to a and b
     */
    function applyArithmeticOpToTensors(arithmeticOp, a, b) {
      assertValidShapeCombination(a, b);
      return applyBinaryOpToTensors(arithmeticOp, a, b);
    }

    /**
     * Adds `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a + b
     */
    function addTensors(a, b) {
      arity(2, arguments, "add-tensors", false);
      return applyArithmeticOpToTensors(tf.add, a, b);
    }

    /**
     * Adds `a` and `b`, element-wise, but throws an exception if they are not
     * the same shape.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a + b
     */
    function addStrict(a, b) {
      arity(2, arguments, "strict-add-tensors", false);
      assertEqualShapes(a, b);
      return addTensors(a, b);
    }

    /**
     * Subtracts `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a - b
     */
    function subtractTensors(a, b) {
      arity(2, arguments, "subtract-tensors", false);
      return applyArithmeticOpToTensors(tf.sub, a, b);
    }

    /**
     * Subtracts `a` and `b`, element-wise, but throws an exception if they are
     * not the same shape.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a - b
     */
    function subtractStrict(a, b) {
      arity(2, arguments, "strict-subtract-tensors", false);
      assertEqualShapes(a, b);
      return subtractTensors(a, b);
    }

    /**
     * Multiplies `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a * b
     */
    function multiplyTensors(a, b) {
      arity(2, arguments, "multiply-tensors", false);
      return applyArithmeticOpToTensors(tf.mul, a, b);
    }

    /**
     * Multiplies `a` and `b`, element-wise, but throws an exception if they are
     * not the same shape.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a * b
     */
    function multiplyStrict(a, b) {
      arity(2, arguments, "strict-multiply-tensors", false);
      assertEqualShapes(a, b);
      return multiplyTensors(a, b);
    }

    /**
     * Divides `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a / b
     */
    function divideTensors(a, b) {
      arity(2, arguments, "divide-tensors", false);
      assertTensorDoesNotContainZero(b);
      return applyArithmeticOpToTensors(tf.div, a, b);
    }

    /**
     * Divides `a` and `b`, element-wise, but throws an exception if they are
     * not the same shape.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a / b
     */
    function divideStrict(a, b) {
      arity(2, arguments, "strict-divide-tensors", false);
      assertEqualShapes(a, b);
      return divideTensors(a, b);
    }

    /**
     * Divides `a` and `b`, element-wise, with the result rounded with the
     * floor function.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of floor(a / b)
     */
    function floorDivideTensors(a, b) {
      arity(2, arguments, "floor-divide-tensors", false);
      assertTensorDoesNotContainZero(b);
      return applyArithmeticOpToTensors(tf.floorDiv, a, b);
    }

    /**
     * Returns the maximum of `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function maxTensor(a, b) {
      arity(2, arguments, "tensor-max", false);
      return applyArithmeticOpToTensors(tf.maximum, a, b);
    }

    /**
     * Returns the maximum of `a` and `b`, element-wise, but throws an
     * exception if they are not the same shape.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function maxStrict(a, b) {
      arity(2, arguments, "strict-tensor-max", false);
      assertEqualShapes(a, b);
      return maxTensor(a, b);
    }

    /**
     * Returns the minimum of `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function minTensor(a, b) {
      arity(2, arguments, "tensor-min", false);
      return applyArithmeticOpToTensors(tf.minimum, a, b);
    }

    /**
     * Returns the minimum of `a` and `b`, element-wise, but throws an
     * exception if they are not the same shape.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function minStrict(a, b) {
      arity(2, arguments, "strict-tensor-min", false);
      assertEqualShapes(a, b);
      return minTensor(a, b);
    }

    /**
     * Returns the modulo of `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function moduloTensor(a, b) {
      arity(2, arguments, "tensor-modulo", false);
      assertTensorDoesNotContainZero(b);
      return applyArithmeticOpToTensors(tf.mod, a, b);
    }

    /**
     * Returns the modulo of `a` and `b`, element-wise, but throws an
     * exception if they are not the same shape.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function moduloStrict(a, b) {
      arity(2, arguments, "strict-tensor-modulo", false);
      assertEqualShapes(a, b);
      return moduloTensor(a, b);
    }

    /**
     * Computes `a` raised to `b`, element-wise.
     * @param {PyretTensor} base The first PyretTensor
     * @param {PyretTensor} exp The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function exptTensor(base, exp) {
      arity(2, arguments, "tensor-expt", false);
      return applyArithmeticOpToTensors(tf.pow, base, exp);
    }

    /**
     * Computes `a` raised to `b`, element-wise, but throws an exception
     * if they are not the same shape.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function exptStrict(a, b) {
      arity(2, arguments, "strict-tensor-modulo", false);
      assertEqualShapes(a, b);
      return exptTensor(a, b);
    }

    /**
     * Computes (a - b) * (a - b), element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function tensorSquaredDifference(a, b) {
      arity(2, arguments, "squared-difference", false);
      return applyArithmeticOpToTensors(tf.squaredDifference, a, b);
    }

    /**
     * Computes (a - b) * (a - b), element-wise, but throws an exception
     * if they are not the same shape.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function strictSquaredDifference(a, b) {
      arity(2, arguments, "strict-squared-difference", false);
      assertEqualShapes(a, b);
      return tensorSquaredDifference(a, b);
    }

    /**
     * Operations (Basic Math)
     */

    /**
     * Applies the specified unary function to the given tensor. Used as a
     * helper function for math operations on Tensors since the format is
     * equivalent for almost all math operators.
     * @param {Function(TFTensor):TFTensor} unaryOp The TensorFlow.js
     *  function to use as the unary operation
     * @param {PyretTensor} pyretTensor The tensor to reduce
     * @returns {PyretTensor} The result
     */
    function applyUnaryOpToTensor(unaryOp, pyretTensor) {
      // Unwrap Pyret values to JavaScript equivalents so TensorFlow.js can
      // recognize them:
      const jsTensor = checkAndUnwrapTensor(pyretTensor);
      const jsResult = unaryOp(jsTensor);
      return buildTensorObject(jsResult);
    }

    /**
     * Raises a Pyret runtime exception if any value in the input Tensor is
     * less than `min` or greater than `max`; otherwise, returns nothing.
     *
     * If `isInclusive` is true, then uses "less than or equal to" and "greater
     * than or equal to" to check for bound acceptance.
     *
     * @param {PyretTensor} tensor The tensor whose values to check
     * @param {Number} min The lower bound; if `null`, no lower bound is
     *  enforced
     * @param {Number} max The upper bound; if `null`, no upper bound is
     *  enforced
     * @param {Boolean} isInclusive If true, uses "less than or equal to" and
     *  "greater than or equal to" when checking against bounds
     * @returns {PyretTensor} The result
     */
    function assertTensorValuesBetween(tensor, min, max, isInclusive) {
      const jsTensor   = checkAndUnwrapTensor(tensor);
      const tensorData = jsTensor.dataSync();
      tensorData.forEach(x => {
        // We're using this (min && x < min) pattern to allow for the setting
        // of no lower or upper bound; that is, if min = null, then no lower
        // bound is enforced, etc:
        const inclusiveCond = ((min && x < min) || (max && x > max));
        const exclusiveCond = ((min && x <= min) || (max && x >= max));
        if (isInclusive ? inclusiveCond : exclusiveCond) {
          if (min && max) {
            const boundDesignation = isInclusive ? "inclusive" : "exclusive";
            // Exception for when both lower and upper bound is specified:
            runtime.ffi.throwMessageException("Values in the input " +
              "Tensor must be between " + min + " and " + max + ", " +
              boundDesignation + ".");
          }
          else if (max) {
            const boundDesignation = isInclusive ? "at most" : "less than";
            // Exception for when only upper bound is specified:
            runtime.ffi.throwMessageException("Values in the input " +
              "Tensor must be " + boundDesignation + " " + max + ".");
          }
          else if (min) {
            const boundDesignation = isInclusive ? "at least" : "greater than";
            // Exception for when only lower bound is specified:
            runtime.ffi.throwMessageException("Values in the input " +
              "Tensor must be " + boundDesignation + " " + min + ".");
          }
        }
      });
    }

    /**
     * Computes the absolute value of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function abs(x) {
      arity(1, arguments, "tensor-abs", false);
      return applyUnaryOpToTensor(tf.abs, x);
    }

    /**
     * Computes the inverse cosine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function acos(x) {
      arity(1, arguments, "tensor-acos", false);
      const lowerBound = -1;
      const upperBound = 1;
      assertTensorValuesBetween(x, -1, 1, true);
      return applyUnaryOpToTensor(tf.acos, x);
    }

    /**
     * Computes the inverse hyperbolic cosine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function acosh(x) {
      arity(1, arguments, "tensor-acosh", false);
      const lowerBound = 1;
      assertTensorValuesBetween(x, lowerBound, null, true);
      return applyUnaryOpToTensor(tf.acosh, x);
    }

    /**
     * Computes the inverse sine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function asin(x) {
      arity(1, arguments, "tensor-asin", false);
      const lowerBound = -1;
      const upperBound = 1;
      assertTensorValuesBetween(x, -1, 1, true);
      return applyUnaryOpToTensor(tf.asin, x);
    }

    /**
     * Computes the inverse hyperbolic sine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function asinh(x) {
      arity(1, arguments, "tensor-asinh", false);
      return applyUnaryOpToTensor(tf.asinh, x);
    }

    /**
     * Computes the inverse tangent of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function atan(x) {
      arity(1, arguments, "tensor-atan", false);
      return applyUnaryOpToTensor(tf.atan, x);
    }

    /**
     * Computes the four-quadrant inverse tangent of a and b, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function atan2(a, b) {
      arity(2, arguments, "tensor-atan2", false);
      return applyBinaryOpToTensors(tf.atan2, a, b);
    }

    /**
     * Computes the inverse hyperbolic tangent of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function atanh(x) {
      arity(1, arguments, "tensor-atanh", false);
      const lowerBound = -1;
      const upperBound = 1;
      assertTensorValuesBetween(x, -1, 1, false);
      return applyUnaryOpToTensor(tf.atanh, x);
    }

    /**
     * Computes the ceiling of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function ceil(x) {
      arity(1, arguments, "tensor-ceil", false);
      return applyUnaryOpToTensor(tf.ceil, x);
    }

    /**
     * Clips the values of the Tensor, element-wise, such that every element
     * in the resulting Tensor is at least min and is at most max.
     * @param {PyretTensor} x The PyretTensor to clip
     * @param {Number} min The minimum value to clip to
     * @param {Number} max The maximum value to clip to
     * @returns {PyretTensor} The result
     */
    function clipByValue(x, min, max) {
      arity(3, arguments, "clip-by-value", false);
      runtime.checkNumber(min);
      runtime.checkNumber(max);
      const tensor = checkAndUnwrapTensor(x);
      const jsMin  = runtime.num_to_fixnum(min);
      const jsMax  = runtime.num_to_fixnum(max);
      if (jsMin > jsMax) {
        runtime.ffi.throwMessageException("The minimum value to clip to must " +
          "be less than or equal to the maximum value to clip to, but the " +
          "input minimum value was " + jsMin + " and the input maximum value " +
          "was " + jsMax + ".");
      }
      return buildTensorObject(tf.clipByValue(tensor, jsMin, jsMax));
    }

    /**
     * Computes the cosine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function cos(x) {
      arity(1, arguments, "tensor-cos", false);
      return applyUnaryOpToTensor(tf.cos, x);
    }

    /**
     * Computes the hyperbolic cosine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function cosh(x) {
      arity(1, arguments, "tensor-cosh", false);
      return applyUnaryOpToTensor(tf.cosh, x);
    }

    /**
     * Computes the exponential linear units of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function elu(x) {
      arity(1, arguments, "elu", false);
      return exponentialLinearUnits(x);
    }

    /**
     * Computes the exponential linear units of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function exponentialLinearUnits(x) {
      arity(1, arguments, "exponential-linear-units", false);
      return applyUnaryOpToTensor(tf.elu, x);
    }

    /**
     * Computes the Gaussian error of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function erf(x) {
      arity(1, arguments, "erf", false);
      return gaussError(x);
    }

    /**
     * Computes the Gaussian error of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function gaussError(x) {
      arity(1, arguments, "gauss-error", false);
      return applyUnaryOpToTensor(tf.erf, x);
    }

    /**
     * Computes the equivalent of num-exp(x), element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function exp(x) {
      arity(1, arguments, "tensor-exp", false);
      return applyUnaryOpToTensor(tf.exp, x);
    }

    /**
     * Computes the equivalent of num-exp(x - 1), element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function expm1(x) {
      arity(1, arguments, "tensor-exp-min1", false);
      return applyUnaryOpToTensor(tf.expm1, x);
    }

    /**
     * Computes the floor of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function floor(x) {
      arity(1, arguments, "tensor-floor", false);
      return applyUnaryOpToTensor(tf.floor, x);
    }

    /**
     * Applies a leaky rectified linear units function to the Tensor,
     * element-wise.
     * @param {PyretTensor} x
     * @param {Number} alpha The scaling factor for negative values
     * @returns {PyretTensor} The result
     */
    function leakyRelu(x, alpha) {
      arity(2, arguments, "leaky-relu", false);
      runtime.checkNumber(alpha);
      const tensor  = checkAndUnwrapTensor(x);
      const jsAlpha = runtime.num_to_fixnum(alpha);
      return buildTensorObject(tf.leakyRelu(tensor, jsAlpha));
    }

    /**
     * Computes the log of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function log(x) {
      arity(1, arguments, "tensor-log", false);
      return applyUnaryOpToTensor(tf.log, x);
    }

    /**
     * Computes the equivalent of num-log(x + 1), element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function log1p(x) {
      arity(1, arguments, "tensor-log-plus1", false);
      return applyUnaryOpToTensor(tf.log1p, x);
    }

    /**
     * Applies the log sigmoid function to the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function logSigmoid(x) {
      arity(1, arguments, "log-sigmoid", false);
      return applyUnaryOpToTensor(tf.logSigmoid, x);
    }

    /**
     * Multiplies each element in the Tensor by -1.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function neg(x) {
      arity(1, arguments, "tensor-negate", false);
      return applyUnaryOpToTensor(tf.neg, x);
    }

    /**
     * Applies a leaky rectified linear units function to the Tensor,
     * element-wise, using parametric alphas.
     * @param {PyretTensor} x
     * @param {Number} alpha The scaling factor for negative values
     * @returns {PyretTensor} The result
     */
    function prelu(x, alpha) {
      arity(2, arguments, "parametric-relu", false);
      runtime.checkNumber(alpha);
      const tensor = checkAndUnwrapTensor(x);
      const jsAlpha = runtime.num_to_fixnum(alpha);
      return buildTensorObject(tf.prelu(tensor, jsAlpha));
    }

    /**
     * Computes the reciprocal of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function reciprocal(x) {
      arity(1, arguments, "tensor-reciprocal", false);
      return applyUnaryOpToTensor(tf.reciprocal, x);
    }

    /**
     * Applies a rectified linear units function to the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function relu(x) {
      arity(1, arguments, "relu", false);
      return applyUnaryOpToTensor(tf.relu, x);
    }

    /**
     * Computes the equivalent of num-round(x), element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function round(x) {
      arity(1, arguments, "tensor-round", false);
      return applyUnaryOpToTensor(tf.round, x);
    }

    /**
     * Computes the recriprocal of the square root of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function rsqrt(x) {
      arity(1, arguments, "reciprocal-sqrt", false);
      return applyUnaryOpToTensor(tf.rsqrt, x);
    }

    /**
     * Applies a scaled, exponential linear units function to the Tensor,
     * element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function selu(x) {
      arity(1, arguments, "scaled-elu", false);
      return applyUnaryOpToTensor(tf.selu, x);
    }

    /**
     * Applies the sigmoid function to the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function sigmoid(x) {
      arity(1, arguments, "sigmoid", false);
      return applyUnaryOpToTensor(tf.sigmoid, x);
    }

    /**
     * Returns an element-wise indication of the sign of each number in the
     * Tensor; that is, every value in the original tensor is represented in
     * the resulting tensor as ~+1 if the value is positive, ~-1 if the value
     * was negative, or ~0 if the value was zero or not a number.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function sign(x) {
      arity(1, arguments, "signed-ones", false);
      return applyUnaryOpToTensor(tf.sign, x);
    }

    /**
     * Computes the sine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function sin(x) {
      arity(1, arguments, "tensor-sin", false);
      return applyUnaryOpToTensor(tf.sin, x);
    }

    /**
     * Computes the hyperbolic sine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function sinh(x) {
      arity(1, arguments, "tensor-sinh", false);
      return applyUnaryOpToTensor(tf.sinh, x);
    }

    /**
     * Applies the softplus function to the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function softplus(x) {
      arity(1, arguments, "softplus", false);
      return applyUnaryOpToTensor(tf.softplus, x);
    }

    /**
     * Computes the square root of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function sqrt(x) {
      arity(1, arguments, "tensor-sqrt", false);
      return applyUnaryOpToTensor(tf.sqrt, x);
    }

    /**
     * Computes the square of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function square(x) {
      arity(1, arguments, "tensor-square", false);
      return applyUnaryOpToTensor(tf.square, x);
    }

    /**
     * Applies the unit step function to the Tensor, element-wise; that is,
     * every value in the original tensor is represented in the resulting
     * tensor as ~0 if the value is negative; otherwise, it is represented
     * as ~+1.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function step(x) {
      arity(1, arguments, "step", false);
      return applyUnaryOpToTensor(tf.step, x);
    }

    /**
     * Computes the tangent of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function tan(x) {
      arity(1, arguments, "tensor-tan", false);
      return applyUnaryOpToTensor(tf.tan, x);
    }

    /**
     * Computes the hyperbolic tangent of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function tanh(x) {
      arity(1, arguments, "tensor-tanh", false);
      return applyUnaryOpToTensor(tf.tanh, x);
    }

    /**
     * Operations (Reduction)
     */

    /**
     * Applies the specified reduction function to the given tensor along the
     * specified axis. Used as a helper function for reduction operations
     * since the format is equivalent for all reduction operations.
     * @param {Function} reductionOp The TensorFlow.js function to use as
     *  the reduction operation
     * @param {PyretTensor} pyretTensor The tensor to reduce
     * @param {Option<Number>} axis The axis to reduce along
     * @returns {PyretTensor} The result
     */
    function applyReductionToTensor(reductionOp, pyretTensor, axis) {
      // Unwrap Pyret values to JavaScript equivalents so TensorFlow.js can
      // recognize them:
      const jsTensor = checkAndUnwrapTensor(pyretTensor);
      const jsAxis   = unwrapFixnumOption(axis);
      // Apply reduction operation:
      const jsReducedTensor = reductionOp(jsTensor, jsAxis);
      return buildTensorObject(jsReducedTensor);
    }

    /**
     * Reduces the input Tensor across the input axis by computing the logical
     * "and" of its elements. The input must be of type "bool"; otherwise, the
     * function raises an error. If axis is none, reduces the tensor across
     * all dimensions.
     * @param {PyretTensor} x
     * @param {Option<Number>} axis The axis to reduce along
     * @returns {PyretTensor} The result
     */
    function all(x, axis) {
      arity(2, arguments, "reduce-all", false);
      return applyReductionToTensor(tf.all, x, axis);
    }

    /**
     * Reduces the input Tensor across the input axis by computing the logical
     * "or" of its elements. The input must be of type "bool"; otherwise, the
     * function raises an error. If axis is none, reduces the tensor across
     * all dimensions.
     * @param {PyretTensor} x
     * @param {Option<Number>} axis The axis to reduce along
     * @returns {PyretTensor} The result
     */
    function any(x, axis) {
      arity(2, arguments, "reduce-any", false);
      return applyReductionToTensor(tf.any, x, axis);
    }

    /**
     * Returns a new Tensor where each element is the index of the maximum
     * values along the axis of x. If axis is none, reduces along the outermost
     * dimension.
     * @param {PyretTensor} x
     * @param {Option<Number>} axis The axis to reduce along
     * @returns {PyretTensor} The result
     */
    function argMax(x, axis) {
      arity(2, arguments, "arg-max", false);
      return applyReductionToTensor(tf.argMax, x, axis);
    }

    /**
     * Returns a new Tensor where each element is the index of the minimum
     * values along the axis of x. If axis is none, reduces along the outermost
     * dimension.
     * @param {PyretTensor} x
     * @param {Option<Number>} axis The axis to reduce along
     * @returns {PyretTensor} The result
     */
    function argMin(x, axis) {
      arity(2, arguments, "arg-min", false);
      return applyReductionToTensor(tf.argMin, x, axis);
    }

    /**
     * Computes log(sum(exp(elements along the outermost dimension)). Reduces
     * x along the axis of x. If axis is none, reduces along the outermost
     * dimension.
     * @param {PyretTensor} x
     * @param {Option<Number>} axis The axis to reduce along
     * @returns {PyretTensor} The result
     */
    function logSumExp(x, axis) {
      arity(2, arguments, "log-sum-exp", false);
      return applyReductionToTensor(tf.logSumExp, x, axis);
    }

    /**
     * Computes the maximum of elements across the axis of x. If axis is none,
     * all dimensions are reduced, and a Tensor containing a single value is
     * returned.
     * @param {PyretTensor} x
     * @param {Option<Number>} axis The axis to reduce along
     * @returns {PyretTensor} The result
     */
    function max(x, axis) {
      arity(2, arguments, "reduce-max", false);
      return applyReductionToTensor(tf.max, x, axis);
    }

    /**
     * Computes the mean of elements across the axis of x. If axis is none,
     * all dimensions are reduced, and a Tensor containing a single value is
     * returned.
     * @param {PyretTensor} x
     * @param {Option<Number>} axis The axis to reduce along
     * @returns {PyretTensor} The result
     */
    function mean(x, axis) {
      arity(1, arguments, "reduce-mean", false);
      return applyReductionToTensor(tf.mean, x, axis);
    }

    /**
     * Computes the minimum of elements across the axis of x. If axis is none,
     * all dimensions are reduced, and a Tensor containing a single value is
     * returned.
     * @param {PyretTensor} x
     * @param {Option<Number>} axis The axis to reduce along
     * @returns {PyretTensor} The result
     */
    function min(x, axis) {
      arity(1, arguments, "reduce-min", false);
      return applyReductionToTensor(tf.min, x, axis);
    }

    /**
     * Computes the sum of elements across the axis of x. If axis is none,
     * all dimensions are reduced, and a Tensor containing a single value is
     * returned.
     * @param {PyretTensor} x
     * @param {Option<Number>} axis The axis to reduce along
     * @returns {PyretTensor} The result
     */
    function sum(x, axis) {
      arity(2, arguments, "reduce-sum", false);
      return applyReductionToTensor(tf.sum, x, axis);
    }

    /**
     * Operations (Slicing and Joining)
     */

    /**
     * Reshapes the given Tensor to the new shape specified by `newShape`.
     * @param {PyretTensor} tensor A PyretTensor
     * @param {List<NumInteger>} newShape The new shape of the Tensor
     * @returns {PyretTensor} A PyretTensor with the specified shape
     */
    function reshapeTensor(tensor, newShape) {
      arity(2, arguments, "reshape", true);
      const jsTensor     = checkAndUnwrapTensor(tensor);
      const jsShapeArray = unwrapListOfNumbersToArray(newShape, runtime.checkNumInteger);
      // Check that no dimension in the new shape is less than or equal to zero:
      jsShapeArray.forEach((dimensionSize, dimensionNum) => {
        if (dimensionSize <= 0) {
          runtime.ffi.throwMessageException("Cannot reshape because the " +
            "size at dimension " + dimensionNum + " was less than or equal " +
            "to zero.");
        }
      });
      // Check that the number of entries in the tensor is equal to the
      // number of spaces in the new shape:
      const spacesInNewShape = jsShapeArray.reduce((a, b) => (a * b), 1);
      if (jsTensor.size !== spacesInNewShape) {
        runtime.ffi.throwMessageException("Cannot reshape because the " +
          "number of entry spaces in the new shape must be equal to the " +
          "number of existing entries");
      }
      return buildTensorObject(jsTensor.reshape(jsShapeArray));
    }

    /**
     * Concatenates a list of tf.Tensors along a given axis. The tensors'
     * ranks and types must match, and their sizes must match in all
     * dimensions except axis.
     * @param {List<PyretTensor>} tensors The tensors to concatenate
     * @param {Option<Number>} axis The axis to reduce along
     * @returns {PyretTensor} The result
     */
    function concatenate(tensors, axis) {
      arity(2, arguments, "concatenate", false);
      const jsTensors = unwrapListOfTensorsToArray(tensors);
      const jsAxis    = unwrapFixnumOption(axis);
      return buildTensorObject(tf.concat(jsTensors, jsAxis));
    }

    /**
     * Gathers slices from the input tensor's axis along the specified axis
     * at the specified indices.
     * @param {PyretTensor} tensor The input tensor on which to gather slices
     * @param {PyretTensor} indices The indices of the values to extract
     * @param {Option<Number>} axis The axis to reduce along
     * @returns {PyretTensor} The result
     */
    function gather(tensor, indices, axis) {
      arity(3, arguments, "gather", false);
      const jsTensor  = checkAndUnwrapTensor(tensor);
      const jsIndices = checkAndUnwrapTensor(indices);
      if (jsIndices.shape.length !== 1) {
        runtime.ffi.throwMessageException("The `indices` argument to " +
          "`gather` must be a one-dimensional Tensor");
      }
      if (jsIndices.dtype !== "int32") {
        runtime.ffi.throwMessageException("The `indices` argument to " +
          "`gather` must have a data type of 'int32'. Try using `.to-int()` " +
          "on the `indices` tensor to convert the data type to 'int32'.");
      }
      const jsAxis = unwrapFixnumOption(axis);
      return buildTensorObject(tf.gather(jsTensor, jsIndices, jsAxis));
    }

    /**
     * Reverses a Tensor along the specified axes.
     * @param {PyretTensor} tensor The input tensor on which to run reverse
     *  operations
     * @param {Option<List<Number>>} axes The set of dimensions to reverse
     * @returns {PyretTensor} The result
     */
    function reverse(tensor, axes) {
      arity(2, arguments, "reverse", false);
      const jsTensor = checkAndUnwrapTensor(tensor);
      const jsAxes   =
        runtime.ffi.cases(runtime.ffi.isOption, "is-Option", axes, {
          some: unwrapListOfNumbersToArray,
          none: () => { return undefined; }
        });
      return buildTensorObject(tf.reverse(jsTensor, jsAxes));
    }

    /**
     * Extracts a slice from a Tensor starting at coordinates `begin` and
     * of size `size`.
     * @param {PyretTensor} tensor The input tensor to slice from
     * @param {List<Number>} begin The coordinates to start the slice from;
     *  if the length of this list is less than the rank of x, the rest of
     *  the axes will have implicit 0 as the starting coordinates
     * @param {Option<List<Number>>} size The size of the slice; if the
     *  length of this list is less than the rank of x, the rest of
     *  the axes will have implicit -1 as the size (a value of -1 in an
     *  axis requests the rest of the dimensions in that axis)
     * @returns {PyretTensor} The result
     */
    function slice(tensor, begin, size) {
      arity(3, arguments, "slice", false);
      const jsTensor = checkAndUnwrapTensor(tensor);
      const jsBegin  = unwrapListOfNumbersToArray(begin);
      const jsSize   =
        runtime.ffi.cases(runtime.ffi.isOption, "is-Option", size, {
          some: unwrapListOfNumbersToArray,
          none: () => { return undefined; }
        });
      return buildTensorObject(tf.slice(jsTensor, jsBegin, jsSize));
    }

    /**
     * Splits a Tensor into sub-tensors.
     * @param {PyretTensor} tensor The input tensor to split
     * @param {List<NumInteger>} splitSizes A list of integers containing
     *  the sizes of each output tensor along the axis; the sum of sizes
     *  must match `tensor.shape().get-value(axis)`
     * @param {Option<Number>} axis The dimension along which to
     *  split; defaults to zero
     * @returns {PyretTensor} The result
     */
    function split(tensor, splitSizes, axis) {
      arity(3, arguments, "split", false);
      const jsTensor = checkAndUnwrapTensor(tensor);
      const jsSplits = unwrapListOfNumbersToArray(splitSizes, runtime.checkNumInteger);
      const jsAxis   = unwrapFixnumOption(axis);
      // Check that the sum of split sizes matches the size of the dimension
      // at jsAxis:
      const sumOfSplits   = jsSplits.reduce((acc, x) => { return acc + x; }, 0);
      const dimensionSize = jsTensor.shape[jsAxis];
      if (sumOfSplits !== dimensionSize) {
        runtime.ffi.throwMessageException("The sum of split sizes must match " +
          "the size of the dimension at the specified axis, but the size of " +
          "the dimension at axis " + jsAxis + " was " + dimensionSize + " " +
          "and the sum of the split sizes was " + sumOfSplits + ".");
      }
      return buildTensorObject(tf.split(jsTensor, jsSplits, jsAxis));
    }

    /**
     * Stacks a list of rank-R Tensors into one rank-(R+1) Tensor.
     * @param {List<PyretTensor>} tensorList A List of PyretTensors with the
     *  same shape and data type.
     * @param {Option<Number>} axis The axis to stack along; defaults to 0.
     * @returns {PyretTensor} The result
     */
    function stack(tensorList, axis) {
      arity(2, arguments, "stack", false);

      const jsTensorArray = unwrapListOfTensorsToArray(tensorList);
      const jsAxis        = unwrapFixnumOption(axis);

      if (jsTensorArray.length <= 0) {
        runtime.ffi.throwMessageException("At least one Tensor must be " +
          "supplied in the List<Tensor> passed to `stack`");
      }

      const firstTensor = jsTensorArray[0];
      const rank  = firstTensor.rank;
      const shape = firstTensor.shape;
      const dtype = firstTensor.dtype;

      if (jsAxis > rank) {
        runtime.ffi.throwMessageException("Axis must be <= rank of the " +
          "tensor");
      }
      jsTensorArray.forEach((t) => {
        if (t.shape !== shape) {
          runtime.ffi.throwMessageException("All tensors passed to `stack` " +
            "must have matching shapes");
        }
        if (t.dtype !== dtype) {
          runtime.ffi.throwMessageException("All tensors passed to `stack` " +
            "must have matching data types");
        }
      });
      return buildTensorObject(tf.stack(jsTensorArray, jsAxis));
    }

    /**
     * Constructs a Tensor by repeating it the number of times given by
     * `repetitions`.
     * @param {PyretTensor} tensor The input tensor to repeat
     * @param {List<Number>} repetitions The number of replications per
     *  dimension (eg: the first element in the list represents the number of
     *  replications along the first dimension, etc.)
     * @returns {PyretTensor} The result
     */
    function tile(tensor, repetitions) {
      // NTI(ZacharyEspiritu)
      arity(2, arguments, "tile", false);
      runtime.ffi.throwMessageException("Not yet implemented");
    }

    /**
     * Unstacks a Tensor of rank-R into a list of rank-(R-1) Tensors.
     * @param {PyretTensor} tensor The input tensor to unstack
     * @param {Option<Number>} axis The dimension along which to unstack;
     *  defaults to zero
     * @returns {List<PyretTensor>} The resulting tensors
     */
    function unstack(tensor, axis) {
      arity(2, arguments, "unstack", false);
      checkTensor(tensor);
      const jsTensor  = unwrapTensor(tensor);
      const jsAxis    = unwrapFixnumOption(axis);
      // Apply function and convert result to a List<PyretTensor>:
      const jsResults    = tf.unstack(jsTensor, jsAxis);
      const pyretResults = jsResults.map(buildTensorObject);
      return runtime.ffi.makeList(pyretResults);
    }

    /**
     * Extracts a strided slice of a Tensor.
     *
     * Roughly speaking, this operations extracts a slice of size
     * `(end - begin) / stride` from `tensor`. Starting at the location
     * specified by `begin`, the slice continues by adding stride to the
     * index until all dimensions are not less than `end`. Note that a
     * stride can be negative, which causes a reverse slice.
     *
     * @param {PyretTensor} tensor The input tensor to repeat
     * @param {List<NumInteger>} begin The coordinates to start the slice from
     * @param {List<NumInteger>} end The coordinates to end the slice at
     * @param {List<Number>} strides The size of the slice
     * @returns {PyretTensor} The result
     */
    function stridedSlice(tensor, begin, end, strides) {
      arity(4, arguments, "strided-slice", false);
      const jsTensor  = checkAndUnwrapTensor(tensor);
      const jsBegin   = unwrapListOfNumbersToArray(begin, runtime.checkNumInteger);
      const jsEnd     = unwrapListOfNumbersToArray(end, runtime.checkNumInteger);
      const jsStrides = unwrapListOfNumbersToArray(strides);
      return buildTensorObject(tf.stridedSlice(jsTensor, jsBegin, jsEnd, jsStrides));
    }

    /**
     * Models (Generic)
     */

    /**
     * Returns PyretTrue if the input `obj` is a PyretModel; otherwise,
     * returns PyretFalse.
     * @param {Any} obj Some Pyret value
     * @returns {PBoolean} A Pyret object representing true or false
     */
    function isModel(obj) {
      arity(1, arguments, "is-model", false);
      return runtime.makeBoolean(hasBrand(brandModel, obj));
    }

    /**
     * Consumes a PyretModel and returns its underlying TensorFlow.js Model.
     * @param {PyretModel} PyretModel
     * @returns {TFModel} The underlying TensorFlow.js Model
     *  of the input PyretModel
     */
    function unwrapSequential(pyretModel) {
      return pyretModel.$underlyingModel;
    }

    /**
     * Consumes a TFModel and wraps it in a PyretObject to make it a
     * PyretModel.
     * @param {TFModel} underlyingLayer A TensorFlow.js Layer
     * @returns {PyretModel} A new PyretModel with the input
     *  as its underlying TFModel
     */
    function buildModelObject(underlyingModel) {
      let obj = O({
        "_output": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "_output");
          return get(VS, "vs-str").app("<model>");
        }),
      });
      obj = applyBrand(brandModel, obj);
      obj.$underlyingModel = underlyingModel;
      return obj;
    }

    function makeModel(config) {
      arity(1, arguments, "make-model", false);
      runtime.checkObject(config);
      const jsConfig = unwrapObject(config);
      return buildModelObject(tf.model(jsConfig));
    }

    /**
     * Models (Sequential)
     */

    /**
     * Returns PyretTrue if the input `obj` is a PyretSequential; otherwise,
     * returns PyretFalse.
     * @param {Any} obj Some Pyret value
     * @returns {PBoolean} A Pyret object representing true or false
     */
    function isSequential(obj) {
      arity(1, arguments, "is-sequential", false);
      return runtime.makeBoolean(hasBrand(brandSequential, obj));
    }

    /**
     * Consumes a PyretSequential and returns its underlying TensorFlow.js
     * Sequential model.
     * @param {PyretSequential} pyretSequential
     * @returns {TFSequential} The underlying TensorFlow.js Sequential model
     *  of the input PyretSequential
     */
    function unwrapSequential(pyretSequential) {
      return pyretSequential.$underlyingSequential;
    }

    /**
     * Consumes a TFSequential and wraps it in a PyretObject to make it a
     * PyretSequential.
     * @param {TFSequential} underlyingLayer A TensorFlow.js Layer
     * @returns {PyretSequential} A new PyretSequential with the input
     *  as its underlying TFSequential
     */
    function buildSequentialObject(underlyingSequential) {
      let obj = O({
        "add": runtime.makeMethod1(function(self, layer) {
          checkMethodArity(2, arguments, "add");
          checkLayer(layer);
          const selfSequential = unwrapSequential(self);
          const unwrappedLayer = unwrapLayer(layer);
          selfSequential.add(unwrappedLayer);
          return runtime.makeNothing();
        }),
        "compile": runtime.makeMethod1(function(self, config) {
          checkMethodArity(2, arguments, "compile");
          runtime.checkObject(config);
          let jsConfig = unwrapObject(config);
          // If there's an Optimizer defined, we have to unwrap it since
          // Tensorflow.js doesn't recognize PyretOptimizers:
          if ("optimizer" in jsConfig) {
            // But it could also be a string key that maps to a predefined
            // TF.js optimizer:
            if (typeof jsConfig["optimizer"] !== "string") {
              jsConfig["optimizer"] = jsConfig["optimizer"].$underlyingOptimizer;
            }
          }
          const selfSequential = unwrapSequential(self);
          selfSequential.compile(jsConfig);
          return runtime.makeNothing();
        }),
        "evaluate": runtime.makeMethod3(function(self, x, y, config) {
          checkMethodArity(4, arguments, "evaluate");
          runtime.checkObject(config);
          const selfSequential = unwrapSequential(self);
          const xTensor        = checkAndUnwrapTensor(x);
          const yTensor        = checkAndUnwrapTensor(y);
          const jsConfig       = unwrapObject(config);
          const result         = selfSequential.evaluate(xTensor, yTensor, jsConfig);
          return buildTensorObject(result);
        }),
        "predict": runtime.makeMethod2(function(self, x, config) {
          checkMethodArity(3, arguments, "predict");
          runtime.checkObject(config);
          const selfSequential = unwrapSequential(self);
          const xTensor        = checkAndUnwrapTensor(x);
          const jsConfig       = unwrapObject(config);
          const result         = selfSequential.predict(xTensor, jsConfig);
          return buildTensorObject(result);
        }),
        "predict-on-batch": runtime.makeMethod1(function(self, x) {
          checkMethodArity(2, arguments, "predict-on-batch");
          const selfSequential = unwrapSequential(self);
          const xTensor        = checkAndUnwrapTensor(x);
          const result         = selfSequential.predictOnBatch(xTensor);
          return buildTensorObject(result);
        }),
        "fit": runtime.makeMethod3(function(self, x, y, config, callback) {
          checkMethodArity(5, arguments, "fit");
          runtime.checkObject(config);
          const xTensor = checkAndUnwrapTensor(x);
          const yTensor = checkAndUnwrapTensor(y);
          let jsConfig  = unwrapObject(config);
          // TODO(Zachary): Generalize across multiple callback types
          jsConfig.callbacks = {
            onEpochEnd: async (epoch, log) => {
              runtime.safeCall(() => {
                callback.app(runtime.makeNumber(epoch), runtime.makeObject(log));
              }, (_) => {}); // handler purposely blank
            },
          };
          const selfSequential = unwrapSequential(self);
          selfSequential.fit(xTensor, yTensor, jsConfig);
          return runtime.makeNothing();
        }),
      });
      obj = applyBrand(brandSequential, obj);
      obj.$underlyingSequential = underlyingSequential;
      return obj;
    }

    function makeSequential(config) {
      arity(1, arguments, "make-sequential", false);
      runtime.checkObject(config);
      const jsConfig = unwrapObject(config);
      return buildSequentialObject(tf.sequential(jsConfig));
    }

    /**
     * Models (SymbolicTensors)
     */

    /**
     * Consumes a PyretSymbolicTensor and returns its underlying TensorFlow.js
     * SymbolicTensor.
     * @param {PyretSymbolicTensor} pyretSymbolic
     * @returns {TFSymbolicTensor} The underlying TensorFlow.js SymbolicTensor
     *  of the input PyretSymbolicTensor
     */
    function unwrapSymbolicTensor(pyretSymbolic) {
      return pyretSymbolic.$underlyingSymbolic;
    }

    /**
     * Consumes a TFSymbolicTensor and wraps it in a PyretObject to make it a
     * PyretSymbolicTensor.
     * @param {TFSymbolicTensor} underlyingLayer A TensorFlow.js Layer
     * @returns {PyretSymbolicTensor} A new PyretSymbolicTensor with the input
     *  as its underlying TFSymbolicTensor
     */
    function buildSymbolicTensorObject(underlyingSymbolic) {
      let obj = O({
        "_output": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "_output");
          return get(VS, "vs-str").app("<symbolic-tensor>");
        }),
        "shape": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "shape");
          const selfSymbolic = unwrapSymbolicTensor(self);
          const optionValues = selfSymbolic.shape.map(x => {
            if (x) {
              return runtime.ffi.makeSome(x);
            }
            else {
              return runtime.ffi.makeNone();
            }
          });
          return runtime.ffi.makeList(optionValues);
        }),
      });
      obj = applyBrand(brandSymbolicTensor, obj);
      obj.$underlyingSymbolic = underlyingSymbolic;
      return obj;
    }

    /**
     * Used to instantiate an input to a PyretModel as a PyretSymbolicTensor.
     * @param {List<Option<Number>>} shape A shape, not including the batch size
     * @returns {PyretSymbolicTensor} The result
     */
    function makeInput(shape) {
      arity(1, arguments, "make-input", false);
      const jsShape = checkAndUnwrapListOfOptionNumbers(shape, null);
      const input   = tf.input({ shape: jsShape });
      return buildSymbolicTensorObject(input);
    }

    /**
     * Used to instantiate an input to a PyretModel as a PyretSymbolicTensor.
     * @param {List<Option<Number>>} batchShape A shape tuple that includes the
     *  batch size
     * @returns {PyretSymbolicTensor} The result
     */
    function makeBatchInput(batchShape) {
      arity(1, arguments, "make-batch-input", false);
      const jsShape = checkAndUnwrapListOfOptionNumbers(batchShape, null);
      const input   = tf.input({ batchShape: jsShape });
      return buildSymbolicTensorObject(input);
    }

    /**
     * Layers
     */

    /**
     * Returns PyretTrue if the input `obj` is a PyretLayer; otherwise,
     * returns PyretFalse.
     * @param {Any} obj Some Pyret value
     * @returns {PBoolean} A Pyret object representing true or false
     */
    function isLayer(obj) {
      arity(1, arguments, "is-layer", false);
      return runtime.makeBoolean(hasBrand(brandLayer, obj));
    }

    /**
     * Consumes a PyretLayer and returns its underlying TensorFlow.js
     * Layer.
     * @param {PyretLayer} pyretLayer
     * @returns {TFLayer} The underlying TensorFlow.js Layer of the
     *  input PyretLayer
     */
    function unwrapLayer(pyretLayer) {
      return pyretLayer.$underlyingLayer;
    }

    /**
     * The Pyret version of a TensorFlow.js Layer configuration.
     * @typedef {Object} PyretLayerConfig
     */

    /**
     * A mapping between Pyret keys in a PyretLayerConfig to the
     * TensorFlow.js keys that should be used when passing the layer
     * configuration to the TensorFlow.js layer constructors. It is
     * used to allow for the "kebob case" naming convention in Pyret
     * but to provide the proper conversions necessary for the Pyret
     * configurations to work with TensorFlow.js.
     *
     * Each top-level key is a configuration option named according
     * to Pyret style conventions, and the value of each key is an object
     * containing the `jsName` that TensorFlow.js will recognize and
     * `typeCheckAndConvert`, a function taking a single parameter that
     * should check that the value at the given key is the correct type
     * and return a version of that value that TensorFlow.js will
     * recognize.
     *
     * @constant
     * @type {Object}
     */
    const DEFAULT_LAYER_OPTIONS_MAPPINGS = {
      "input-shape": {
        // List<NumInteger>
        jsName: "inputShape",
        typeCheckAndConvert: (v) => unwrapListOfNumbersToArray(v, runtime.checkNumInteger),
        prevents: ["batch-input-shape"],
      },
      "batch-input-shape": {
        // List<NumInteger>
        jsName: "batchInputShape",
        typeCheckAndConvert: (v) => unwrapListOfNumbersToArray(v, runtime.checkNumInteger),
        prevents: ["input-shape"],
      },
      "batch-size": {
        // NumInteger
        jsName: "batchSize",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
      },
      "trainable": {
        // Boolean
        jsName: "trainable",
        typeCheckAndConvert: (v) => {
          runtime.checkBoolean(v);
          return runtime.isPyretTrue(v);
        },
      },
      "updatable": {
        // Boolean
        jsName: "updatable",
        typeCheckAndConvert: (v) => {
          runtime.checkBoolean(v);
          return runtime.isPyretTrue(v);
        },
      },
      // TODO(ZacharyEspiritu): "weights" option unimplemented for now; kind of
      // complicated to implement and haven't seen use-cases for it yet.
    };

    /**
     * Converts a PyretLayerConfig to a Javascript layer config object
     * that can be passed into a layer constructor.
     *
     * This is used to allow for the Pyret naming convention of "kebob
     * case" but to handle TensorFlow.js's requirement of camel case
     * for configuration objects. Additionally, sometimes TensorFlow.js
     * needs an actual TensorFlow.js object, like a Model or a Layer, as
     * one of the parameters, so we use this to unwrap the Pyret
     * equivalents accordingly.
     *
     * This function raises an Pyret error if any key in pyretLayerConfig
     * does not have a valid mapping in either the default layer config
     * mappings or the input mapExtension.
     *
     * @param {PyretLayerConfig} pyretLayerConfig
     * @param {Object} mapExtension An extension to the default layer config
     *  mappings (DEFAULT_LAYER_OPTIONS_MAPPINGS) for when certain layers have
     *  parameters specifically for the given Layer variant
     * @returns {Object} The same layer configuration, but with the keys
     *  properly named for use in a TensorFlow.js Layer constructor
     */
    function pyretLayerConfigToJsConfig(pyretLayerConfig, mapExtension) {
      runtime.checkObject(pyretLayerConfig);
      const pyretConfig = unwrapObject(pyretLayerConfig);
      const pyretKeys   = Object.keys(pyretConfig);
      // Extend the default layer configuration mappings with per-layer
      // options:
      const mappings = Object.assign(DEFAULT_LAYER_OPTIONS_MAPPINGS, mapExtension);
      // Some keys prevent other keys from being added, so we'll keep track
      // of when a key is no longer allowed to be added due to the presence
      // of another (and which key was the culprit of that restriction):
      let preventedKeys = {};
      // Iterate over every key in pyretLayerConfig and use the associated
      // mapping for each key to get the TensorFlow.js name for that key and
      // the unwrapped, Javascript value for the value at that position:
      return pyretKeys.reduce((accumulator, pyretKey) => {
        const keyMapping = mappings[pyretKey];
        // Check to make sure that we have a mapping for pyretKey:
        if (!keyMapping) {
          // Raise an error if we don't:
          runtime.ffi.throwMessageException("Layer configuration object " +
            "contained \"" + pyretKey + "\" as a key, which was not a valid " +
            "configuration option for this layer.");
        }
        else if (pyretKey in preventedKeys) {
          // Raise an error if a previous key prevents the addition of the
          // current key due to TensorFlow.js restrictions:
          const culprit = preventedKeys[pyretKey];
          runtime.ffi.throwMessageException("Both \"" + pyretKey + "\" and \"" +
            culprit + "\" were present in the same Layer configuration object, " +
            "but only one of these options may be present in a single Layer " +
            "configuration.");
        }
        else {
          // Otherwise, we're good to go, and we can update the accumulator:
          const pyretValue   = pyretConfig[pyretKey];
          const jsKey        = keyMapping.jsName;
          const jsValue      = keyMapping.typeCheckAndConvert(pyretValue);
          accumulator[jsKey] = jsValue;
          // Check if this key prevents other keys from being added; if so,
          // add it to our running list of restricted keys:
          const blockedKeys = keyMapping.prevents;
          if (blockedKeys) {
            const restictions = blockedKeys.reduce((o, key) => {
              return Object.assign(o, {[key]: pyretKey});
            }, {});
            preventedKeys = Object.assign(preventedKeys, restictions)
          }
        }
        return accumulator;
      }, {});
    }

    /**
     * Tiny helper function used for listing the possible options for a
     * given Layer parameter in error messages.
     * @param {String[]} optionsArray An array of the possible strings
     *  to choose from
     * @returns {String} A comma-delimited list of the input options
     */
    function optionsToSentence(optionsArray) {
      let base = "";
      for (let i = 0; i < optionsArray.length; i++) {
        const nextOption = optionsArray[i];
        base += "\"" + optionsArray + "\"";
        // If not the last element, add a comma:
        if ((i + 1) !== optionsArray.length) {
          base += ", ";
        }
        // If we're at the second-to-last element, add an "and":
        if ((i + 2) === optionsArray.length) {
          base += "and ";
        }
      }
      return base;
    }

    /**
     * Checks if the input corresponds to a valid, TensorFlow.js
     * activation function. If so, it returns the value that should be
     * passed to a TensorFlow.js function for the given input; otherwise,
     * it raises a Pyret runtime error.
     * @param {Any} possibleActivation The name of the activation function.
     *  (This parameter is typed as Any because in some cases a given
     *  TensorFlow.js parameter has multiple possible types, so we can
     *  easily extend this function if necessary.)
     * @returns {Any} The value that should be passed to TensorFlow.js
     *  corresponding to the given input
     */
    function checkAndConvertActivationFunction(possibleActivation) {
      const VALID_ACTIVATION_FUNCTIONS = [
        "elu",
        "hardSigmoid",
        "linear",
        "relu",
        "relu6",
        "selu",
        "sigmoid",
        "softmax",
        "softplus",
        "softsign",
        "tanh"
      ];
      // Check that possibleActivation is a known activation function:
      if (!VALID_ACTIVATION_FUNCTIONS.includes(possibleActivation)) {
        runtime.ffi.throwMessageException("\"" + possibleActivation + "\" is " +
          "not a valid activation function. The possible activation functions " +
          " are: " + optionsToSentence(VALID_ACTIVATION_FUNCTIONS) + ".");
      }
      return possibleActivation;
    }

    /**
     * Checks if the input corresponds to a valid, TensorFlow.js
     * initializer function. If so, it returns the value that should be
     * passed to a TensorFlow.js function for the given input; otherwise,
     * it raises a Pyret runtime error.
     * @param {Any} possibleInitializer The name of the initializer
     *  function. (This parameter is typed as Any because in some cases
     *  a given TensorFlow.js parameter has multiple possible types, so
     *  we can easily extend this function if necessary.)
     * @returns {Any} The value that should be passed to TensorFlow.js
     *  corresponding to the given input
     */
    function checkAndConvertInitializerFunction(possibleInitializer) {
      const VALID_INITIALIZER_FUNCTIONS = [
        "constant",
        "glorotNormal",
        "glorotUniform",
        "heNormal",
        "identity",
        "leCunNormal",
        "ones",
        "orthogonal",
        "randomNormal",
        "randomUniform",
        "truncatedNormal",
        "varianceScaling",
        "zeros"
      ];
      // Check that possibleInitializer is a known initializer function:
      if (!VALID_INITIALIZER_FUNCTIONS.includes(possibleInitializer)) {
        runtime.ffi.throwMessageException("\"" + possibleInitializer + "\" is " +
          "not a valid initializer function. The possible initializer functions " +
          " are: " + optionsToSentence(VALID_INITIALIZER_FUNCTIONS) + ".");
      }
      return possibleInitializer;
    }

    /**
     * Checks if the input corresponds to a valid, TensorFlow.js
     * constraint function. If so, it returns the value that should be
     * passed to a TensorFlow.js function for the given input; otherwise,
     * it raises a Pyret runtime error.
     * @param {Any} possibleConstraint The name of the constraint function.
     *  (This parameter is typed as Any because in some cases a given
     *  TensorFlow.js parameter has multiple possible types, so we can
     *  easily extend this function if necessary.)
     * @returns {Any} The value that should be passed to TensorFlow.js
     *  corresponding to the given input
     */
    function checkAndConvertConstraintFunction(possibleConstraint) {
      const VALID_CONSTRAINT_FUNCTIONS = [
        "maxNorm",
        "minMaxNorm",
        "nonNeg",
        "unitNorm"
      ];
      // Check that possibleConstraint is a known constraint function:
      if (!VALID_CONSTRAINT_FUNCTIONS.includes(possibleConstraint)) {
        runtime.ffi.throwMessageException("\"" + possibleConstraint + "\" is " +
          "not a valid constraint function. The possible constraint functions " +
          " are: " + optionsToSentence(VALID_CONSTRAINT_FUNCTIONS) + ".");
      }
      return possibleConstraint;
    }

    /**
     * Checks if the input corresponds to a valid, TensorFlow.js
     * regularizer. If so, it returns the value that should be passed to
     * a TensorFlow.js function for the given input; otherwise, it raises
     * a Pyret runtime error.
     * @param {Any} possibleRegularizer The name of the regularizer.
     *  (This parameter is typed as Any because in some cases a given
     *  TensorFlow.js parameter has multiple possible types, so we can
     *  easily extend this function if necessary.)
     * @returns {Any} The value that should be passed to TensorFlow.js
     *  corresponding to the given input
     */
    function checkAndConvertRegularizer(possibleRegularizer) {
      const VALID_REGULARIZERS = [
        "l1l2"
      ];
      // Check that possibleRegularizer is a known regularizer function:
      if (!VALID_REGULARIZERS.includes(possibleRegularizer)) {
        runtime.ffi.throwMessageException("\"" + possibleRegularizer + "\" is " +
          "not a valid regularizer. The possible regularizers are: " +
          optionsToSentence(VALID_REGULARIZERS) + ".");
      }
      return possibleRegularizer;
    }

    /**
     * Checks if the input corresponds to a valid, TensorFlow.js
     * data format (mainly for use in type checking the "data-format" option
     * passed to many convolution layers). If so, it returns the value that
     * should be passed to a TensorFlow.js function for the given input;
     * otherwise, it raises a Pyret runtime error.
     * @param {Any} possibleDataFormat The name of the data format.
     *  (This parameter is typed as Any because in some cases a given
     *  TensorFlow.js parameter has multiple possible types, so we can
     *  easily extend this function if necessary.)
     * @returns {Any} The value that should be passed to TensorFlow.js
     *  corresponding to the given input
     */
    function checkAndConvertDataFormat(possibleDataFormat) {
      const VALID_DATA_FORMATS = [
        "channelsFirst",
        "channelsLast"
      ];
      // Check that possibleDataFormat is a known data format:
      if (!VALID_DATA_FORMATS.includes(possibleDataFormat)) {
        runtime.ffi.throwMessageException("\"" + possibleDataFormat + "\" " +
          "is not a valid data format. The possible data formats are: " +
          optionsToSentence(VALID_DATA_FORMATS) + ".");
      }
      return possibleDataFormat;
    }

    /**
     * Checks if the input corresponds to a valid, TensorFlow.js
     * padding method (mainly for use in type checking the "padding" option
     * in pooling layers). If so, it returns the value that should be passed
     * to a TensorFlow.js function for the given input; otherwise, it raises
     * a Pyret runtime error.
     * @param {Any} possiblePaddingMethod The name of the padding method.
     *  (This parameter is typed as Any because in some cases a given
     *  TensorFlow.js parameter has multiple possible types, so we can
     *  easily extend this function if necessary.)
     * @returns {Any} The value that should be passed to TensorFlow.js
     *  corresponding to the given input
     */
    function checkAndConvertPaddingMethod(possiblePaddingMethod) {
      const VALID_PADDINGS = ["valid", "same", "casual"];
      // Check that possiblePaddingMethod is a known padding method:
      if (!VALID_PADDINGS.includes(possiblePaddingMethod)) {
        runtime.ffi.throwMessageException("\"" + possiblePaddingMethod + "\" " +
          "is not a valid padding method. The possible padding methods " +
          "are: " + optionsToSentence(VALID_PADDINGS) + ".");
      }
      return possiblePaddingMethod;
    }

    /**
     * Creates a new PyretLayer using the given TensorFlow.js factory
     * function and the Pyret Object representing the layer configuration.
     * @param {Function} tfLayerFn A TensorFlow.js layer function
     *  which consumes a JavaScript layer configuration and returns
     *  a new TensorFlow.js layer
     * @param {PyretLayerConfig} pyretConfig The configuration to use when
     *  constructing the new layer
     * @param {Object} mappingExtension An object to extend the default
     *  `DEFAULT_LAYER_OPTIONS_MAPPINGS` configuration to allow for
     *  layer-specific Pyret to TensorFlow conversions
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeLayerWith(tfLayerFn, pyretConfig, mappingExtension) {
      const jsConfig = pyretLayerConfigToJsConfig(pyretConfig, mappingExtension);
      return buildLayerObject(tfLayerFn(jsConfig));
    }

    /**
     * Consumes a TFLayer and wraps it in a PyretObject to make it a
     * PyretLayer.
     * @param {TFLayer} underlyingLayer A TensorFlow.js Layer
     * @returns {PyretLayer} A new PyretLayer with the input as its
     *  underlying TFLayer
     */
    function buildLayerObject(underlyingLayer) {
      let obj = O({
        "_output": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "_output");
          return get(VS, "vs-str").app("<layer>");
        }),
        "apply-tensors": runtime.makeMethod1(function(self, tensors) {
          checkMethodArity(2, arguments, "apply-tensors");
          const inputs    = unwrapListOfTensorsToArray(tensors);
          const selfLayer = unwrapLayer(self);
          const outputs   = selfLayer.apply(inputs).map(buildTensorObject);
          return runtime.ffi.makeList(outputs);
        }),
        "apply-symbolic-tensors": runtime.makeMethod1(function(self, symbolics) {
          checkMethodArity(2, arguments, "apply-symbolic-tensors");
          runtime.checkList(symbolics);
          const inputs = runtime.ffi.toArray(symbolics).map((x) => {
            checkTensor(x);
            return unwrapSymbolicTensor(x);
          });
          const selfLayer = unwrapLayer(self);
          const outputs   = selfLayer.apply(inputs).map(buildTensorObject);
          return runtime.ffi.makeList(outputs);
        })
      });
      obj = applyBrand(brandLayer, obj);
      obj.$underlyingLayer = underlyingLayer;
      return obj;
    }

    /**
     * Additional valid configuration options for activation layers. See
     * `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const ACTIVATION_LAYER_OPTIONS = {
      "activation": {
        // ActivationIdentifier (String)
        jsName: "activation",
        typeCheckAndConvert: checkAndConvertActivationFunction,
        required: true,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js activation layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeActivationLayer(config) {
      arity(1, arguments, "activation-layer", false);
      return makeLayerWith(tf.layers.activation, config, ACTIVATION_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for dense layers. See
     * `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const DENSE_LAYER_OPTIONS = {
      "units": {
        // NumInteger
        jsName: "units",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
        required: true,
      },
      "activation": {
        // ActivationIdentifier (String)
        jsName: "activation",
        typeCheckAndConvert: checkAndConvertActivationFunction,
      },
      "use-bias": {
        // Boolean
        jsName: "useBias",
        typeCheckAndConvert: (v) => {
          runtime.checkBoolean(v);
          return runtime.isPyretTrue(v);
        },
      },
      "kernel-initializer": {
        // Initializer (String)
        jsName: "kernelInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "bias-initializer": {
        // Initializer (String)
        jsName: "biasInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "input-dim": {
        // NumInteger
        jsName: "inputDim",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
      },
      "kernel-constraint": {
        // Constraint (String)
        jsName: "kernelConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "bias-constraint": {
        // Constraint (String)
        jsName: "biasConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "kernel-regularizer": {
        // Regularizer (String)
        jsName: "kernelRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "bias-regularizer": {
        // Regularizer (String)
        jsName: "biasRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "activity-regularizer": {
        // Regularizer (String)
        jsName: "activityRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js dense layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeDenseLayer(config) {
      arity(1, arguments, "dense-layer", false);
      return makeLayerWith(tf.layers.dense, config, DENSE_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for dropout layers. See
     * `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const DROPOUT_LAYER_OPTIONS = {
      "rate": {
        // Number (must be between zero and one)
        jsName: "rate",
        typeCheckAndConvert: (v) => {
          runtime.checkNumber(v);
          const unwrappedNum = runtime.num_to_fixnum(v);
          if (unwrappedNum < 0 || unwrappedNum > 1) {
            runtime.throwMessageException("A dropout layer's rate must be " +
              "between 0 and 1, but the input rate was " + unwrappedNum);
          }
          return unwrappedNum;
        },
        required: true,
      },
      "noise-shape": {
        // List<NumInteger>
        jsName: "noiseShape",
        typeCheckAndConvert: (v) => unwrapListOfNumbersToArray(v, runtime.checkNumInteger),
      },
      "seed": {
        // NumInteger
        jsName: "seed",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js dropout layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeDropoutLayer(config) {
      arity(1, arguments, "dropout-layer", false);
      return makeLayerWith(tf.layers.dropout, config, DROPOUT_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for embedding layers. See
     * `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const EMBEDDING_LAYER_OPTIONS = {
      "input-dim": {
        // NumPositive
        jsName: "inputDim",
        typeCheckAndConvert: (v) => {
          runtime.checkNumPositive(v);
          return runtime.num_to_fixnum(v);
        },
        required: true,
      },
      "output-dim": {
        // NumNonNegative
        jsName: "outputDim",
        typeCheckAndConvert: (v) => {
          runtime.checkNumNonNegative(v);
          return runtime.num_to_fixnum(v);
        },
        required: true,
      },
      "embeddings-initializer": {
        // Initializer (String)
        jsName: "embeddingsInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "embeddings-regularizer": {
        // Regularizer (String)
        jsName: "embeddingsRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "activity-regularizer": {
        // Regularizer (String)
        jsName: "activityRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "embeddings-constraint": {
        // Constraint (String)
        jsName: "embeddingsConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "mask-zero": {
        // Boolean
        jsName: "maskZero",
        typeCheckAndConvert: (v) => {
          runtime.checkBoolean(v);
          return runtime.isPyretTrue(v);
        },
      },
      "input-length": {
        // List<Number>
        jsName: "inputLength",
        typeCheckAndConvert: unwrapListOfNumbersToArray,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js embedding layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeEmbeddingLayer(config) {
      arity(1, arguments, "embedding-layer", false);
      return makeLayerWith(tf.layers.embedding, config, EMBEDDING_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for flatten layers. See
     * `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const FLATTEN_LAYER_OPTIONS = {};

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js flatten layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeFlattenLayer(config) {
      arity(1, arguments, "flatten-layer", false);
      return makeLayerWith(tf.layers.flatten, config, FLATTEN_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for repeat vector layers.
     * See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const REPEAT_VECTOR_LAYER_OPTIONS = {
      "num-repeats": {
        // NumInteger%(is-num-positive)
        jsName: "n",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          runtime.checkNumPositive(v);
          return runtime.num_to_fixnum(v);
        },
        required: true,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js repeat vector layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeRepeatVectorLayer(config) {
      arity(1, arguments, "repeat-vector-layer", false);
      return makeLayerWith(tf.layers.repeatVector, config, REPEAT_VECTOR_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for repeat vector layers.
     * See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const RESHAPE_LAYER_OPTIONS = {
      "target-shape": {
        // List<NumInteger>
        jsName: "targetShape",
        typeCheckAndConvert: (v) => unwrapListOfNumbersToArray(v, runtime.checkNumInteger),
        required: true,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js reshape layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeReshapeLayer(config) {
      arity(1, arguments, "reshape-layer", false);
      return makeLayerWith(tf.layers.reshape, config, RESHAPE_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for one-dimensional,
     * convolution layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the
     * specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const CONV1D_LAYER_OPTIONS = {
      "filters": {
        // NumInteger
        jsName: "filters",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
        required: true,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js one-dimensional, convolution layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeConv1dLayer(config) {
      arity(1, arguments, "conv-1d-layer", false);
      return makeLayerWith(tf.layers.conv1d, config, CONV1D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for two-dimensional,
     * convolution layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the
     * specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const CONV2D_LAYER_OPTIONS = {
      "filters": {
        // NumInteger
        jsName: "filters",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
        required: true,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js two-dimensional, convolution layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeConv2dLayer(config) {
      arity(1, arguments, "conv-2d-layer", false);
      return makeLayerWith(tf.layers.conv2d, config, CONV2D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for "deconvolution" layers.
     * See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const CONV2D_TRANSPOSE_LAYER_OPTIONS = {
      "filters": {
        // NumInteger
        jsName: "filters",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
        required: true,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js transposed, two-dimensional, convolution layer
     * (also known as a "deconvolution" layer).
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeConv2dTransposeLayer(config) {
      arity(1, arguments, "conv-2d-transpose-layer", false);
      return makeLayerWith(tf.layers.conv2dTranspose, config, CONV2D_TRANSPOSE_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for cropping layers.
     * See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const CROPPING_2D_LAYER_OPTIONS = {
      "cropping": {
        // Object
        jsName: "cropping",
        typeCheckAndConvert: (v) => {
          runtime.checkObject(v);
          // Unwrap individual cropping dimensions from object:
          const topCrop    = runtime.getField(v, "top-crop");
          const bottomCrop = runtime.getField(v, "bottom-crop");
          const leftCrop   = runtime.getField(v, "left-crop");
          const rightCrop  = runtime.getField(v, "right-crop");
          // Typecheck individual cropping dimensions:
          runtime.checkNumInteger(topCrop);
          runtime.checkNumInteger(bottomCrop);
          runtime.checkNumInteger(leftCrop);
          runtime.checkNumInteger(rightCrop);
          // tf.layers.cropping2D needs the crop info in a nested array format
          // (see https://js.tensorflow.org/api/0.12.0/#layers.cropping2D):
          const verticalCrops = [topCrop, bottomCrop].map(x => runtime.num_to_fixnum(x));
          const horizontalCrops = [leftCrop, rightCrop].map(x => runtime.num_to_fixnum(x));
          return [verticalCrops, horizontalCrops];
        },
        required: true,
      },
      "data-format": {
        // String
        jsName: "dataFormat",
        typeCheckAndConvert: checkAndConvertDataFormat,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js cropping layer for a two-dimensional input (for
     * example, the pixels of an image).
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeCropping2dLayer(config) {
      arity(1, arguments, "cropping-2d-layer", false);
      return makeLayerWith(tf.layers.cropping2D, config, CROPPING_2D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for depthwise, separable,
     * two-dimensional, convolution layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS`
     * for the specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const DEPTHWISE_CONV_2D_LAYER_OPTIONS = {
      "kernel-size": {
        // Object
        jsName: "kernelSize",
        typeCheckAndConvert: (v) => {
          runtime.checkObject(v);
          // Unwrap individual dimensions from object:
          const width  = runtime.getField(v, "width");
          const height = runtime.getField(v, "height");
          // Typecheck individual dimensions:
          runtime.checkNumInteger(width);
          runtime.checkNumInteger(height);
          // tf.layers.depthwiseConv2d needs the info in this format (see
          // https://js.tensorflow.org/api/0.12.0/#layers.depthwiseConv2d):
          return [width, height].map(x => runtime.num_to_fixnum(x));
        },
        required: true,
      },
      "depth-multiplier": {
        // NumInteger
        jsName: "depthMultiplier",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
      },
      "depthwise-initalizer": {
        // Initializer (String)
        jsName: "depthwiseInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "depthwise-constraint": {
        // Constraint (String)
        jsName: "depthwiseConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "depthwise-regularizer": {
        // Regularizer (String)
        jsName: "depthwiseRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js depthwise, separable, two-dimensional, convolution
     * layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeDepthwiseConv2dLayer(config) {
      arity(1, arguments, "depthwise-conv-2d-layer", false);
      return makeLayerWith(tf.layers.depthwiseConv2d, config, DEPTHWISE_CONV_2D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for separable, two-dimensional,
     * convolution layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the
     * specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const SEPARABLE_CONV_2D_LAYER_OPTIONS = {
      "depth-multiplier": {
        // NumInteger
        jsName: "depthMultiplier",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
      },
      "depthwise-initalizer": {
        // Initializer (String)
        jsName: "depthwiseInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "pointwise-initalizer": {
        // Initializer (String)
        jsName: "pointwiseInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "depthwise-regularizer": {
        // Regularizer (String)
        jsName: "depthwiseRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "pointwise-regularizer": {
        // Regularizer (String)
        jsName: "pointwiseRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "depthwise-constraint": {
        // Constraint (String)
        jsName: "depthwiseConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "pointwise-constraint": {
        // Constraint (String)
        jsName: "pointwiseConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js separable, two-dimensional, convolution layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeSeparableConv2dLayer(config) {
      arity(1, arguments, "separable-conv-2d-layer", false);
      return makeLayerWith(tf.layers.separableConv2d, config, SEPARABLE_CONV_2D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for upsampling layers for
     * two-dimensional inputs. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the
     * specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const UPSAMPLING_2D_LAYER_OPTIONS = {
      "size": {
        // List<Number>
        jsName: "size",
        typeCheckAndConvert: unwrapListOfNumbersToArray,
      },
      "data-format": {
        // String
        jsName: "dataFormat",
        typeCheckAndConvert: checkAndConvertDataFormat,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js upsampling layer for two-dimensional inputs
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeUpSampling2dLayer(config) {
      arity(1, arguments, "up-sampling-2d-layer", false);
      return makeLayerWith(tf.layers.upSampling2d, config, UPSAMPLING_2D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for separable, two-dimensional,
     * convolution layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the
     * specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const ADD_LAYER_OPTIONS = {};

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js separable, two-dimensional, convolution layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeAddLayer(config) {
      arity(1, arguments, "add-layer", false);
      return makeLayerWith(tf.layers.add, config, ADD_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for separable, two-dimensional,
     * convolution layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the
     * specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const AVERAGE_LAYER_OPTIONS = {};

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js separable, two-dimensional, convolution layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeAverageLayer(config) {
      arity(1, arguments, "average-layer", false);
      return makeLayerWith(tf.layers.average, config, AVERAGE_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for concatenation layers.
     * See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const CONCATENATE_LAYER_OPTIONS = {
      "axis": {
        // NumInteger
        jsName: "axis",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js concatenation layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeConcatenateLayer(config) {
      arity(1, arguments, "concatenate-layer", false);
      return makeLayerWith(tf.layers.concatenate, config, CONCATENATE_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for maximum layers.
     * See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const MAXIMUM_LAYER_OPTIONS = {};

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js maximum layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeMaximumLayer(config) {
      arity(1, arguments, "maximum-layer", false);
      return makeLayerWith(tf.layers.maximum, config, MAXIMUM_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for minimum layers.
     * See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const MINIMUM_LAYER_OPTIONS = {};

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js minimum layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeMinimumLayer(config) {
      arity(1, arguments, "minimum-layer", false);
      return makeLayerWith(tf.layers.minimum, config, MINIMUM_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for multiply layers.
     * See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const MULTIPLY_LAYER_OPTIONS = {};

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js multiply layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeMultiplyLayer(config) {
      arity(1, arguments, "multiply-layer", false);
      return makeLayerWith(tf.layers.multiply, config, MULTIPLY_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for batch normalization layers.
     * See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const BATCH_NORMALIZATION_LAYER_OPTIONS = {
      "axis": {
        // NumInteger
        jsName: "axis",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
      },
      "momentum": {
        // Number
        jsName: "momentum",
        typeCheckAndConvert: (v) => {
          runtime.checkNumber(v);
          return runtime.num_to_fixnum(v);
        },
      },
      "epsilon": {
        // Number
        jsName: "epsilon",
        typeCheckAndConvert: (v) => {
          runtime.checkNumber(v);
          return runtime.num_to_fixnum(v);
        },
      },
      "center": {
        // Boolean
        jsName: "center",
        typeCheckAndConvert: (v) => {
          runtime.checkBoolean(v);
          return runtime.isPyretTrue(v);
        },
      },
      "scale": {
        // Boolean
        jsName: "scale",
        typeCheckAndConvert: (v) => {
          runtime.checkBoolean(v);
          return runtime.isPyretTrue(v);
        },
      },
      "beta-initalizer": {
        // Initializer (String)
        jsName: "betaInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "gamma-initalizer": {
        // Initializer (String)
        jsName: "gammaInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "moving-mean-initalizer": {
        // Initializer (String)
        jsName: "movingMeanInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "moving-variance-initalizer": {
        // Initializer (String)
        jsName: "movingVarianceInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "beta-constraint": {
        // Constraint (String)
        jsName: "betaConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "gamma-constraint": {
        // Constraint (String)
        jsName: "gammaConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "beta-regularizer": {
        // Regularizer (String)
        jsName: "betaRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "gamma-regularizer": {
        // Regularizer (String)
        jsName: "gammaRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js batch normalization layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeBatchNormalizationLayer(config) {
      arity(1, arguments, "batch-normalization-layer", false);
      return makeLayerWith(tf.layers.batchNormalization, config, BATCH_NORMALIZATION_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for one-dimensional, average
     * pooling layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the
     * specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const AVERAGE_POOLING_1D_LAYER_OPTIONS = {
      "pool-size": {
        // NumInteger
        jsName: "poolSize",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
      },
      "strides": {
        // Number
        jsName: "strides",
        typeCheckAndConvert: (v) => {
          runtime.checkNumber(v);
          return runtime.num_to_fixnum(v);
        },
      },
      "padding": {
        // PaddingMethod (String)
        jsName: "padding",
        typeCheckAndConvert: checkAndConvertPaddingMethod,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js one-dimensional, average pooling operation layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeAveragePooling1dLayer(config) {
      arity(1, arguments, "average-pooling-1d-layer", false);
      return makeLayerWith(tf.layers.averagePooling1d, config, AVERAGE_POOLING_1D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for two-dimensional, average
     * pooling layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the
     * specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const AVERAGE_POOLING_2D_LAYER_OPTIONS = {
      "pool-size": {
        // Object
        jsName: "poolSize",
        typeCheckAndConvert: (v) => {
          runtime.checkObject(v);
          // Unwrap individual dimensions from object:
          const vertical   = runtime.getField(v, "vertical");
          const horizontal = runtime.getField(v, "horizontal");
          // Typecheck individual dimensions:
          runtime.checkNumInteger(vertical);
          runtime.checkNumInteger(horizontal);
          // tf.layers.averagePooling1d needs the info in this format (see
          // https://js.tensorflow.org/api/0.12.0/#layers.averagePooling2d):
          return [vertical, horizontal].map(x => runtime.num_to_fixnum(x));
        },
      },
      "strides": {
        // Object
        jsName: "strides",
        typeCheckAndConvert: (v) => {
          runtime.checkObject(v);
          // Unwrap individual dimensions from object:
          const vertical   = runtime.getField(v, "vertical");
          const horizontal = runtime.getField(v, "horizontal");
          // Typecheck individual dimensions:
          runtime.checkNumInteger(vertical);
          runtime.checkNumInteger(horizontal);
          // tf.layers.averagePooling1d needs the info in this format (see
          // https://js.tensorflow.org/api/0.12.0/#layers.averagePooling2d):
          return [vertical, horizontal].map(x => runtime.num_to_fixnum(x));
        },
      },
      "padding": {
        // PaddingMethod (String)
        jsName: "padding",
        typeCheckAndConvert: checkAndConvertPaddingMethod,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js two-dimensional, average pooling operation layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeAveragePooling2dLayer(config) {
      arity(1, arguments, "average-pooling-2d-layer", false);
      return makeLayerWith(tf.layers.averagePooling2d, config, AVERAGE_POOLING_2D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for global average pooling
     * operation layers for temporal data. See `DEFAULT_LAYER_OPTIONS_MAPPINGS`
     * for the specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const GLOBAL_AVERAGE_POOLING_1D_LAYER_OPTIONS = {};

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js global average pooling operation layer for temporal
     * data.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeGlobalAveragePooling1dLayer(config) {
      arity(1, arguments, "global-average-pooling-1d-layer", false);
      return makeLayerWith(tf.layers.globalAveragePooling1d, config, GLOBAL_AVERAGE_POOLING_1D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for global average pooling
     * operation layers for spatial data. See `DEFAULT_LAYER_OPTIONS_MAPPINGS`
     * for the specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const GLOBAL_AVERAGE_POOLING_2D_LAYER_OPTIONS = {
      "data-format": {
        // String
        jsName: "dataFormat",
        typeCheckAndConvert: checkAndConvertDataFormat,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js global average pooling operation layer for spatial
     * data.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeGlobalAveragePooling2dLayer(config) {
      arity(1, arguments, "global-average-pooling-2d-layer", false);
      return makeLayerWith(tf.layers.globalAveragePooling2d, config, GLOBAL_AVERAGE_POOLING_2D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for global max pooling
     * operation layers for temporal data. See `DEFAULT_LAYER_OPTIONS_MAPPINGS`
     * for the specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const GLOBAL_MAX_POOLING_1D_LAYER_OPTIONS = {};

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js global max pooling operation layer for temporal data.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeGlobalMaxPooling1dLayer(config) {
      arity(1, arguments, "global-max-pooling-1d-layer", false);
      return makeLayerWith(tf.layers.globalMaxPooling1d, config, GLOBAL_MAX_POOLING_1D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for global max pooling
     * operation layers for spatial data. See `DEFAULT_LAYER_OPTIONS_MAPPINGS`
     * for the specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const GLOBAL_MAX_POOLING_2D_LAYER_OPTIONS = {
      "data-format": {
        // String
        jsName: "dataFormat",
        typeCheckAndConvert: checkAndConvertDataFormat,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js global max pooling operation layer for spatial data.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeGlobalMaxPooling2dLayer(config) {
      arity(1, arguments, "global-max-pooling-2d-layer", false);
      return makeLayerWith(tf.layers.globalMaxPooling2d, config, GLOBAL_MAX_POOLING_2D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for max pooling operation
     * layers for temporal data. See `DEFAULT_LAYER_OPTIONS_MAPPINGS`
     * for the specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const MAX_POOLING_1D_LAYER_OPTIONS = {
      "pool-size": {
        // NumInteger
        jsName: "poolSize",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
      },
      "strides": {
        // Number
        jsName: "strides",
        typeCheckAndConvert: (v) => {
          runtime.checkNumber(v);
          return runtime.num_to_fixnum(v);
        },
      },
      "padding": {
        // PaddingMethod (String)
        jsName: "padding",
        typeCheckAndConvert: checkAndConvertPaddingMethod,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js max pooling operation layer for temporal data.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeMaxPooling1dLayer(config) {
      arity(1, arguments, "max-pooling-1d-layer", false);
      return makeLayerWith(tf.layers.maxPooling1d, config, MAX_POOLING_1D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for max pooling operation
     * layers for spatial data. See `DEFAULT_LAYER_OPTIONS_MAPPINGS`
     * for the specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const MAX_POOLING_2D_LAYER_OPTIONS = {
      "pool-size": {
        // Object
        jsName: "poolSize",
        typeCheckAndConvert: (v) => {
          runtime.checkObject(v);
          // Unwrap individual dimensions from object:
          const vertical   = runtime.getField(v, "vertical");
          const horizontal = runtime.getField(v, "horizontal");
          // Typecheck individual dimensions:
          runtime.checkNumInteger(vertical);
          runtime.checkNumInteger(horizontal);
          // tf.layers.averagePooling1d needs the info in this format (see
          // https://js.tensorflow.org/api/0.12.0/#layers.averagePooling2d):
          return [vertical, horizontal].map(x => runtime.num_to_fixnum(x));
        },
      },
      "strides": {
        // Object
        jsName: "strides",
        typeCheckAndConvert: (v) => {
          runtime.checkObject(v);
          // Unwrap individual dimensions from object:
          const vertical   = runtime.getField(v, "vertical");
          const horizontal = runtime.getField(v, "horizontal");
          // Typecheck individual dimensions:
          runtime.checkNumInteger(vertical);
          runtime.checkNumInteger(horizontal);
          // tf.layers.averagePooling1d needs the info in this format (see
          // https://js.tensorflow.org/api/0.12.0/#layers.averagePooling2d):
          return [vertical, horizontal].map(x => runtime.num_to_fixnum(x));
        },
      },
      "padding": {
        // PaddingMethod (String)
        jsName: "padding",
        typeCheckAndConvert: checkAndConvertPaddingMethod,
      },
      "data-format": {
        // DataFormat (String)
        jsName: "dataFormat",
        typeCheckAndConvert: checkAndConvertDataFormat,
      }
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js max pooling operation layer for spatial data.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeMaxPooling2dLayer(config) {
      arity(1, arguments, "max-pooling-2d-layer", false);
      return makeLayerWith(tf.layers.maxPooling2d, config, MAX_POOLING_2D_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for gated recurrent unit
     * layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification
     * used to construct this object.
     * @constant
     * @type {Object}
     */
    const GRU_LAYER_OPTIONS = {
      "recurrent-activation": {
        // Activation (String)
        jsName: "recurrentActivation",
        typeCheckAndConvert: checkAndConvertActivationFunction,
      },
      "implementation-mode": {
        // Number
        jsName: "implementation",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          const mode = runtime.num_to_fixnum(v);
          if (mode !== 1 || mode !== 2) {
            runtime.throwMessageException("The implementation mode for a " +
              "GRU layer must be either 1 or 2, but was specified to " +
              "be " + mode + ".");
          }
          return mode;
        },
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js gated recurrent unit layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeGruLayer(config) {
      arity(1, arguments, "gru-layer", false);
      return makeLayerWith(tf.layers.gru, config, GRU_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for gated recurrent unit cell
     * layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification
     * used to construct this object.
     * @constant
     * @type {Object}
     */
    const GRU_CELL_LAYER_OPTIONS = {
      "recurrent-activation": {
        // Activation (String)
        jsName: "recurrentActivation",
        typeCheckAndConvert: checkAndConvertActivationFunction,
      },
      "implementation-mode": {
        // Number
        jsName: "implementation",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          const mode = runtime.num_to_fixnum(v);
          if (mode !== 1 || mode !== 2) {
            runtime.throwMessageException("The implementation mode for a " +
              "GRU cell layer must be either 1 or 2, but was specified to " +
              "be " + mode + ".");
          }
          return mode;
        },
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js gated recurrent unit cell layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeGruCellLayer(config) {
      arity(1, arguments, "gru-cell-layer", false);
      return makeLayerWith(tf.layers.gruCell, config, GRU_CELL_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for "long-short term memory"
     * layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification
     * used to construct this object.
     * @constant
     * @type {Object}
     */
    const LSTM_LAYER_OPTIONS = {
      "recurrent-activation": {
        // Activation (String)
        jsName: "recurrentActivation",
        typeCheckAndConvert: checkAndConvertActivationFunction,
      },
      "unit-forget-bias": {
        // Boolean
        jsName: "unitForgetBias",
        typeCheckAndConvert: (v) => {
          runtime.checkBoolean(v);
          return runtime.isPyretTrue(v);
        },
      },
      "implementation-mode": {
        // Number
        jsName: "implementation",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          const mode = runtime.num_to_fixnum(v);
          if (mode !== 1 || mode !== 2) {
            runtime.throwMessageException("The implementation mode for a " +
              "GRU cell layer must be either 1 or 2, but was specified to " +
              "be " + mode + ".");
          }
          return mode;
        },
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js "long-short term memory" layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeLstmLayer(config) {
      arity(1, arguments, "lstm-layer", false);
      return makeLayerWith(tf.layers.lstm, config, LSTM_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for "long-short term memory"
     * cell layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the
     * specification used to construct this object.
     * @constant
     * @type {Object}
     */
    const LSTM_CELL_LAYER_OPTIONS = {
      "recurrent-activation": {
        // Activation (String)
        jsName: "recurrentActivation",
        typeCheckAndConvert: checkAndConvertActivationFunction,
      },
      "unit-forget-bias": {
        // Boolean
        jsName: "unitForgetBias",
        typeCheckAndConvert: (v) => {
          runtime.checkBoolean(v);
          return runtime.isPyretTrue(v);
        },
      },
      "implementation-mode": {
        // Number
        jsName: "implementation",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          const mode = runtime.num_to_fixnum(v);
          if (mode !== 1 || mode !== 2) {
            runtime.throwMessageException("The implementation mode for a " +
              "GRU cell layer must be either 1 or 2, but was specified to " +
              "be " + mode + ".");
          }
          return mode;
        },
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js "long-short term memory" cell layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeLstmCellLayer(config) {
      arity(1, arguments, "lstm-cell-layer", false);
      return makeLayerWith(tf.layers.lstmCell, config, LSTM_CELL_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for recurrent layers. See
     * `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const RNN_LAYER_OPTIONS = {
      "cells": {
        // List<Layer> [where each Layer is an RNN cell Layer]
        // TODO(ZacharyEspiritu): Check that these are specifically
        // RNN cell layers. For now, we're just hoping for the best
        // and doing a naive general Layer check.
        jsName: "cell",
        typeCheckAndConvert: unwrapListOfLayersToArray,
      },
      "stateful": {
        // Boolean
        jsName: "stateful",
        typeCheckAndConvert: (v) => {
          runtime.checkBoolean(v);
          return runtime.isPyretTrue(v);
        },
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js recurrent layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeRNNLayer(config) {
      arity(1, arguments, "rnn-layer", false);
      return makeLayerWith(tf.layers.rnn, config, RNN_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for simple recurrent layers.
     * See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const SIMPLE_RNN_LAYER_OPTIONS = {
      "units": {
        // NumInteger
        jsName: "units",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
        required: true,
      },
      "activation": {
        // ActivationIdentifier (String)
        jsName: "activation",
        typeCheckAndConvert: checkAndConvertActivationFunction,
      },
      "use-bias": {
        // Boolean
        jsName: "useBias",
        typeCheckAndConvert: (v) => {
          runtime.checkBoolean(v);
          return runtime.isPyretTrue(v);
        },
      },
      "kernel-initializer": {
        // Initializer (String)
        jsName: "kernelInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "recurrent-initializer": {
        // Initializer (String)
        jsName: "recurrentInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "bias-initializer": {
        // Initializer (String)
        jsName: "biasInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "kernel-regularizer": {
        // Regularizer (String)
        jsName: "kernelRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "recurrent-regularizer": {
        // Regularizer (String)
        jsName: "recurrentRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "bias-regularizer": {
        // Regularizer (String)
        jsName: "biasRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "kernel-constraint": {
        // Constraint (String)
        jsName: "kernelConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "recurrent-constraint": {
        // Constraint (String)
        jsName: "recurrentConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "bias-constraint": {
        // Constraint (String)
        jsName: "biasConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "dropout": {
        // Number
        jsName: "dropout",
        typeCheckAndConvert: (v) => {
          runtime.checkNumber(v);
          const dropout = runtime.num_to_fixnum(v);
          if (dropout < 0 || dropout > 1) {
            runtime.throwMessageException("The dropout for simple RNN layers " +
              "must be between 0 and 1 (exclusive), but was specified to be " +
              dropout + ".");
          }
          return dropout;
        },
      },
      "recurrent-dropout": {
        // Number
        jsName: "recurrentDropout",
        typeCheckAndConvert: (v) => {
          runtime.checkNumber(v);
          const dropout = runtime.num_to_fixnum(v);
          if (dropout < 0 || dropout > 1) {
            runtime.throwMessageException("The recurrent dropout for simple " +
              "RNN layers must be between 0 and 1 (exclusive), but was " +
              "specified to be " + dropout + ".");
          }
          return dropout;
        },
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js simple recurrent layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeSimpleRNNLayer(config) {
      arity(1, arguments, "simple-rnn-layer", false);
      return makeLayerWith(tf.layers.simpleRNN, config, SIMPLE_RNN_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for simple recurrent cell
     * layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification
     * used to construct this object.
     * @constant
     * @type {Object}
     */
    const SIMPLE_RNN_CELL_LAYER_OPTIONS = {
      "units": {
        // NumInteger
        jsName: "units",
        typeCheckAndConvert: (v) => {
          runtime.checkNumInteger(v);
          return runtime.num_to_fixnum(v);
        },
        required: true,
      },
      "activation": {
        // ActivationIdentifier (String)
        jsName: "activation",
        typeCheckAndConvert: checkAndConvertActivationFunction,
      },
      "use-bias": {
        // Boolean
        jsName: "useBias",
        typeCheckAndConvert: (v) => {
          runtime.checkBoolean(v);
          return runtime.isPyretTrue(v);
        },
      },
      "kernel-initializer": {
        // Initializer (String)
        jsName: "kernelInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "recurrent-initializer": {
        // Initializer (String)
        jsName: "recurrentInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "bias-initializer": {
        // Initializer (String)
        jsName: "biasInitializer",
        typeCheckAndConvert: checkAndConvertInitializerFunction,
      },
      "kernel-regularizer": {
        // Regularizer (String)
        jsName: "kernelRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "recurrent-regularizer": {
        // Regularizer (String)
        jsName: "recurrentRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "bias-regularizer": {
        // Regularizer (String)
        jsName: "biasRegularizer",
        typeCheckAndConvert: checkAndConvertRegularizer,
      },
      "kernel-constraint": {
        // Constraint (String)
        jsName: "kernelConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "recurrent-constraint": {
        // Constraint (String)
        jsName: "recurrentConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "bias-constraint": {
        // Constraint (String)
        jsName: "biasConstraint",
        typeCheckAndConvert: checkAndConvertConstraintFunction,
      },
      "dropout": {
        // Number
        jsName: "dropout",
        typeCheckAndConvert: (v) => {
          runtime.checkNumber(v);
          const dropout = runtime.num_to_fixnum(v);
          if (dropout < 0 || dropout > 1) {
            runtime.throwMessageException("The dropout for simple RNN cell " +
              "layers must be between 0 and 1 (exclusive), but was specified " +
              "to be " + dropout + ".");
          }
          return dropout;
        },
      },
      "recurrent-dropout": {
        // Number
        jsName: "recurrentDropout",
        typeCheckAndConvert: (v) => {
          runtime.checkNumber(v);
          const dropout = runtime.num_to_fixnum(v);
          if (dropout < 0 || dropout > 1) {
            runtime.throwMessageException("The recurrent dropout for simple " +
              "RNN cell layers must be between 0 and 1 (exclusive), but was " +
              "specified to be " + dropout + ".");
          }
          return dropout;
        },
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js simple recurrent cell layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeSimpleRNNCellLayer(config) {
      arity(1, arguments, "simple-rnn-cell-layer", false);
      return makeLayerWith(tf.layers.simpleRNNCell, config, SIMPLE_RNN_CELL_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for stacked, recurrent cell
     * layers. See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification
     * used to construct this object.
     * @constant
     * @type {Object}
     */
    const STACKED_RNN_CELLS_LAYER_OPTIONS = {
      "cells": {
        // List<Layer> [where each Layer is an RNN cell Layer]
        // TODO(ZacharyEspiritu): Check that these are specifically
        // RNN cell layers. For now, we're just hoping for the best
        // and doing a naive general Layer check.
        jsName: "cell",
        typeCheckAndConvert: unwrapListOfLayersToArray,
      },
      "stateful": {
        // Boolean
        jsName: "stateful",
        typeCheckAndConvert: (v) => {
          runtime.checkBoolean(v);
          return runtime.isPyretTrue(v);
        },
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js stacked, recurrent cell layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeStackedRNNCellsLayer(config) {
      arity(1, arguments, "stacked-rnn-cells-layer", false);
      return makeLayerWith(tf.layers.stackedRNNCells, config, STACKED_RNN_CELLS_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for bidirectional layers.
     * See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const BIDIRECTIONAL_LAYER_OPTIONS = {
      "layer": {
        // Layer [should be an RNN layer]
        jsName: "layer",
        typeCheckAndConvert: (v) => {
          // TODO(ZacharyEspiritu): Check that this is specifically a
          // RNN cell layer. For now, we're just hoping for the best
          // and doing a naive general Layer check.
          checkLayer(v);
          return unwrapLayer(v);
        },
        required: true,
      },
      "merge-mode": {
        // BidirectionalMergeMode (String)
        jsName: "mergeMode",
        typeCheckAndConvert: (possibleMergeMode) => {
          const VALID_MERGE_MODES = [
            "sum",
            "mul",
            "concat",
            "ave"
          ];
          // Check that possibleActivation is a known activation function:
          if (!VALID_MERGE_MODES.includes(possibleMergeMode)) {
            runtime.ffi.throwMessageException("\"" + possibleMergeMode + "\" is " +
              "not a valid merge mode. The possible merge modes are " +
              optionsToSentence(VALID_MERGE_MODES) + ".");
          }
          return possibleMergeMode;
        }
      }
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js bidirectional layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeBidirectionalLayer(config) {
      arity(1, arguments, "bidirectional-layer", false);
      return makeLayerWith(tf.layers.bidirectional, config, BIDIRECTIONAL_LAYER_OPTIONS);
    }

    /**
     * Additional valid configuration options for bidirectional layers.
     * See `DEFAULT_LAYER_OPTIONS_MAPPINGS` for the specification used to
     * construct this object.
     * @constant
     * @type {Object}
     */
    const TIME_DISTRIBUTED_LAYER_OPTIONS = {
      "layer": {
        // Layer
        jsName: "layer",
        typeCheckAndConvert: (v) => {
          checkLayer(v);
          return unwrapLayer(v);
        },
        required: true,
      },
    };

    /**
     * Consumes a PyretLayerConfig and returns a PyretLayer representing
     * a TensorFlow.js time distributed layer.
     * @param {PyretLayerConfig} config The configuration to build the
     *  object with
     * @returns {PyretLayer} The newly constructed layer
     */
    function makeTimeDistributedLayer(config) {
      arity(1, arguments, "time-distributed-layer", false);
      return makeLayerWith(tf.layers.timeDistributed, config, TIME_DISTRIBUTED_LAYER_OPTIONS);
    }

    /**
     * Training (Optimizers)
     */

    /**
     * Returns PyretTrue if the input `obj` is a PyretOptimizer; otherwise,
     * returns PyretFalse.
     * @param {Any} obj Some Pyret value
     * @returns {PBoolean} A Pyret object representing true or false
     */
    function isOptimizer(obj) {
      arity(1, arguments, "is-optimizer", false);
      return runtime.makeBoolean(hasBrand(brandOptimizer, obj));
    }

    /**
     * Consumes a PyretOptimizer and returns its underlying TensorFlow.js
     * Optimizer.
     * @param {PyretOptimizer} pyretOptimizer
     * @returns {TFOptimizer} The underlying TensorFlow.js Optimizer of the
     *  input PyretOptimizer
     */
    function unwrapOptimizer(pyretOptimizer) {
      return pyretOptimizer.$underlyingOptimizer;
    }

    /**
     * Consumes a TFOptimizer and wraps it in a PyretObject to make it a
     * PyretOptimizer.
     * @param {TFOptimizer} underlyingOptimizer A TensorFlow.js Optimizer
     * @returns {PyretOptimizer} A new PyretOptimizer with the input
     *  as its underlying TFOptimizer
     */
    function buildOptimizerObject(underlyingOptimizer) {
      let obj = O({
        "_output": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "_output");
          return get(VS, "vs-str").app("<optimizer>");
        }),
        "minimize": runtime.makeMethod2(function(self, functionToMinimize, varList) {
          checkMethodArity(3, arguments, "minimize");
          // varList is a list of mutable tensors for the Optimizer to edit. If
          // it is empty, it should be set to `undefined` so TensorFlow.js
          // knows to modify all available mutable tensors in the space:
          // TODO(ZacharyEspiritu): Check that these are actually mutable;
          // currently only verifies that they are Tensors, not Variables
          const variables = unwrapListOfTensorsToArray(varList);
          if (variables.length === 0) { variables = undefined; }

          // Run minimization thunk. The thunk should return a scalar Tensor.
          // TODO(ZacharyEspiritu): Check that this returns a scalar;
          // currently only verifies that it returns a Tensor, not a scalar
          var selfOptimizer = unwrapOptimizer(self);
          var result = selfOptimizer.minimize(() => {
            return runtime.safeCall(() => {
              return functionToMinimize.app();
            }, checkAndUnwrapTensor);
          }, true, variables);
          return buildTensorObject(result);
        })
      });
      obj = applyBrand(brandOptimizer, obj);
      obj.$underlyingOptimizer = underlyingOptimizer;
      return obj;
    }

    /**
     * Constructs a Optimizer that uses stochastic gradient descent.
     * @param {Number} learningRate The learning rate for the Optimizer
     * @returns {PyretOptimizer} The constructed Optimizer
     */
    function trainSgd(learningRate) {
      arity(1, arguments, "train-sgd", false);
      runtime.checkNumber(learningRate);
      const jsRate = runtime.num_to_fixnum(learningRate);
      return buildOptimizerObject(tf.train.sgd(jsRate));
    }

    /**
     * Constructs a Optimizer that uses momentum gradient descent.
     * @param {Number} learningRate The learning rate for the Optimizer
     * @param {Number} momentum The momentum to use for the momentum
     *  gradient descent algorithm
     * @returns {PyretOptimizer} The constructed Optimizer
     */
    function trainMomentum(learningRate, momentum) {
      arity(2, arguments, "train-momentum", false);
      runtime.checkNumber(learningRate);
      runtime.checkNumber(momentum);
      const jsRate     = runtime.num_to_fixnum(learningRate);
      const jsMomentum = runtime.num_to_fixnum(momentum);
      return buildOptimizerObject(tf.train.momentum(jsRate, jsMomentum));
    }

    /**
     * Constructs a Optimizer that uses the Adagrad algorithm.
     * @param {Number} learningRate The learning rate for the Optimizer
     * @param {Option<Number>} momentum Starting value for the accumulators (if
     *  specified, must be positive)
     * @returns {PyretOptimizer} The constructed Optimizer
     */
    function trainAdagrad(learningRate, initialAccumulatorValue) {
      arity(2, arguments, "train-adagrad", false);
      runtime.checkNumber(learningRate);
      const jsRate    = runtime.num_to_fixnum(learningRate);
      const jsInitial = unwrapFixnumOption(initialAccumulatorValue);
      if (jsInitial && (jsInitial <= 0)) {
        runtime.ffi.throwMessageException("The initial accumulator value " +
          "passed to `train-adagrad` must be positive.");
      }
      return buildOptimizerObject(tf.train.adagrad(jsRate, jsInitial));
    }

    /**
     * Constructs a Optimizer that uses the Adadelta algorithm.
     * @param {Option<Number>} learningRate The learning rate for the Optimizer
     * @param {Option<Number>} rho The learning rate decay over each update
     * @param {Option<Number>} epsilon A constant epsilon used to better
     *  condition the gradient update
     * @returns {PyretOptimizer} The constructed Optimizer
     */
    function trainAdadelta(learningRate, rho, epsilon) {
      arity(3, arguments, "train-adadelta", false);
      const jsRate    = unwrapFixnumOption(learningRate);
      const jsRho     = unwrapFixnumOption(rho);
      const jsEpsilon = unwrapFixnumOption(epsilon);
      return buildOptimizerObject(tf.train.adadelta(jsRate, jsRho, jsEpsilon));
    }

    /**
     * Constructs a Optimizer that uses the Adam algorithm.
     * @param {Option<Number>} learningRate The learning rate for the Optimizer
     * @param {Option<Number>} beta1 The exponential decay rate for the 1st
     *  moment estimates
     * @param {Option<Number>} beta2 The exponential decay rate for the 2nd
     *  moment estimates
     * @param {Option<Number>} epsilon A small constant for numerical stability
     * @returns {PyretOptimizer} The constructed Optimizer
     */
    function trainAdam(learningRate, beta1, beta2, epsilon) {
      arity(4, arguments, "train-adam", false);
      const jsRate    = unwrapFixnumOption(learningRate);
      const jsBeta1   = unwrapFixnumOption(beta1);
      const jsBeta2   = unwrapFixnumOption(beta2);
      const jsEpsilon = unwrapFixnumOption(epsilon);
      return buildOptimizerObject(tf.train.adam(jsRate, jsBeta1, jsBeta2, jsEpsilon));
    }

    /**
     * Constructs a Optimizer that uses the Adamax algorithm.
     * @param {Option<Number>} learningRate The learning rate for the Optimizer
     * @param {Option<Number>} beta1 The exponential decay rate for the 1st
     *  moment estimates
     * @param {Option<Number>} beta2 The exponential decay rate for the 2nd
     *  moment estimates
     * @param {Option<Number>} epsilon A small constant for numerical stability
     * @param {Option<Number>} decay The learning rate decay over each update
     * @returns {PyretOptimizer} The constructed Optimizer
     */
    function trainAdamax(learningRate, beta1, beta2, epsilon, decay) {
      arity(5, arguments, "train-adamax", false);
      const jsRate    = unwrapFixnumOption(learningRate);
      const jsBeta1   = unwrapFixnumOption(beta1);
      const jsBeta2   = unwrapFixnumOption(beta2);
      const jsEpsilon = unwrapFixnumOption(epsilon);
      const jsDecay   = unwrapFixnumOption(decay);
      return buildOptimizerObject(tf.train.adamax(jsRate, jsBeta1, jsBeta2, jsEpsilon, jsDecay));
    }

    /**
     * Constructs a Optimizer that uses the RMSProp gradient descent.
     * @param {Number} learningRate The learning rate for the Optimizer
     * @param {Option<Number>} decay The discounting factor for the
     *  history/coming gradient
     * @param {Option<Number>} momentum The momentum to use for the RMSProp
     *  gradient descent algorithm
     * @param {Option<Number>} epsilon Small value to avoid zero denominator
     * @param {Boolean} centered If true, gradients are normalized by the
     *  estimated variance of the gradient
     * @returns {PyretOptimizer} The constructed Optimizer
     */
    function trainRmsprop(learningRate, decay, momentum, epsilon, centered) {
      arity(5, arguments, "train-rmsprop", false);
      runtime.checkNumber(learningRate);
      runtime.checkBoolean(centered);
      const jsRate     = runtime.num_to_fixnum(learningRate);
      const jsDecay    = unwrapFixnumOption(decay);
      const jsMomentum = unwrapFixnumOption(momentum);
      const jsEpsilon  = unwrapFixnumOption(epsilon);
      const jsCentered = runtime.isPyretTrue(centered);
      return buildOptimizerObject(tf.train.rmsprop(jsRate, jsDecay, jsMomentum, jsEpsilon, jsCentered));
    }

    var values = {
      // Tensors
      "is-tensor": F(isTensor, "is-tensor"),
      "list-to-tensor": F(listToTensor, "list-to-tensor"),
      "make-scalar": F(makeScalar, "make-scalar"),
      "fill": F(fill, "fill"),
      "tensor": O({
        make: F(createTensorFromArray, "tensor:make"),
        make0: F(createTensor0, "tensor:make0"),
        make1: F(createTensor1, "tensor:make1"),
        make2: F(createTensor2, "tensor:make2"),
        make3: F(createTensor3, "tensor:make3"),
        make4: F(createTensor4, "tensor:make4"),
        make5: F(createTensor5, "tensor:make5")
      }),
      "linspace": F(linspace, "linspace"),
      "ones": F(ones, "ones"),
      "zeros": F(zeros, "zeros"),
      "multinomial": F(multinomial, "multinomial"),
      "random-normal": F(randomNormal, "random-normal"),
      "random-uniform": F(randomUniform, "random-uniform"),
      "make-variable": F(makeVariable, "make-variable"),

      // TensorBuffers
      "is-tensor-buffer": F(isTensorBuffer, "is-tensor-buffer"),
      "make-buffer": F(makeBuffer, "make-buffer"),

      // Operations (Arithmetic)
      "add-tensors": F(addTensors, "add-tensors"),
      "subtract-tensors": F(subtractTensors, "subtract-tensors"),
      "multiply-tensors": F(multiplyTensors, "multiply-tensors"),
      "divide-tensors": F(divideTensors, "divide-tensors"),
      "floor-divide-tensors": F(floorDivideTensors, "floor-divide-tensors"),
      "tensor-max": F(maxTensor, "tensor-max"),
      "tensor-min": F(minTensor, "tensor-min"),
      "tensor-modulo": F(moduloTensor, "tensor-modulo"),
      "tensor-expt": F(exptTensor, "tensor-expt"),
      "squared-difference": F(tensorSquaredDifference, "squared-difference"),

      "strict-add-tensors": F(addStrict, "strict-add-tensors"),
      "strict-subtract-tensors": F(subtractStrict, "strict-subtract-tensors"),
      "strict-multiply-tensors": F(multiplyStrict, "strict-multiply-tensors"),
      "strict-divide-tensors": F(divideStrict, "strict-divide-tensors"),
      "strict-tensor-max": F(maxStrict, "strict-tensor-max"),
      "strict-tensor-min": F(minStrict, "strict-tensor-min"),
      "strict-tensor-modulo": F(moduloStrict, "strict-tensor-modulo"),
      "strict-tensor-expt": F(exptStrict, "strict-tensor-expt"),
      "strict-squared-difference": F(strictSquaredDifference, "strict-squared-difference"),

      // Operations (Basic Math)
      "tensor-abs": F(abs, "tensor-abs"),
      "tensor-acos": F(acos, "tensor-acos"),
      "tensor-acosh": F(acosh, "tensor-acosh"),
      "tensor-asin": F(asin, "tensor-asin"),
      "tensor-asinh": F(asinh, "tensor-asinh"),
      "tensor-atan": F(atan, "tensor-atan"),
      "tensor-atan2": F(atan2, "tensor-atan2"),
      "tensor-atanh": F(atanh, "tensor-atanh"),
      "tensor-ceil": F(ceil, "tensor-ceil"),
      "clip-by-value": F(clipByValue, "clip-by-value"),
      "tensor-cos": F(cos, "tensor-cos"),
      "tensor-cosh": F(cosh, "tensor-cosh"),
      "elu": F(elu, "elu"),
      "exponential-linear-units": F(exponentialLinearUnits, "exponential-linear-units"),
      "erf": F(erf, "erf"),
      "gauss-error": F(gaussError, "gauss-error"),
      "tensor-exp": F(exp, "tensor-exp"),
      "tensor-exp-min1": F(expm1, "tensor-exp-min1"),
      "tensor-floor": F(floor, "tensor-floor"),
      "leaky-relu": F(leakyRelu, "leaky-relu"),
      "tensor-log": F(log, "tensor-log"),
      "tensor-log-plus1": F(log1p, "tensor-log-plus1"),
      "log-sigmoid": F(logSigmoid, "log-sigmoid"),
      "tensor-negate": F(neg, "tensor-negate"),
      "parametric-relu": F(prelu, "parametric-relu"),
      "tensor-reciprocal": F(reciprocal, "tensor-reciprocal"),
      "relu": F(relu, "relu"),
      "tensor-round": F(round, "tensor-round"),
      "reciprocal-sqrt": F(rsqrt, "reciprocal-sqrt"),
      "scaled-elu": F(selu, "scaled-elu"),
      "sigmoid": F(sigmoid, "sigmoid"),
      "signed-ones": F(sign, "signed-ones"),
      "tensor-sin": F(sin, "tensor-sin"),
      "tensor-sinh": F(sinh, "tensor-sinh"),
      "softplus": F(softplus, "softplus"),
      "tensor-sqrt": F(sqrt, "tensor-sqrt"),
      "tensor-square": F(square, "tensor-square"),
      "step": F(step, "step"),
      "tensor-tan": F(tan, "tensor-tan"),
      "tensor-tanh": F(tanh, "tensor-tanh"),

      // Operations (Reduction)
      "reduce-all": F(all, "reduce-all"),
      "reduce-any": F(any, "reduce-any"),
      "arg-max": F(argMax, "arg-max"),
      "arg-min": F(argMin, "arg-min"),
      "log-sum-exp": F(logSumExp, "log-sum-exp"),
      "reduce-max": F(max, "reduce-max"),
      "reduce-mean": F(mean, "reduce-mean"),
      "reduce-min": F(min, "reduce-min"),
      "reduce-sum": F(sum, "reduce-sum"),

      // Operations (Slicing and Joining)
      "reshape": F(reshapeTensor, "reshape"),
      "concatenate": F(concatenate, "concatenate"),
      "gather": F(gather, "gather"),
      "reverse": F(reverse, "reverse"),
      "slice": F(slice, "slice"),
      "split": F(split, "split"),
      "stack": F(stack, "stack"),
      "tile": F(tile, "tile"),
      "unstack": F(unstack, "unstack"),
      "strided-slice": F(stridedSlice, "strided-slice"),

      // Models (Generic)
      "is-model": F(isModel, "is-model"),
      "make-model": F(makeModel, "make-model"),

      // Models (Sequential)
      "is-sequential": F(isSequential, "is-sequential"),
      "make-sequential": F(makeSequential, "make-sequential"),

      // Models (Inputs / SymbolicTensors)
      "make-input": F(makeInput, "make-input"),
      "make-batch-input": F(makeBatchInput, "make-batch-input"),

      // Layers
      "is-layer": F(isLayer, "is-layer"),
      "layers": O({
        "activation": F(makeActivationLayer, "activation"),
        "dense": F(makeDenseLayer, "dense"),
        "dropout": F(makeDropoutLayer, "dropout"),
        "embedding": F(makeEmbeddingLayer, "embedding"),
        "flatten": F(makeFlattenLayer, "flatten"),
        "repeat-vector": F(makeRepeatVectorLayer, "repeat-vector"),
        "reshape": F(makeReshapeLayer, "reshape"),
        "conv-1d": F(makeConv1dLayer, "conv-1d"),
        "conv-2d": F(makeConv2dLayer, "conv-2d"),
        "conv-2d-transpose": F(makeConv2dTransposeLayer, "conv-2d-transpose"),
        "cropping-2d": F(makeCropping2dLayer, "cropping-2d"),
        "depthwise-conv-2d": F(makeDepthwiseConv2dLayer, "depthwise-conv-2d"),
        "separable-conv-2d": F(makeSeparableConv2dLayer, "separable-conv-2d"),
        "up-sampling-2d": F(makeUpSampling2dLayer, "up-sampling-2d"),
        "add": F(makeAddLayer, "add"),
        "average": F(makeAverageLayer, "average"),
        "concatenate": F(makeConcatenateLayer, "concatenate"),
        "maximum": F(makeMaximumLayer, "maximum"),
        "minimum": F(makeMinimumLayer, "minimum"),
        "multiply": F(makeMultiplyLayer, "multiply"),
        "batch-normalization": F(makeBatchNormalizationLayer, "batch-normalization"),
        "average-pooling-1d": F(makeAveragePooling1dLayer, "average-pooling-1d"),
        "average-pooling-2d": F(makeAveragePooling2dLayer, "average-pooling-2d"),
        "global-average-pooling-1d": F(makeGlobalAveragePooling1dLayer, "global-average-pooling-1d"),
        "global-average-pooling-2d": F(makeGlobalAveragePooling2dLayer, "global-average-pooling-2d"),
        "global-max-pooling-1d": F(makeGlobalMaxPooling1dLayer, "global-max-pooling-1d"),
        "global-max-pooling-2d": F(makeGlobalMaxPooling2dLayer, "global-max-pooling-2d"),
        "max-pooling-1d": F(makeMaxPooling1dLayer, "max-pooling-1d"),
        "max-pooling-2d": F(makeMaxPooling2dLayer, "max-pooling-2d"),
        "gru": F(makeGruLayer, "gru"),
        "gru-cell": F(makeGruCellLayer, "gru-cell"),
        "lstm": F(makeLstmLayer, "lstm"),
        "lstm-cell": F(makeLstmCellLayer, "lstm-cell"),
        "rnn": F(makeRNNLayer, "rnn"),
        "simple-rnn": F(makeSimpleRNNLayer, "simple-rnn"),
        "simple-rnn-cell": F(makeSimpleRNNCellLayer, "simple-rnn-cell"),
        "stacked-rnn-cells": F(makeStackedRNNCellsLayer, "stacked-rnn-cells"),
        "bidirectional": F(makeBidirectionalLayer, "bidirectional"),
        "time-distributed": F(makeTimeDistributedLayer, "time-distributed"),
      }),
      "activation-layer": F(makeActivationLayer, "activation-layer"),
      "dense-layer": F(makeDenseLayer, "dense-layer"),
      "dropout-layer": F(makeDropoutLayer, "dropout-layer"),
      "embedding-layer": F(makeEmbeddingLayer, "embedding-layer"),
      "flatten-layer": F(makeFlattenLayer, "flatten-layer"),
      "repeat-vector-layer": F(makeRepeatVectorLayer, "repeat-vector-layer"),
      "reshape-layer": F(makeReshapeLayer, "reshape-layer"),
      "conv-1d-layer": F(makeConv1dLayer, "conv-1d-layer"),
      "conv-2d-layer": F(makeConv2dLayer, "conv-2d-layer"),
      "conv-2d-transpose-layer": F(makeConv2dTransposeLayer, "conv-2d-transpose-layer"),
      "cropping-2d-layer": F(makeCropping2dLayer, "cropping-2d-layer"),
      "depthwise-conv-2d-layer": F(makeDepthwiseConv2dLayer, "depthwise-conv-2d-layer"),
      "separable-conv-2d-layer": F(makeSeparableConv2dLayer, "separable-conv-2d-layer"),
      "up-sampling-2d-layer": F(makeUpSampling2dLayer, "up-sampling-2d-layer"),
      "add-layer": F(makeAddLayer, "add-layer"),
      "average-layer": F(makeAverageLayer, "average-layer"),
      "concatenate-layer": F(makeConcatenateLayer, "concatenate-layer"),
      "maximum-layer": F(makeMaximumLayer, "maximum-layer"),
      "minimum-layer": F(makeMinimumLayer, "minimum-layer"),
      "multiply-layer": F(makeMultiplyLayer, "multiply-layer"),
      "batch-normalization-layer": F(makeBatchNormalizationLayer, "batch-normalization-layer"),
      "average-pooling-1d-layer": F(makeAveragePooling1dLayer, "average-pooling-1d-layer"),
      "average-pooling-2d-layer": F(makeAveragePooling2dLayer, "average-pooling-2d-layer"),
      "global-average-pooling-1d-layer": F(makeGlobalAveragePooling1dLayer, "global-average-pooling-1d-layer"),
      "global-average-pooling-2d-layer": F(makeGlobalAveragePooling2dLayer, "global-average-pooling-2d-layer"),
      "global-max-pooling-1d-layer": F(makeGlobalMaxPooling1dLayer, "global-max-pooling-1d-layer"),
      "global-max-pooling-2d-layer": F(makeGlobalMaxPooling2dLayer, "global-max-pooling-2d-layer"),
      "max-pooling-1d-layer": F(makeMaxPooling1dLayer, "max-pooling-1d-layer"),
      "max-pooling-2d-layer": F(makeMaxPooling2dLayer, "max-pooling-2d-layer"),
      "gru-layer": F(makeGruLayer, "gru-layer"),
      "gru-cell-layer": F(makeGruCellLayer, "gru-cell-layer"),
      "lstm-layer": F(makeLstmLayer, "lstm-layer"),
      "lstm-cell-layer": F(makeLstmCellLayer, "lstm-cell-layer"),
      "rnn-layer": F(makeRNNLayer, "rnn-layer"),
      "simple-rnn-layer": F(makeSimpleRNNLayer, "simple-rnn-layer"),
      "simple-rnn-cell-layer": F(makeSimpleRNNCellLayer, "simple-rnn-cell-layer"),
      "stacked-rnn-cells-layer": F(makeStackedRNNCellsLayer, "stacked-rnn-cells-layer"),
      "bidirectional-layer": F(makeBidirectionalLayer, "bidirectional-layer"),
      "time-distributed-layer": F(makeTimeDistributedLayer, "time-distributed-layer"),

      // Training (Optimizers)
      "is-optimizer": F(isOptimizer, "is-optimizer"),
      "train": O({
        "sgd": F(trainSgd, "sgd"),
        "momentum": F(trainMomentum, "momentum"),
        "adagrad": F(trainAdagrad, "adagrad"),
        "adadelta": F(trainAdadelta, "sgd"),
        "adam": F(trainAdam, "adam"),
        "adamax": F(trainAdamax, "adamax"),
        "rmsprop": F(trainRmsprop, "rmsprop"),
      }),
      "train-sgd": F(trainSgd, "train-sgd"),
      "train-momentum": F(trainMomentum, "train-momentum"),
      "train-adagrad": F(trainAdagrad, "train-adagrad"),
      "train-adadelta": F(trainAdadelta, "train-adadelta"),
      "train-adam": F(trainAdam, "train-adam"),
      "train-adamax": F(trainAdamax, "train-adamax"),
      "train-rmsprop": F(trainRmsprop, "train-rmsprop"),
    };
    var types = {
      Tensor: annTensor,
      TensorBuffer: annTensorBuffer,
      Model: annModel,
      Sequential: annSequential,
      SymbolicTensor: annSymbolicTensor,
      Layer: annLayer,
      Optimizer: annOptimizer,
    };
    var internal = {
      checkTensor: checkTensor,
      checkSequential: checkSequential,
      checkModel: checkModel,
      checkSequential: checkSequential,
      checkSymbolicTensor: checkSymbolicTensor,
      checkLayer: checkLayer,
      checkOptimizer: checkOptimizer
    };
    return runtime.makeModuleReturn(values, types, internal);
  }
})
