({
  requires: [
    { "import-type": "builtin", name: "valueskeleton" }
  ],
  nativeRequires: ["@tensorflow/tfjs"],
  provides: {
    shorthands: {
      "VS": {
        tag: "name",
        origin: {
          "import-type": "uri",
          uri: "builtin://valueskeleton"
        },
        name: "ValueSkeleton"
      },
    },
    values: {
      "is-tensor": ["arrow", ["Any"], "Boolean"],
      "is-tensor-buffer": ["arrow", ["Any"], "Boolean"],
      "is-model": ["arrow", ["Any"], "Boolean"],
      "is-sequential": ["arrow", ["Any"], "Boolean"],
      "is-symbolic-tensor": ["arrow", ["Any"], "Boolean"],
      "is-layer": ["arrow", ["Any"], "Boolean"],
      "is-optimizer": ["arrow", ["Any"], "Boolean"],

      "tensor": ["Maker", "Number", ["local", "Tensor"]],
    },
    datatype: {
      "Tensor": ["data", "Tensor", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
      }],
      "TensorBuffer": ["data", "TensorBuffer", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
      }],
      "Model": ["data", "Model", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
      }],
      "Sequential": ["data", "Sequential", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
      }],
      "SymbolicTensor": ["data", "SymbolicTensor", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
      }],
      "Layer": ["data", "Layer", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
      }],
      "Optimizer": ["data", "Optimizer", [], [], {
        "_output": ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
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
    var brandTensor = runtime.namedBrander("tensor", ["tensor: tensor brander"]);
    var annTensor = runtime.makeBranderAnn(brandTensor, "Tensor");

    /**
     * The Pyret version of a TensorFlow.js TensorBuffer.
     * @typedef {Object} PyretTensorBuffer
     */
    var brandTensorBuffer = runtime.namedBrander("tensor-buffer", ["tensor-buffer: tensor-buffer brander"]);
    var annTensorBuffer = runtime.makeBranderAnn(brandTensorBuffer, "TensorBuffer");

    /**
     * The Pyret version of a TensorFlow.js Model.
     * @typedef {Object} PyretModel
     */
    var brandModel = runtime.namedBrander("model", ["model: model brander"]);
    var annModel = runtime.makeBranderAnn(brandModel, "Model");

    /**
     * The Pyret version of a TensorFlow.js Sequential.
     * @typedef {Object} PyretSequential
     */
    var brandSequential = runtime.namedBrander("sequential", ["sequential: sequential brander"]);
    var annSequential = runtime.makeBranderAnn(brandSequential, "Sequential");

    /**
     * The Pyret version of a TensorFlow.js SymbolicTensor.
     * @typedef {Object} PyretSymbolicTensor
     */
    var brandSymbolicTensor = runtime.namedBrander("symbolic-tensor", ["symbolic-tensor: symbolic-tensor brander"]);
    var annSymbolicTensor = runtime.makeBranderAnn(brandSymbolicTensor, "SymbolicTensor");

    /**
     * The Pyret version of a TensorFlow.js Layer.
     * @typedef {Object} PyretLayer
     */
    var brandLayer = runtime.namedBrander("layer", ["layer: layer brander"]);
    var annLayer = runtime.makeBranderAnn(brandLayer, "Layer");

    /**
     * The Pyret version of a TensorFlow.js Optimizer.
     * @typedef {Object} PyretOptimizer
     */
    var brandOptimizer = runtime.namedBrander("optimizer", ["optimizer: optimizer brander"]);
    var annOptimizer = runtime.makeBranderAnn(brandOptimizer, "Optimizer");

    /**
     * Runtime Helpers
     */
    var O = runtime.makeObject;
    var F = runtime.makeFunction;
    var arity = runtime.checkArity;
    var get = runtime.getField;
    var unwrap = runtime.unwrap;

    var VS = get(VSlib, "values");

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

    var unwrapObject = (obj) => { return unwrap(obj).dict; };

    var checkTensor = (val) => { runtime._checkAnn(["tensor"], annTensor, val); };
    var checkTensorBuffer = (val) => { runtime._checkAnn(["tensor-buffer"], annTensorBuffer, val); };
    var checkSequential = (val) => { runtime._checkAnn(["sequential"], annSequential, val); };
    var checkModel = (val) => { runtime._checkAnn(["model"], annModel, val); };
    var checkSymbolicTensor = (val) => { runtime._checkAnn(["symbolic-tensor"], annSymbolicTensor, val); };
    var checkLayer = (val) => { runtime._checkAnn(["layer"], annLayer, val); };
    var checkOptimizer = (val) => { runtime._checkAnn(["optimizer"], annOptimizer, val); };

    function checkMethodArity(arity, args, methodName) {
      if (args.length !== arity) {
        var $a=new Array(args.length);
        for (var $i=0;$i<args.length;$i++) {
          $a[$i]=args[$i];
        }
        throw runtime.ffi.throwArityErrorC([methodName], arity, $a, true);
      }
    }

    function unwrapFixnumOption(option) {
      return runtime.ffi.cases(runtime.ffi.isOption, "is-Option", option, {
        some: (v) => { return runtime.num_to_fixnum(v); },
        none: () => { return undefined; }
      });
    }

    /**
     * Tensors
     */

    // Brand Checks

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

    // Constructor

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
     * Consumes a TFTensor and wraps it in a PyretObject to make it a
     * PyretTensor.
     * @param {TFTensor} underlyingBuffer A TensorFlow.js Tensor
     * @returns {PyretTensor} A new PyretTensor with the input as its
     *  underlying TFTensor
     */
    function buildTensorObject(underlyingTensor) {
      var obj = O({
        "_output": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "_output");
          var selfTensor = unwrapTensor(self);
          var elts = [];
          var tensorData = Array.from(selfTensor.dataSync());
          var vsValue = get(VS, "vs-value");
          for (var i = 0; i < tensorData.length; i++) {
            var wrappedNum = runtime.num_to_roughnum(tensorData[i]);
            elts.push(vsValue.app(wrappedNum));
          }
          return get(VS, "vs-collection").app(
            runtime.makeString("tensor"),
            runtime.ffi.makeList(elts));
        }),
        "size": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "size");
          var selfTensor = unwrapTensor(self);
          return runtime.makeNumber(selfTensor.size);
        }),
        "shape": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "shape");
          var selfTensor = unwrapTensor(self);
          return runtime.ffi.makeList(selfTensor.shape);
        }),
        "flatten": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "flatten");
          var selfTensor = unwrapTensor(self);
          return buildTensorObject(selfTensor.flatten());
        }),
        "as-scalar": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "as-scalar");
          var selfTensor = unwrapTensor(self);
          if (selfTensor.size !== 1) {
            runtime.ffi.throwMessageException("Tensor was size-" +
              selfTensor.size + " but `as-scalar` requires the " +
              "tensor to be size-1");
          }
          return buildTensorObject(selfTensor.asScalar());
        }),
        "as-1d": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "as-1d");
          var selfTensor = unwrapTensor(self);
          return buildTensorObject(selfTensor.as1D());
        }),
        "as-2d": runtime.makeMethod2(function(self, rows, columns) {
          checkMethodArity(3, arguments, "as-2d");
          runtime.checkNumInteger(rows);
          runtime.checkNumInteger(columns);
          var r = runtime.num_to_fixnum(rows);
          var c = runtime.num_to_fixnum(columns);

          var selfTensor = unwrapTensor(self);
          if (selfTensor.size !== (r * c)) {
            runtime.ffi.throwMessageException("Cannot reshape because the number " +
              "of entry spaces in the new shape must be equal to the number of " +
              "existing entries");
          }
          return buildTensorObject(selfTensor.as2D(r, c));
        }),
        "as-3d": runtime.makeMethod3(function(self, rows, columns, depth) {
          checkMethodArity(4, arguments, "as-3d");
          runtime.checkNumInteger(rows);
          runtime.checkNumInteger(columns);
          runtime.checkNumInteger(depth);
          var r = runtime.num_to_fixnum(rows);
          var c = runtime.num_to_fixnum(columns);
          var d = runtime.num_to_fixnum(depth);

          var selfTensor = unwrapTensor(self);
          if (selfTensor.size !== (r * c * d)) {
            runtime.ffi.throwMessageException("Cannot reshape because the number " +
              "of entry spaces in the new shape must be equal to the number of " +
              "existing entries");
          }
          return buildTensorObject(selfTensor.as3D(r, c, d));
        }),
        "as-4d": runtime.makeMethod4(function(self, rows, columns, depth1, depth2) {
          checkMethodArity(5, arguments, "as-4d");
          runtime.checkNumInteger(rows);
          runtime.checkNumInteger(columns);
          runtime.checkNumInteger(depth1);
          runtime.checkNumInteger(depth2);
          var r = runtime.num_to_fixnum(rows);
          var c = runtime.num_to_fixnum(columns);
          var d1 = runtime.num_to_fixnum(depth1);
          var d2 = runtime.num_to_fixnum(depth2);

          var selfTensor = unwrapTensor(self);
          if (selfTensor.size !== (r * c * d1 * d2)) {
            runtime.ffi.throwMessageException("Cannot reshape because the number " +
              "of entry spaces in the new shape must be equal to the number of " +
              "existing entries");
          }
          return buildTensorObject(selfTensor.as4D(r, c, d1, d2));
        }),
        "as-type": runtime.makeMethod1(function(self, datatype) {
          checkMethodArity(2, arguments, "as-type");
          var selfTensor = unwrapTensor(self);
          runtime.checkString(datatype);
          var type = unwrap(datatype);
          if (type !== "float32" || type !== "int32" || type !== "bool") {
            runtime.ffi.throwMessageException("Attempted to cast tensor to " +
              "invalid type (" + type + "); valid types are 'float32', 'int32', " +
              "or 'bool'");
          }
          return buildTensorObject(selfTensor.asType(type));
        }),
        "to-buffer": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-buffer");
          var selfTensor = unwrapTensor(self);
          var newBuffer = selfTensor.buffer();
          return buildTensorBufferObject(newBuffer);
        }),
        "data-sync": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "data-sync");
          var selfTensor = unwrapTensor(self);
          // .dataSync returns a TypedArray, so convert it to a normal JSArray
          // so we can then convert it to a Pyret List:
          var typedArrayData = selfTensor.dataSync();
          var arrayData = Array.from(typedArrayData);
          // Convert to Roughnums, since the numbers returned from a Tensor are
          // floating point:
          arrayData = arrayData.map((x) => { return runtime.num_to_roughnum(x); });
          return runtime.ffi.makeList(arrayData);
        }),
        "to-float": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-float");
          var selfTensor = unwrapTensor(self);
          return buildTensorObject(selfTensor.toFloat());
        }),
        "to-int": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-int");
          var selfTensor = unwrapTensor(self);
          return buildTensorObject(selfTensor.toInt());
        }),
        "to-bool": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-bool");
          var selfTensor = unwrapTensor(self);
          return buildTensorObject(selfTensor.toBool());
        }),
        "to-variable": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-variable");
          return makeVariable(self);
        }),
        "reshape": runtime.makeMethod0(function(self, newShape) {
          checkMethodArity(2, arguments, "reshape");
          var selfTensor = unwrapTensor(self);
          runtime.checkList(newShape);
          var ns = runtime.toArray(newShape);
          var product = ns.reduce((a, b) => { return a * b; }, 1);
          if (selfTensor.size !== product) {
            runtime.ffi.throwMessageException("Cannot reshape because the number " +
              "of entry spaces in the new shape must be equal to the number of " +
              "existing entries");
          }
          return buildTensorObject(selfTensor.reshape(ns));
        }),
        "expand-dims": runtime.makeMethod1(function(self, axis) {
          checkMethodArity(2, arguments, "expand-dims");
          var selfTensor = unwrapTensor(self);
          var a = unwrapFixnumOption(axis);
          return buildTensorObject(selfTensor.expandDims(a));
        }),
        "squeeze": runtime.makeMethod1(function(self, axes) {
          checkMethodArity(2, arguments, "squeeze");
          var selfTensor = unwrapTensor(self);
          var a = runtime.ffi.cases(runtime.ffi.isOption, "is-Option", axes, {
            some: (v) => {
              runtime.checkList(v);
              return runtime.ffi.toArray(v).map((x) => { return runtime.num_to_fixnum(x); });
            },
            none: () => { return undefined; }
          });
          return buildTensorObject(selfTensor.squeeze(a));
        }),
        "clone": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "clone");
          var selfTensor = unwrapTensor(self);
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
     * @param {JSArray<Number>} array An array of types
     * @returns {PyretTensor} A Tensor
     */
    function createTensorFromArray(array) {
      arity(1, arguments, "tensor", false);
      runtime.checkArray(array);
      var fixnums = array.map((x) => {
        runtime.checkNumber(x);
        return runtime.num_to_fixnum(x);
      });
      var underlyingTensor = tf.tensor(fixnums, null, null);
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
      // A tensor can be rank 0 (just a number); otherwise, it is a List :(
      runtime.checkList(values);
      values = runtime.ffi.toArray(values);
      return createTensorFromArray(values);
    }

    /**
     * Creates a PyretTensor of rank-0 with the input value.
     * @param {Number} value The value to put into the Tensor
     * @returns {PyretTensor} A PyretTensor with the input value
     */
    function makeScalar(value) {
      arity(1, arguments, "make-scalar", false);
      runtime.checkNumber(value);
      var fixnum = runtime.num_to_fixnum(value);
      var newScalar = tf.scalar(fixnum);
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
      runtime.checkList(shape);
      runtime.checkNumber(value);
      var s = runtime.ffi.toArray(shape).map((x) => {
        runtime.checkNumber(value);
        return runtime.num_to_fixnum(x);
      });
      var v = runtime.num_to_fixnum(value);
      return buildTensorObject(tf.fill(s, v));
    }

    /**
     * Creates a PyretTensor where all of the values are sampled from a
     * multinomial distribution.
     * @param {PyretTensor} logits An one-dimensional Tensor representing
     *  unnormalized log-probabilities
     * @param {Number} numSamples The number of samples to draw for each row
     *  slice
     * @param {Option<Number>} seed The randomization seed to use; if none,
     *  uses TensorFlow.js defaults
     * @param {Boolean} normalized Whether the provided logits are normalized
     *  true probabilities (sum to 1)
     * @returns {PyretTensor}
     */
    function multinomial(logits, numSamples, randomSeed, normalized) {
      arity(4, arguments, "multinomial", false);
      checkTensor(logits);
      runtime.checkNumber(numSamples);
      runtime.checkBoolean(normalized);
      var tensor  = unwrapTensor(logits);
      var samples = runtime.num_to_fixnum(numSamples);
      var seed    = unwrapFixnumOption(randomSeed);
      var norm    = runtime.isPyretTrue(normalized);
      // Check if `logits` is 1D or 2D, as required by the `multinomial`
      // function:
      var dimensions = tensor.shape.length;
      if (dimensions !== 1 && dimensions !== 2) {
        runtime.ffi.throwMessageException("The `logits` argument must be a " +
          "one-dimensional or two-dimensional Tensor");
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
      runtime.checkList(shape);
      var s = runtime.ffi.toArray(shape);
      s.forEach((x) => { runtime.checkNumInteger(x); });
      var m = unwrapFixnumOption(mean);
      var d = unwrapFixnumOption(standardDeviation);
      return buildTensorObject(tf.randomNormal(s, m, d));
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
      runtime.checkList(shape);
      var s = runtime.ffi.toArray(shape);
      s.forEach((x) => { runtime.checkNumInteger(x); });
      var min = unwrapFixnumOption(minVal);
      var max = unwrapFixnumOption(maxVal)
      return buildTensorObject(tf.randomUniform(s, min, max));
    }

    /**
     * Creates a mutable PyretTensor from the input PyretTensor.
     * @param {PyretTensor} tensor A PyretTensor
     * @returns {PyretTensor} A mutable PyretTensor
     */
    function makeVariable(tensor) {
      arity(1, arguments, "make-variable", false);
      checkTensor(tensor);
      var newVariable = tf.variable(unwrapTensor(tensor));
      return buildTensorObject(newVariable);
    }

    /**
     * TensorBuffers
     */

    /**
     * Returns PyretTrue if the input `obj` is a P; otherwise,
     * returns PyretFalse.
     * @param {Any} obj Some Pyret value
     * @returns {PBoolean} A Pyret object representing true or false
     */
    function isTensorBuffer(obj) {
      arity(1, arguments, "is-tensor-buffer", false);
      return runtime.makeBoolean(hasBrand(brandTensorBuffer, obj));
    }

    // Constructor

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
      var obj = O({
        "_output": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "_output");
          var selfBuffer = unwrapTensorBuffer(self);
          var elts       = [];
          var bufferData = Array.from(selfBuffer.values);
          var vsValue    = get(VS, "vs-value");
          for (var i = 0; i < bufferData.length; i++) {
            var wrappedNum = runtime.num_to_roughnum(bufferData[i]);
            elts.push(vsValue.app(wrappedNum));
          }
          return get(VS, "vs-collection").app(
            runtime.makeString("tensor-buffer"),
            runtime.ffi.makeList(elts));
        }),
        "set-now": runtime.makeMethod2(function(self, value, locs) {
          checkMethodArity(3, arguments, "set-now");
          runtime.checkNumber(value);
          runtime.checkList(locs);
          var val       = runtime.num_to_fixnum(value);
          var locations = runtime.ffi.toArray(locs).map((x) => {
            runtime.checkNumber(x);
            return runtime.num_to_fixnum(x);
          });
          var selfBuffer = unwrapTensorBuffer(self);
          selfBuffer.set(val, ...locations);
          return runtime.makeNothing();
        }),
        "get-now": runtime.makeMethod1(function(self, locs) {
          checkMethodArity(2, arguments, "get-now");
          runtime.checkList(locs);
          var locations = runtime.ffi.toArray(locs).map((x) => {
            runtime.checkNumber(x);
            return runtime.num_to_fixnum(x);
          });
          var selfBuffer = unwrapTensorBuffer(self);
          var result     = selfBuffer.get(...locations);
          return runtime.makeNumber(result);
        }),
        "get-all-now": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "get-all-now");
          var selfBuffer = unwrapTensorBuffer(self);
          var bufferData = Array.from(selfBuffer.values);
          var roughnums  = bufferData.map((x) => { return runtime.num_to_roughnum(x); });
          return runtime.ffi.makeList(roughnums);
        }),
        "to-tensor": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-tensor");
          var selfBuffer = unwrapTensorBuffer(self);
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
      var s = runtime.ffi.toArray(shape);
      s.forEach((x) => { runtime.checkNumInteger(x); });
      return buildTensorBufferObject(tf.buffer(s));
    }

    /**
     * Operations (Arithmetic)
     */

    /**
     * Returns true if the PyretTensors `a` and `b` have the same shape;
     * otherwise, throws a Pyret runtime exception.
     *
     * This is not intended to be a Pyret function, but instead a helper JS
     * function for the "strict" operation methods.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {JSBoolean} Always returns true if no exception was thrown
     */
    function assertEqualShapes(a, b) {
      // Get the underlying array representation of Tensor shapes:
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      var aShape  = aTensor.shape;
      var bShape  = bTensor.shape;

      // Check if the shapes are the same length:
      if (aShape.length != bShape.length) { return false; }

      // Check element-wise equality:
      for (var i = 0; i < aShape.length; i++) {
        if (aShape[i] != bShape[i]) {
          runtime.ffi.throwMessageException("The first tensor does not have " +
          "the same shape as the second tensor");
        }
      }
      return true;
    }

    /**
     * Adds `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a + b
     */
    function addTensors(a, b) {
      arity(2, arguments, "add-tensors", false);
      checkTensor(a);
      checkTensor(b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.add(aTensor, bTensor));
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
      checkTensor(a);
      checkTensor(b);
      assertEqualShapes(a, b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.add(aTensor, bTensor));
    }

    /**
     * Subtracts `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a - b
     */
    function subtractTensors(a, b) {
      arity(2, arguments, "subtract-tensors", false);
      checkTensor(a);
      checkTensor(b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.sub(aTensor, bTensor));
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
      checkTensor(a);
      checkTensor(b);
      assertEqualShapes(a, b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.sub(aTensor, bTensor));
    }

    /**
     * Multiplies `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a * b
     */
    function multiplyTensors(a, b) {
      arity(2, arguments, "multiply-tensors", false);
      checkTensor(a);
      checkTensor(b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.mul(aTensor, bTensor));
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
      checkTensor(a);
      checkTensor(b);
      assertEqualShapes(a, b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.mul(aTensor, bTensor));
    }

    /**
     * Divides `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a / b
     */
    function divideTensors(a, b) {
      arity(2, arguments, "divide-tensors", false);
      checkTensor(a);
      checkTensor(b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.div(aTensor, bTensor));
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
      checkTensor(a);
      checkTensor(b);
      assertEqualShapes(a, b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.div(aTensor, bTensor));
    }

    /**
     * Divides `a` and `b`, element-wise, with the result rounded with the
     * floor function.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result of a / b
     */
    function floorDivideTensors(a, b) {
      arity(2, arguments, "floor-divide-tensors", false);
      checkTensor(a);
      checkTensor(b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.floorDiv(aTensor, bTensor));
    }

    /**
     * Returns the maximum of `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function maxTensor(a, b) {
      arity(2, arguments, "tensor-max", false);
      checkTensor(a);
      checkTensor(b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.maximum(aTensor, bTensor));
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
      checkTensor(a);
      checkTensor(b);
      assertEqualShapes(a, b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.maximum(aTensor, bTensor));
    }

    /**
     * Returns the minimum of `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function minTensor(a, b) {
      arity(2, arguments, "tensor-min", false);
      checkTensor(a);
      checkTensor(b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.minimum(aTensor, bTensor));
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
      checkTensor(a);
      checkTensor(b);
      assertEqualShapes(a, b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.minimum(aTensor, bTensor));
    }

    /**
     * Returns the modulo of `a` and `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function moduloTensor(a, b) {
      arity(2, arguments, "tensor-modulo", false);
      checkTensor(a);
      checkTensor(b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.mod(aTensor, bTensor));
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
      checkTensor(a);
      checkTensor(b);
      assertEqualShapes(a, b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.mod(aTensor, bTensor));
    }

    /**
     * Computes `a` raised to `b`, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function exptTensor(base, exp) {
      arity(2, arguments, "tensor-expt", false);
      checkTensor(base);
      checkTensor(exp);
      var baseTensor = unwrapTensor(base);
      var expTensor  = unwrapTensor(exp);
      return buildTensorObject(tf.pow(baseTensor, expTensor));
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
      checkTensor(a);
      checkTensor(b);
      assertEqualShapes(a, b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.pow(aTensor, bTensor));
    }

    /**
     * Computes (a - b) * (a - b), element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function tensorSquaredDifference(a, b) {
      arity(2, arguments, "squared-difference", false);
      checkTensor(a);
      checkTensor(b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.squaredDifference(aTensor, bTensor));
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
      checkTensor(a);
      checkTensor(b);
      assertEqualShapes(a, b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.squaredDifference(aTensor, bTensor));
    }

    /**
     * Operations (Basic Math)
     */

    /**
     * Computes the absolute value of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function abs(x) {
      arity(1, arguments, "tensor-abs", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.abs(tensor));
    }

    /**
     * Computes the inverse cosine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function acos(x) {
      arity(1, arguments, "tensor-acos", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.acos(tensor));
    }

    /**
     * Computes the inverse hyperbolic cosine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function acosh(x) {
      arity(1, arguments, "tensor-acosh", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.acosh(tensor));
    }

    /**
     * Computes the inverse sine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function asin(x) {
      arity(1, arguments, "tensor-asin", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.asin(tensor));
    }

    /**
     * Computes the inverse hyperbolic sine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function asinh(x) {
      arity(1, arguments, "tensor-asinh", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.asinh(tensor));
    }

    /**
     * Computes the inverse tangent of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function atan(x) {
      arity(1, arguments, "tensor-atan", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.atan(tensor));
    }

    /**
     * Computes the four-quadrant inverse tangent of a and b, element-wise.
     * @param {PyretTensor} a The first PyretTensor
     * @param {PyretTensor} b The second PyretTensor
     * @returns {PyretTensor} The result
     */
    function atan2(a, b) {
      arity(2, arguments, "tensor-atan2", false);
      checkTensor(a);
      checkTensor(b);
      var aTensor = unwrapTensor(a);
      var bTensor = unwrapTensor(b);
      return buildTensorObject(tf.atan2(aTensor, bTensor));
    }

    /**
     * Computes the inverse hyperbolic tangent of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function atanh(x) {
      arity(1, arguments, "tensor-atanh", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.atanh(tensor));
    }

    /**
     * Computes the ceiling of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function ceil(x) {
      arity(1, arguments, "tensor-ceil", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.atan(tensor));
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
      checkTensor(x);
      runtime.checkNumber(min);
      runtime.checkNumber(max);
      var tensor = unwrapTensor(x);
      var mi     = unwrap(min);
      var ma     = unwrap(max);
      return buildTensorObject(tf.clipByValue(tensor, mi, ma));
    }

    /**
     * Computes the cosine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function cos(x) {
      arity(1, arguments, "tensor-cos", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.cos(tensor));
    }

    /**
     * Computes the hyperbolic cosine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function cosh(x) {
      arity(1, arguments, "tensor-cosh", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.cosh(tensor));
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
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.elu(tensor));
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
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.erf(tensor));
    }

    /**
     * Computes the equivalent of num-exp(x), element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function exp(x) {
      arity(1, arguments, "tensor-exp", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.exp(tensor));
    }

    /**
     * Computes the equivalent of num-exp(x - 1), element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function expm1(x) {
      arity(1, arguments, "tensor-exp-min1", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.expm1(tensor));
    }

    /**
     * Computes the floor of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function floor(x) {
      arity(1, arguments, "tensor-floor", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.floor(tensor));
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
      checkTensor(x);
      runtime.checkNumber(alpha);
      var tensor = unwrapTensor(x);
      var a = unwrap(alpha);
      return buildTensorObject(tf.leakyRelu(tensor, a));
    }

    /**
     * Computes the log of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function log(x) {
      arity(1, arguments, "tensor-log", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.log(tensor));
    }

    /**
     * Computes the equivalent of num-log(x + 1), element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function log1p(x) {
      arity(1, arguments, "tensor-log-plus1", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.log1p(tensor));
    }

    /**
     * Applies the log sigmoid function to the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function logSigmoid(x) {
      arity(1, arguments, "log-sigmoid", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.logSigmoid(tensor));
    }

    /**
     * Multiplies each element in the Tensor by -1.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function neg(x) {
      arity(1, arguments, "tensor-negate", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.neg(tensor));
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
      checkTensor(x);
      runtime.checkNumber(alpha);
      var tensor = unwrapTensor(x);
      var a = runtime.num_to_fixnum(alpha);
      return buildTensorObject(tf.prelu(tensor, a));
    }

    /**
     * Computes the reciprocal of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function reciprocal(x) {
      arity(1, arguments, "tensor-reciprocal", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.reciprocal(tensor));
    }

    /**
     * Applies a rectified linear units function to the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function relu(x) {
      arity(1, arguments, "relu", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.relu(tensor));
    }

    /**
     * Computes the equivalent of num-round(x), element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function round(x) {
      arity(1, arguments, "tensor-round", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.round(tensor));
    }

    /**
     * Computes the recriprocal of the square root of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function rsqrt(x) {
      arity(1, arguments, "reciprocal-sqrt", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.rsqrt(tensor));
    }

    /**
     * Applies a scaled, exponential linear units function to the Tensor,
     * element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function selu(x) {
      arity(1, arguments, "scaled-elu", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.selu(tensor));
    }

    /**
     * Applies the sigmoid function to the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function sigmoid(x) {
      arity(1, arguments, "sigmoid", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.sigmoid(tensor));
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
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.sign(tensor));
    }

    /**
     * Computes the sine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function sin(x) {
      arity(1, arguments, "tensor-sin", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.sin(tensor));
    }

    /**
     * Computes the hyperbolic sine of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function sinh(x) {
      arity(1, arguments, "tensor-sinh", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.sinh(tensor));
    }

    /**
     * Applies the softplus function to the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function softplus(x) {
      arity(1, arguments, "softplus", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.softplus(tensor));
    }

    /**
     * Computes the square root of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function sqrt(x) {
      arity(1, arguments, "tensor-sqrt", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.sqrt(tensor));
    }

    /**
     * Computes the square of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function square(x) {
      arity(1, arguments, "tensor-square", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.square(tensor));
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
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.step(tensor));
    }

    /**
     * Computes the tangent of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function tan(x) {
      arity(1, arguments, "tensor-tan", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.tan(tensor));
    }

    /**
     * Computes the hyperbolic tangent of the Tensor, element-wise.
     * @param {PyretTensor} x
     * @returns {PyretTensor} The result
     */
    function tanh(x) {
      arity(1, arguments, "tensor-tanh", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.tanh(tensor));
    }

    /**
     * Operations (Reduction)
     */

    function all(x) {
      arity(1, arguments, "all", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.all(tensor));
    }

    function any(x) {
      arity(1, arguments, "any", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.any(tensor));
    }

    function argMax(x) {
      arity(1, arguments, "arg-max", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.argMax(tensor));
    }

    function argMin(x) {
      arity(1, arguments, "arg-min", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.argMin(tensor));
    }

    function logSumExp(x) {
      arity(1, arguments, "log-sum-exp", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      return buildTensorObject(tf.logSumExp(tensor));
    }

    function max(x, axis) {
      arity(2, arguments, "reduce-max", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      var a = unwrapFixnumOption(axis);
      return buildTensorObject(tf.max(tensor, a));
    }

    function mean(x, axis) {
      arity(1, arguments, "reduce-mean", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      var a = unwrapFixnumOption(axis);
      return buildTensorObject(tf.mean(tensor, a));
    }

    function min(x, axis) {
      arity(1, arguments, "reduce-min", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      var a = unwrapFixnumOption(axis);
      return buildTensorObject(tf.min(tensor, a));
    }

    function sum(x, axis) {
      arity(2, arguments, "reduce-sum", false);
      checkTensor(x);
      var tensor = unwrapTensor(x);
      var a = unwrapFixnumOption(axis);
      return buildTensorObject(tf.sum(tensor, a));
    }

    /**
     * Operations (Slicing and Joining)
     */

    // List<Tensor> Option<Number> -> Tensor
    function concatenate(tensors, axis) {
      arity(2, arguments, "concatenate", false);
      runtime.checkList(tensors);
      var ts = runtime.ffi.toArray(tensors).map((x) => {
        checkTensor(x);
        return unwrapTensor(x);
      });
      var a = unwrapFixnumOption(axis);
      return buildTensorObject(tf.concat(ts, a));
    }

    // Tensor Tensor Option<Number> -> Tensor
    function gather(tensor, indices, axis) {
      arity(3, arguments, "gather", false);
      checkTensor(x);
      checkTensor(indices);
      var t = unwrapTensor(tensor);
      var i = unwrapTensor(indices);
      if (i.shape.length !== 1) {
        runtime.ffi.throwMessageException("The `indices` argument to `gather` " +
          "must be a one-dimensional Tensor");
      }
      var a = unwrapFixnumOption(axis);
      return buildTensorObject(tf.gather(t, i, a));
    }

    // Tensor Option<List<Number>> -> Tensor
    function reverse(tensor, axes) {
      arity(2, arguments, "reverse", false);
      checkTensor(x);
      var t = unwrapTensor(tensor);
      var a = runtime.ffi.cases(runtime.ffi.isOption, "is-Option", axes, {
        some: (v) => {
          runtime.checkList(v);
          return runtime.ffi.toArray(v).map((x) => { return runtime.num_to_fixnum(x); });
        },
        none: () => { return undefined; }
      });
      return buildTensorObject(tf.reverse(t, a));
    }

    // Tensor List<Number> Option<List<Number>> -> Tensor
    function slice(tensor, begin, size) {
      arity(3, arguments, "slice", false);
      checkTensor(tensor);
      runtime.checkList(begin);
      var t = unwrapTensor(tensor);
      var b = runtime.ffi.toArray(begin).map((x) => { return runtime.num_to_fixnum(x); });
      var s = runtime.ffi.cases(runtime.ffi.isOption, "is-Option", size, {
        some: (v) => {
          runtime.checkList(v);
          return runtime.ffi.toArray(v).map((x) => { return runtime.num_to_fixnum(x); });
        },
        none: () => { return undefined; }
      });
      return buildTensorObject(tf.slice(t, b, s));
    }

    function split() {

    }

    function stack() {

    }

    function tile() {

    }

    function unstack() {

    }

    // PyretTensor List<Number> List<Number> List<Number> -> PyretTensor
    function stridedSlice(tensor, begin, end, strides) {
      arity(4, arguments, "strided-slice", false);
      checkTensor(tensor);
      runtime.checkList(begin);
      runtime.checkList(end);
      runtime.checkList(strides);
      var t = unwrapTensor(tensor);
      var b = runtime.ffi.toArray(begin).map((x) => { return runtime.num_to_fixnum(x); });
      var e = runtime.ffi.toArray(end).map((x) => { return runtime.num_to_fixnum(x); });
      var s = runtime.ffi.toArray(strides).map((x) => { return runtime.num_to_fixnum(x); });
      var result = tf.stridedSlice(t, b, e, s);
      return buildTensorObject(result);
    }

    /**
     * Models (Generic)
     */

    // Brand Checks

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

    // Constructor

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
      var obj = O({
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
      var c     = unwrapObject(config);
      var model = tf.model(c);
      return buildModelObject(model);
    }

    /**
     * Models (Sequential)
     */

    // Brand Checks

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

    // Constructor

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
      var obj = O({
        "add": runtime.makeMethod1(function(self, layer) {
          checkMethodArity(2, arguments, "add");
          checkLayer(layer);
          var selfSequential = unwrapSequential(self);
          var unwrappedLayer = unwrapLayer(layer);
          selfSequential.add(unwrappedLayer);
          return runtime.makeNothing();
        }),
        "compile": runtime.makeMethod1(function(self, config) {
          checkMethodArity(2, arguments, "compile");
          runtime.checkObject(config);
          var c = unwrapObject(config);
          // If there's an Optimizer defined, we have to unwrap it since
          // Tensorflow.js doesn't recognize PyretOptimizers:
          if ("optimizer" in c) {
            // But it could also be a string key that maps to a predefined
            // TF.js optimizer:
            if (typeof c["optimizer"] !== "string") {
              c["optimizer"] = c["optimizer"].$underlyingOptimizer;
            }
          }
          var selfSequential = unwrapSequential(self);
          selfSequential.compile(c);
          return runtime.makeNothing();
        }),
        "evaluate": runtime.makeMethod3(function(self, x, y, config) {
          checkMethodArity(4, arguments, "evaluate");
          checkTensor(x);
          checkTensor(y);
          runtime.checkObject(config);
          var selfSequential = unwrapSequential(self);
          var xTensor        = unwrapTensor(x);
          var yTensor        = unwrapTensor(y);
          var c              = unwrapObject(config);
          var result         = selfSequential.evaluate(xTensor, yTensor, c);
          return buildTensorObject(result);
        }),
        "predict": runtime.makeMethod2(function(self, x, config) {
          checkMethodArity(3, arguments, "predict");
          checkTensor(x);
          runtime.checkObject(config);
          var selfSequential = unwrapSequential(self);
          var xTensor        = unwrapTensor(x);
          var c              = unwrapObject(config);
          var result         = selfSequential.predict(xTensor, c);
          return buildTensorObject(result);
        }),
        "predict-on-batch": runtime.makeMethod1(function(self, x) {
          checkMethodArity(2, arguments, "predict-on-batch");
          checkTensor(x);
          var selfSequential = unwrapSequential(self);
          var xTensor        = unwrapTensor(x);
          var result         = selfSequential.predictOnBatch(xTensor, c);
          return buildTensorObject(result);
        }),
        "fit": runtime.makeMethod3(function(self, x, y, config, callback) {
          checkMethodArity(5, arguments, "fit");
          checkTensor(x);
          checkTensor(y);
          runtime.checkObject(config);
          var xTensor = unwrapTensor(x);
          var yTensor = unwrapTensor(y);
          var c       = unwrapObject(config);
          // TODO(Zachary): Generalize across multiple callback types
          c.callbacks = {onEpochEnd: async (epoch, log) => {
            runtime.safeCall(() => {
              callback.app(runtime.makeNumber(epoch), runtime.makeObject(log));
            }, (_) => {}); // handler purposely blank
          }};
          var selfSequential = unwrapSequential(self);
          selfSequential.fit(xTensor, yTensor, c);
          return runtime.makeNothing();
        })
      });
      obj = applyBrand(brandSequential, obj);
      obj.$underlyingSequential = underlyingSequential;
      return obj;
    }

    function makeSequential(config) {
      arity(1, arguments, "make-sequential", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      var model = tf.sequential(c);
      return buildSequentialObject(model);
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
      var obj = O({
        "_output": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "_output");
          return get(VS, "vs-str").app("<symbolic-tensor>");
        }),
        "shape": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "shape");
          var selfSymbolic = unwrapSymbolicTensor(self);
          var optionValues = selfSymbolic.shape.map((x) => {
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
      runtime.checkList(shape);
      var array = runtime.ffi.toArray(shape);
      var s = array.map((x) => {
        var num = unwrapFixnumOption(x);
        if (num === undefined) {
          num = null;
        }
        return num;
      });
      var input = tf.input({ shape: s });
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
      runtime.checkList(shape);
      var array = runtime.ffi.toArray(shape);
      var s = array.map((x) => {
        var num = unwrapFixnumOption(x);
        if (num === undefined) {
          num = null;
        }
        return num;
      });
      var input = tf.input({ batchShape: s });
      return buildSymbolicTensorObject(input);
    }

    /**
     * Dense Layer
     */

    // Brand Checks

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

    // Constructor

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
     * Consumes a TFLayer and wraps it in a PyretObject to make it a
     * PyretLayer.
     * @param {TFLayer} underlyingLayer A TensorFlow.js Layer
     * @returns {PyretLayer} A new PyretLayer with the input as its
     *  underlying TFLayer
     */
    function buildLayerObject(underlyingLayer) {
      var obj = O({
        "_output": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "_output");
          return get(VS, "vs-str").app("<layer>");
        }),
        "apply-tensors": runtime.makeMethod1(function(self, tensors) {
          checkMethodArity(2, arguments, "apply-tensors");
          runtime.checkList(tensors);
          var inputs = runtime.ffi.toArray(tensors).map((x) => {
            checkTensor(x);
            return unwrapTensor(x);
          });
          var selfLayer = unwrapLayer(self);
          var outputs = selfLayer.apply(inputs).map((x) => {
            buildTensorObject(x);
          });
          return runtime.ffi.makeList(outputs);
        }),
        "apply-symbolic-tensors": runtime.makeMethod1(function(self, symbolics) {
          checkMethodArity(2, arguments, "apply-symbolic-tensors");
          runtime.checkList(symbolics);
          var inputs = runtime.ffi.toArray(symbolics).map((x) => {
            checkTensor(x);
            return unwrapSymbolicTensor(x);
          });
          var selfLayer = unwrapLayer(self);
          var outputs = selfLayer.apply(inputs).map((x) => {
            return buildTensorObject(x);
          });
          return runtime.ffi.makeList(outputs);
        })
      });
      obj = applyBrand(brandLayer, obj);
      obj.$underlyingLayer = underlyingLayer;
      return obj;
    }

    function makeActivationLayer(config) {
      arity(1, arguments, "activation-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.activation(c));
    }

    function makeDenseLayer(config) {
      arity(1, arguments, "dense-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.dense(c));
    }

    function makeDropoutLayer(config) {
      arity(1, arguments, "dropout-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.dropout(c));
    }

    function makeEmbeddingLayer(config) {
      arity(1, arguments, "embedding-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.embedding(c));
    }

    function makeFlattenLayer(config) {
      arity(1, arguments, "flatten-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.flatten(c));
    }

    function makeRepeatVectorLayer(config) {
      arity(1, arguments, "repeat-vector-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.repeatVector(c));
    }

    function makeReshapeLayer(config) {
      arity(1, arguments, "reshape-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.reshape(c));
    }

    function makeConv1dLayer(config) {
      arity(1, arguments, "conv-1d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.conv1d(c));
    }

    function makeConv2dLayer(config) {
      arity(1, arguments, "conv-2d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.conv2d(c));
    }

    function makeConv2dTransposeLayer(config) {
      arity(1, arguments, "conv-2d-transpose-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.conv2dTranspose(c));
    }

    function makeCropping2dLayer(config) {
      arity(1, arguments, "cropping-2d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.cropping2D(c));
    }

    function makeDepthwiseConv2dLayer(config) {
      arity(1, arguments, "depthwise-conv-2d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.depthwiseConv2d(c));
    }

    function makeSeparableConv2dLayer(config) {
      arity(1, arguments, "separable-conv-2d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.separableConv2d(c));
    }

    function makeUpSampling2dLayer(config) {
      arity(1, arguments, "up-sampling-2d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.upSampling2d(c));
    }

    function makeAddLayer(config) {
      arity(1, arguments, "add-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.add(c));
    }

    function makeAverageLayer(config) {
      arity(1, arguments, "average-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.average(c));
    }

    function makeConcatenateLayer(config) {
      arity(1, arguments, "concatenate-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.concatenate(c));
    }

    function makeMaximumLayer(config) {
      arity(1, arguments, "maximum-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.maximum(c));
    }

    function makeMinimumLayer(config) {
      arity(1, arguments, "minimum-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.minimum(c));
    }

    function makeMultiplyLayer(config) {
      arity(1, arguments, "multiply-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.multiply(c));
    }

    function makeBatchNormalizationLayer(config) {
      arity(1, arguments, "batch-normalization-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.batchNormalization(c));
    }

    function makeAveragePooling1dLayer(config) {
      arity(1, arguments, "average-pooling-1d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.averagePooling1d(c));
    }

    function makeAveragePooling2dLayer(config) {
      arity(1, arguments, "average-pooling-2d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.averagePooling2d(c));
    }

    function makeGlobalAveragePooling1dLayer(config) {
      arity(1, arguments, "global-average-pooling-1d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.globalAveragePooling1d(c));
    }

    function makeGlobalAveragePooling2dLayer(config) {
      arity(1, arguments, "global-average-pooling-2d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.globalAveragePooling2d(c));
    }

    function makeGlobalMaxPooling1dLayer(config) {
      arity(1, arguments, "global-max-pooling-1d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.globalMaxPooling1d(c));
    }

    function makeGlobalMaxPooling2dLayer(config) {
      arity(1, arguments, "global-max-pooling-2d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.globalMaxPooling2d(c));
    }

    function makeMaxPooling1dLayer(config) {
      arity(1, arguments, "max-pooling-1d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.maxPooling1d(c));
    }

    function makeMaxPooling2dLayer(config) {
      arity(1, arguments, "max-pooling-2d-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.maxPooling2d(c));
    }

    function makeGruLayer(config) {
      arity(1, arguments, "gru-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.gru(c));
    }

    function makeGruCellLayer(config) {
      arity(1, arguments, "gru-cell-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.gruCell(c));
    }

    function makeLstmLayer(config) {
      arity(1, arguments, "lstm-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      // If there's an Layer defined, we have to unwrap it since
      // Tensorflow.js doesn't recognize PyretLayers:
      if ("batchInputShape" in c) {
        runtime.checkList(c["batchInputShape"]);
        var batchInputs = runtime.ffi.toArray(c["batchInputShape"]);
        var unwrapped = batchInputs.map((input) => {
          return runtime.ffi.cases(runtime.ffi.isOption, "is-Option", input, {
            some: (v) => { return runtime.num_to_fixnum(v); },
            none: () => { return null; }
          });
        })
        c["batchInputShape"] = unwrapped;
      }
      return buildLayerObject(tf.layers.lstm(c));
    }

    function makeLstmCellLayer(config) {
      arity(1, arguments, "lstm-cell-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.lstmCell(c));
    }

    function makeRNNLayer(config) {
      arity(1, arguments, "rnn-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.rnn(c));
    }

    function makeSimpleRNNLayer(config) {
      arity(1, arguments, "simple-rnn-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.simpleRNN(c));
    }

    function makeSimpleRNNCellLayer(config) {
      arity(1, arguments, "simple-rnn-cell-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.simpleRNNCell(c));
    }

    function makeStackedRNNCellsLayer(config) {
      arity(1, arguments, "stacked-rnn-cells-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.stackedRNNCells(c));
    }

    function makeBidirectionalLayer(config) {
      arity(1, arguments, "bidirectional-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      return buildLayerObject(tf.layers.bidirectional(c));
    }

    function makeTimeDistributedLayer(config) {
      arity(1, arguments, "time-distributed-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      // If there's an Layer defined, we have to unwrap it since
      // Tensorflow.js doesn't recognize PyretLayers:
      if ("layer" in c) {
        c["layer"] = c["layer"].$underlyingLayer;
      }
      return buildLayerObject(tf.layers.timeDistributed(c));
    }

    /**
     * Training (Optimizers)
     */

    // Brand Checks

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

    // Constructors

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
      var obj = O({
        "_output": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "_output");
          return get(VS, "vs-str").app("<optimizer>");
        }),
        "minimize": runtime.makeMethod2(function(self, functionToMinimize, varList) {
          checkMethodArity(3, arguments, "minimize");
          // varList is a list of mutable tensors for the Optimizer to edit. If
          // it is empty, it should be set to `undefined` so TensorFlow.js
          // knows to modify all available mutable tensors in the space:
          runtime.checkList(varList);
          var variables = runtime.ffi.toArray(varList).map((v) => { return unwrapTensor(v); });
          if (variables.length === 0) { variables = undefined; }

          // Run minimization thunk. The thunk should return a scalar Tensor.
          // TODO(Zachary): We don't have good checks for scalars yet, but for
          // now just check that the return type of the function is a scalar:
          var selfOptimizer = unwrapOptimizer(self);
          var result = selfOptimizer.minimize(() => {
            return runtime.safeCall(() => {
              return functionToMinimize.app();
            }, (scalar) => {
              checkTensor(scalar);
              return unwrapTensor(scalar);
            });
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
      var rate = runtime.num_to_fixnum(learningRate);
      return buildOptimizerObject(tf.train.sgd(rate));
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
      var rate = runtime.num_to_fixnum(learningRate);
      var moment = runtime.num_to_fixnum(momentum);
      return buildOptimizerObject(tf.train.momentum(rate, moment));
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
      var rate = runtime.num_to_fixnum(learningRate);
      var initial = unwrapFixnumOption(initialAccumulatorValue);
      if ((initial !== null) && (initial <= 0)) {
        runtime.ffi.throwMessageException("The initial accumulator value " +
          "passed to `train-adagrad` must be positive");
      }
      return buildOptimizerObject(tf.train.adagrad(rate, initial));
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
      var l = unwrapFixnumOption(learningRate);
      var r = unwrapFixnumOption(rho);
      var e = unwrapFixnumOption(epsilon);
      return buildOptimizerObject(tf.train.adadelta(l, r, e));
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
      var l = unwrapFixnumOption(learningRate);
      var b1 = unwrapFixnumOption(beta1);
      var b2 = unwrapFixnumOption(beta2);
      var e = unwrapFixnumOption(epsilon);
      return buildOptimizerObject(tf.train.adam(l, b1, b2, e));
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
      var l = unwrapFixnumOption(learningRate);
      var b1 = unwrapFixnumOption(beta1);
      var b2 = unwrapFixnumOption(beta2);
      var e = unwrapFixnumOption(epsilon);
      var d = unwrapFixnumOption(decay);
      return buildOptimizerObject(tf.train.adam(l, b1, b2, e, d));
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
      var rate = runtime.num_to_fixnum(learningRate);
      var d = unwrapFixnumOption(decay);
      var m = unwrapFixnumOption(momentum);
      var e = unwrapFixnumOption(epsilon);
      var c = runtime.isPyretTrue(centered);
      return buildOptimizerObject(tf.train.rmsprop(rate, d, m, e, c));
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
      "softplus": F(softplus, "softplus"),
      "tensor-sqrt": F(sqrt, "tensor-sqrt"),
      "tensor-square": F(square, "tensor-square"),
      "step": F(step, "step"),
      "tensor-tan": F(tan, "tensor-tan"),
      "tensor-tanh": F(tanh, "tensor-tanh"),

      // Operations (Reduction)
      "all": F(all, "all"),
      "any": F(any, "any"),
      "arg-max": F(argMax, "arg-max"),
      "arg-min": F(argMin, "arg-min"),
      "log-sum-exp": F(logSumExp, "log-sum-exp"),
      "reduce-max": F(max, "reduce-max"),
      "reduce-mean": F(mean, "reduce-mean"),
      "reduce-min": F(min, "reduce-min"),
      "reduce-sum": F(sum, "reduce-sum"),

      // Operations (Slicing and Joining)
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
