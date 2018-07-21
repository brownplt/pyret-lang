({
  requires: [
    { "import-type": "builtin", name: "tensorflow-structs" }
  ],
  nativeRequires: ["@tensorflow/tfjs"],
  provides: {
    values: {
      "tensor": ["forall", ["a"], ["Maker", "Any", ["tyapp", ["local", "Tensor"], [["tid", "a"]]]]],
    },
    datatype: {
      "Tensor": ["data", "Tensor", ["a"], [], {}],
      "Model": ["data", "Model", ["a"], [], {}],
      "Sequential": ["data", "Sequential", ["a"], [], {}],
      "Layer": ["data", "Layer", ["a"], [], {}],
      "Optimizer": ["data", "Optimizer", ["a"], [], {}],
    }
  },
  theModule: function(runtime, namespace, uri, tfStructs, tf) {

    /**
     * Tensorflow Brands and Annotations
     */
    var brandTensor = runtime.namedBrander("tensor", ["tensor: tensor brander"]);
    var annTensor = runtime.makeBranderAnn(brandTensor, "Tensor");

    var brandTensorBuffer = runtime.namedBrander("tensor-buffer", ["tensor-buffer: tensor-buffer brander"]);
    var annTensorBuffer = runtime.makeBranderAnn(brandTensorBuffer, "TensorBuffer");

    var brandModel = runtime.namedBrander("model", ["model: model brander"]);
    var annModel = runtime.makeBranderAnn(brandModel, "Model");

    var brandSequential = runtime.namedBrander("sequential", ["sequential: sequential brander"]);
    var annSequential = runtime.makeBranderAnn(brandSequential, "Sequential");

    var brandSymbolicTensor = runtime.namedBrander("symbolic-tensor", ["symbolic-tensor: symbolic-tensor brander"]);
    var annSymbolicTensor = runtime.makeBranderAnn(brandSymbolicTensor, "SymbolicTensor");

    var brandLayer = runtime.namedBrander("layer", ["layer: layer brander"]);
    var annLayer = runtime.makeBranderAnn(brandLayer, "Layer");

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

    var sVals = get(tfStructs, "values");
    var sTyps = get(tfStructs, "types");

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
     * @returns {PyretBoolean} A Pyret object representing true or false
     */
    function isTensor(obj) {
      arity(1, arguments, "is-tensor", false);
      return runtime.makeBoolean(hasBrand(brandTensor, obj));
    }

    // Constructor

    function buildTensorObject(underlyingTensor) {
      var obj = O({
        "size": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "size");
          return runtime.makeNumber(self.$underlyingTensor.size);
        }),
        "shape": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "shape");
          return runtime.ffi.makeList(self.$underlyingTensor.shape);
        }),
        "flatten": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "flatten");
          return buildTensorObject(self.$underlyingTensor.flatten());
        }),
        "as-scalar": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "as-scalar");
          if (self.$underlyingTensor.size !== 1) {
            runtime.ffi.throwMessageException("Tensor was size-" +
              self.$underlyingTensor.size + " but `as-scalar` requires the " +
              "tensor to be size-1");
          }
          return buildTensorObject(self.$underlyingTensor.asScalar());
        }),
        "as-1d": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "as-1d");
          return buildTensorObject(self.$underlyingTensor.as1D());
        }),
        "as-2d": runtime.makeMethod2(function(self, rows, columns) {
          checkMethodArity(3, arguments, "as-2d");
          runtime.checkNumInteger(rows);
          runtime.checkNumInteger(columns);
          var r = runtime.num_to_fixnum(rows);
          var c = runtime.num_to_fixnum(columns);

          if (self.$underlyingTensor.size !== (r * c)) {
            runtime.ffi.throwMessageException("Cannot reshape because the number " +
              "of entry spaces in the new shape must be equal to the number of " +
              "existing entries");
          }
          return buildTensorObject(self.$underlyingTensor.as2D(r, c));
        }),
        "as-3d": runtime.makeMethod3(function(self, rows, columns, depth) {
          checkMethodArity(4, arguments, "as-3d");
          runtime.checkNumInteger(rows);
          runtime.checkNumInteger(columns);
          runtime.checkNumInteger(depth);
          var r = runtime.num_to_fixnum(rows);
          var c = runtime.num_to_fixnum(columns);
          var d = runtime.num_to_fixnum(depth);

          if (self.$underlyingTensor.size !== (r * c * d)) {
            runtime.ffi.throwMessageException("Cannot reshape because the number " +
              "of entry spaces in the new shape must be equal to the number of " +
              "existing entries");
          }
          return buildTensorObject(self.$underlyingTensor.as3D(r, c, d));
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

          if (self.$underlyingTensor.size !== (r * c * d1 * d2)) {
            runtime.ffi.throwMessageException("Cannot reshape because the number " +
              "of entry spaces in the new shape must be equal to the number of " +
              "existing entries");
          }
          return buildTensorObject(self.$underlyingTensor.as4D(r, c, d1, d2));
        }),
        "as-type": runtime.makeMethod1(function(self, datatype) {
          checkMethodArity(2, arguments, "as-type");
          runtime.checkString(datatype);
          var type = unwrap(datatype);
          if (type !== "float32" || type !== "int32" || type !== "bool") {
            runtime.ffi.throwMessageException("Attempted to cast tensor to " +
              "invalid type (" + type + "); valid types are 'float32', 'int32', " +
              "or 'bool'");
          }
          return buildTensorObject(self.$underlyingTensor.asType(type));
        }),
        "to-buffer": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-buffer");
          var newBuffer = self.$underlyingTensor.buffer()
          return buildTensorBufferObject(newBuffer);
        }),
        "data-sync": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "data-sync");
          // .dataSync returns a TypedArray, so convert it to a normal JSArray
          // so we can then convert it to a Pyret List:
          var typedArrayData = self.$underlyingTensor.dataSync();
          var arrayData = Array.from(typedArrayData);
          // Convert to Roughnums, since the numbers returned from a Tensor are
          // floating point:
          arrayData = arrayData.map((x) => { return runtime.num_to_roughnum(x); });
          return runtime.ffi.makeList(arrayData);
        }),
        "to-float": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-float");
          return buildTensorObject(self.$underlyingTensor.toFloat());
        }),
        "to-int": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-int");
          return buildTensorObject(self.$underlyingTensor.toInt());
        }),
        "to-bool": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-bool");
          return buildTensorObject(self.$underlyingTensor.toBool());
        }),
        "to-variable": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-variable");
          return makeVariable(self);
        }),
        "reshape": runtime.makeMethod0(function(self, newShape) {
          checkMethodArity(2, arguments, "reshape");
          runtime.checkList(newShape);
          var ns = runtime.toArray(newShape);
          var product = ns.reduce((a, b) => { return a * b; }, 1);
          if (self.$underlyingTensor.size !== product) {
            runtime.ffi.throwMessageException("Cannot reshape because the number " +
              "of entry spaces in the new shape must be equal to the number of " +
              "existing entries");
          }
          return buildTensorObject(self.$underlyingTensor.reshape(ns));
        }),
        "expand-dims": runtime.makeMethod1(function(self, axis) {
          checkMethodArity(2, arguments, "expand-dims");
          var a = unwrapFixnumOption(axis);
          return buildTensorObject(self.$underlyingTensor.expandDims(a));
        }),
        "squeeze": runtime.makeMethod1(function(self, axes) {
          checkMethodArity(2, arguments, "squeeze");
          var a = runtime.ffi.cases(runtime.ffi.isOption, "is-Option", axes, {
            some: (v) => {
              runtime.checkList(v);
              return runtime.ffi.toArray(v).map((x) => { return runtime.num_to_fixnum(x); });
            },
            none: () => { return undefined; }
          });
          return buildTensorObject(self.$underlyingTensor.squeeze(a));
        }),
        "clone": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "clone");
          return buildTensorObject(self.$underlyingTensor.clone());
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

    function createTensor(values) {
      var underlyingTensor = tf.tensor(values, null, null);
      return buildTensorObject(underlyingTensor);
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
      return createTensor(fixnums);
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

    function multinomial(logits, numSamples, randomSeed, normalized) {
      arity(4, arguments, "multinomial", false);
      checkTensor(logits);
      runtime.checkNumber(numSamples);
      runtime.checkBoolean(normalized);
      var tensor  = logits.$underlyingTensor;
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
      var newVariable = tf.variable(tensor.$underlyingTensor);
      return buildTensorObject(newVariable);
    }

    /**
     * TensorBuffers
     */

    function isTensorBuffer(obj) {
      arity(1, arguments, "is-tensor-buffer", false);
      return runtime.makeBoolean(hasBrand(brandTensorBuffer, obj));
    }

    // Constructor

    function buildTensorBufferObject(underlyingBuffer) {
      var obj = O({
        "set": runtime.makeMethod2(function(self, value, locs) {
          checkMethodArity(3, arguments, "set");
          runtime.checkNumber(value);
          runtime.checkList(locs);
          var val = runtime.num_to_fixnum(value);
          var locations = runtime.ffi.toArray(locs).map((x) => {
            runtime.checkNumber(x);
            return runtime.num_to_fixnum(x);
          });
          self.$underlyingBuffer.set(val, ...locations);
          return runtime.makeNothing();
        }),
        "get": runtime.makeMethod1(function(self, locs) {
          checkMethodArity(2, arguments, "get");
          runtime.checkList(locs);
          var locations = runtime.ffi.toArray(locs).map((x) => {
            runtime.checkNumber(x);
            return runtime.num_to_fixnum(x);
          });
          var result = self.$underlyingBuffer.get(...locations);
          return runtime.makeNumber(result);
        }),
        "to-tensor": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-tensor");
          return buildTensorObject(self.$underlyingBuffer.toTensor());
        })
      });
      obj = applyBrand(brandTensorBuffer, obj);
      obj.$underlyingBuffer = underlyingBuffer;
      return obj;
    }

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
      var aShape = a.$underlyingTensor.shape;
      var bShape = b.$underlyingTensor.shape;

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
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
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
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
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
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
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
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
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
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
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
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
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
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
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
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
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
      var baseTensor = base.$underlyingTensor;
      var expTensor = exp.$underlyingTensor;
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
      return buildTensorObject(tf.pow(aTensor, bTensor));
    }

    function tensorSquaredDifference(a, b) {
      arity(2, arguments, "squared-difference", false);
      checkTensor(a);
      checkTensor(b);
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
      return buildTensorObject(tf.squaredDifference(aTensor, bTensor));
    }

    function strictSquaredDifference(a, b) {
      arity(2, arguments, "strict-squared-difference", false);
      checkTensor(a);
      checkTensor(b);
      assertEqualShapes(a, b);
      return buildTensorObject(tf.squaredDifference(aTensor, bTensor));
    }

    /**
     * Operations (Basic Math)
     */

    function abs(x) {
      arity(1, arguments, "tensor-abs", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.abs(tensor));
    }

    function acos(x) {
      arity(1, arguments, "tensor-acos", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.acos(tensor));
    }

    function acosh(x) {
      arity(1, arguments, "tensor-acosh", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.acosh(tensor));
    }

    function asin(x) {
      arity(1, arguments, "tensor-asin", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.asin(tensor));
    }

    function asinh(x) {
      arity(1, arguments, "tensor-asinh", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.asinh(tensor));
    }

    function atan(x) {
      arity(1, arguments, "tensor-atan", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.atan(tensor));
    }

    function atan2(a, b) {
      arity(2, arguments, "tensor-atan2", false);
      checkTensor(a);
      checkTensor(b);
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
      return buildTensorObject(tf.atan2(aTensor, bTensor));
    }

    function atanh(x) {
      arity(1, arguments, "tensor-atanh", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.atanh(tensor));
    }

    function ceil(x) {
      arity(1, arguments, "tensor-ceil", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.atan(tensor));
    }

    function clipByValue(x, min, max) {
      arity(3, arguments, "clip-by-value", false);
      checkTensor(x);
      runtime.checkNumber(min);
      runtime.checkNumber(max);
      var tensor = x.$underlyingTensor;
      var mi = unwrap(min);
      var ma = unwrap(max);
      return buildTensorObject(tf.clipByValue(tensor, mi, ma));
    }

    function cos(x) {
      arity(1, arguments, "tensor-cos", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.cos(tensor));
    }

    function cosh(x) {
      arity(1, arguments, "tensor-cosh", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.cosh(tensor));
    }

    function elu(x) {
      arity(1, arguments, "elu", false);
      return exponentialLinearUnits(x);
    }

    function exponentialLinearUnits(x) {
      arity(1, arguments, "exponential-linear-units", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.elu(tensor));
    }

    function erf(x) {
      arity(1, arguments, "erf", false);
      return gaussError(x);
    }

    function gaussError(x) {
      arity(1, arguments, "gauss-error", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.erf(tensor));
    }

    function exp(x) {
      arity(1, arguments, "tensor-exp", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.exp(tensor));
    }

    function expm1(x) {
      arity(1, arguments, "tensor-exp-min1", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.expm1(tensor));
    }

    function floor(x) {
      arity(1, arguments, "tensor-floor", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.floor(tensor));
    }

    function leakyRelu(x, alpha) {
      arity(2, arguments, "leaky-relu", false);
      checkTensor(x);
      runtime.checkNumber(alpha);
      var tensor = x.$underlyingTensor;
      var a = unwrap(alpha);
      return buildTensorObject(tf.leakyRelu(tensor, a));
    }

    function log(x) {
      arity(1, arguments, "tensor-log", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.log(tensor));
    }

    function log1p(x) {
      arity(1, arguments, "tensor-log-plus1", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.log1p(tensor));
    }

    function logSigmoid(x) {
      arity(1, arguments, "log-sigmoid", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.logSigmoid(tensor));
    }

    function neg(x) {
      arity(1, arguments, "tensor-negate", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.neg(tensor));
    }

    function prelu(x, alpha) {
      arity(2, arguments, "parametric-relu", false);
      checkTensor(x);
      runtime.checkNumber(alpha);
      var tensor = x.$underlyingTensor;
      var a = runtime.num_to_fixnum(alpha);
      return buildTensorObject(tf.prelu(tensor, a));
    }

    function reciprocal(x) {
      arity(1, arguments, "tensor-reciprocal", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.reciprocal(tensor));
    }

    function relu(x) {
      arity(1, arguments, "relu", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.relu(tensor));
    }

    function round(x) {
      arity(1, arguments, "tensor-round", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.round(tensor));
    }

    function rsqrt(x) {
      arity(1, arguments, "reciprocal-sqrt", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.rsqrt(tensor));
    }

    function selu(x) {
      arity(1, arguments, "scaled-elu", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.selu(tensor));
    }

    function sigmoid(x) {
      arity(1, arguments, "sigmoid", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.sigmoid(tensor));
    }

    function sign(x) {
      arity(1, arguments, "signed-ones", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.sign(tensor));
    }

    function sin(x) {
      arity(1, arguments, "tensor-sin", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.sin(tensor));
    }

    function sinh(x) {
      arity(1, arguments, "tensor-sinh", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.sinh(tensor));
    }

    function softplus(x) {
      arity(1, arguments, "softplus", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.softplus(tensor));
    }

    function sqrt(x) {
      arity(1, arguments, "tensor-sqrt", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.sqrt(tensor));
    }

    function square(x) {
      arity(1, arguments, "tensor-square", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.square(tensor));
    }

    function step(x) {
      arity(1, arguments, "step", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.step(tensor));
    }

    function tan(x) {
      arity(1, arguments, "tensor-tan", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.tan(tensor));
    }

    function tanh(x) {
      arity(1, arguments, "tensor-tanh", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.tanh(tensor));
    }

    /**
     * Operations (Reduction)
     */

    function all(x) {
      arity(1, arguments, "all", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.all(tensor));
    }

    function any(x) {
      arity(1, arguments, "any", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.any(tensor));
    }

    function argMax(x) {
      arity(1, arguments, "arg-max", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.argMax(tensor));
    }

    function argMin(x) {
      arity(1, arguments, "arg-min", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.argMin(tensor));
    }

    function logSumExp(x) {
      arity(1, arguments, "log-sum-exp", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.logSumExp(tensor));
    }

    function max(x, axis) {
      arity(2, arguments, "reduce-max", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      var a = unwrapFixnumOption(axis);
      return buildTensorObject(tf.max(tensor, a));
    }

    function mean(x, axis) {
      arity(1, arguments, "reduce-mean", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      var a = unwrapFixnumOption(axis);
      return buildTensorObject(tf.mean(tensor, a));
    }

    function min(x, axis) {
      arity(1, arguments, "reduce-min", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
      var a = unwrapFixnumOption(axis);
      return buildTensorObject(tf.min(tensor, a));
    }

    function sum(x, axis) {
      arity(2, arguments, "reduce-sum", false);
      checkTensor(x);
      var tensor = x.$underlyingTensor;
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
        return x.$underlyingTensor;
      });
      var a = unwrapFixnumOption(axis);
      return buildTensorObject(tf.concat(ts, a));
    }

    // Tensor Tensor Option<Number> -> Tensor
    function gather(tensor, indices, axis) {
      arity(3, arguments, "gather", false);
      checkTensor(x);
      checkTensor(indices);
      var t = tensor.$underlyingTensor;
      var i = tensor.$underlyingTensor;
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
      var t = tensor.$underlyingTensor;
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
      var t = tensor.$underlyingTensor;
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
      var t = tensor.$underlyingTensor;
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

    function isModel(obj) {
      arity(1, arguments, "is-model", false);
      return runtime.makeBoolean(hasBrand(brandModel, obj));
    }

    // Constructor

    function buildModelObject(underlyingModel) {
      var obj = O({
      });
      obj = applyBrand(brandModel, obj);
      obj.$underlyingModel = underlyingModel;
      return obj;
    }

    function makeModel(config) {
      arity(1, arguments, "make-model", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      var model = tf.model(c);
      return buildModelObject(model);
    }

    /**
     * Models (Sequential)
     */

    // Brand Checks

    function isSequential(obj) {
      arity(1, arguments, "is-sequential", false);
      return runtime.makeBoolean(hasBrand(brandSequential, obj));
    }

    // Constructor

    function buildSequentialObject(underlyingSequential) {
      var obj = O({
        "add": runtime.makeMethod1(function(self, layer) {
          checkMethodArity(2, arguments, "add");
          if (!hasBrand(brandLayer, layer)) {
            runtime.ffi.throwTypeMismatch(layer, "Layer");
          }
          self.$underlyingSequential.add(layer.$underlyingLayer);
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
          self.$underlyingSequential.compile(c);
          return runtime.makeNothing();
        }),
        "evaluate": runtime.makeMethod3(function(self, x, y, config) {
          checkMethodArity(4, arguments, "evaluate");
          checkTensor(x);
          checkTensor(y);
          runtime.checkObject(config);
          var xTensor = x.$underlyingTensor;
          var yTensor = y.$underlyingTensor;
          var c = unwrapObject(config);
          var result = self.$underlyingSequential.evaluate(xTensor, yTensor, c);
          return buildTensorObject(result);
        }),
        "predict": runtime.makeMethod2(function(self, x, config) {
          checkMethodArity(3, arguments, "predict");
          checkTensor(x);
          runtime.checkObject(config);
          var xTensor = x.$underlyingTensor;
          var c = unwrapObject(config);
          var result = self.$underlyingSequential.predict(xTensor, c);
          return buildTensorObject(result);
        }),
        "predict-on-batch": runtime.makeMethod1(function(self, x) {
          checkMethodArity(2, arguments, "predict-on-batch");
          checkTensor(x);
          var xTensor = x.$underlyingTensor;
          var result = self.$underlyingSequential.predictOnBatch(xTensor, c);
          return buildTensorObject(result);
        }),
        "fit": runtime.makeMethod3(function(self, x, y, config, callback) {
          checkMethodArity(5, arguments, "fit");
          checkTensor(x);
          checkTensor(y);
          runtime.checkObject(config);
          var xTensor = x.$underlyingTensor;
          var yTensor = y.$underlyingTensor;
          var c = unwrapObject(config);
          c.callbacks = {onEpochEnd: async (epoch, log) => {
            runtime.safeCall(() => {
              callback.app(runtime.makeNumber(epoch), runtime.makeObject(log));
            }, (_) => {}); // handler purposely blank
          }};
          self.$underlyingSequential.fit(xTensor, yTensor, c);
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

    function buildSymbolicTensorObject(underlyingSymbolic) {
      var obj = O({
        "shape": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "shape");
          var optionValues = self.$underlyingSymbolic.shape.map((x) => {
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

    function isLayer(obj) {
      arity(1, arguments, "is-layer", false);
      return runtime.makeBoolean(hasBrand(brandLayer, obj));
    }

    // Constructor

    function buildLayerObject(underlyingLayer) {
      var obj = O({
        "apply-tensors": runtime.makeMethod1(function(self, tensors) {
          checkMethodArity(2, arguments, "apply-tensors");
          runtime.checkList(tensors);
          var inputs = runtime.ffi.toArray(tensors).map((x) => {
            checkTensor(x);
            return x.$underlyingTensor;
          });
          var outputs = self.$underlyingLayer.apply(inputs).map((x) => {
            buildTensorObject(x);
          });
          return runtime.ffi.makeList(outputs);
        }),
        "apply-symbolic-tensors": runtime.makeMethod1(function(self, symbolics) {
          checkMethodArity(2, arguments, "apply-symbolic-tensors");
          runtime.checkList(symbolics);
          var inputs = runtime.ffi.toArray(symbolics).map((x) => {
            checkTensor(x);
            return x.$underlyingSymbolic;
          });
          var outputs = self.$underlyingLayer.apply(inputs).map((x) => {
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

    function isOptimizer(obj) {
      arity(1, arguments, "is-optimizer", false);
      return runtime.makeBoolean(hasBrand(brandOptimizer, obj));
    }

    // Constructors

    function buildOptimizerObject(underlyingOptimizer) {
      var obj = O({
        "minimize": runtime.makeMethod2(function(self, functionToMinimize, varList) {
          checkMethodArity(3, arguments, "minimize");
          // varList is a list of mutable tensors for the Optimizer to edit. If
          // it is empty, it should be set to `undefined` so TensorFlow.js
          // knows to modify all available mutable tensors in the space:
          runtime.checkList(varList);
          var variables = runtime.ffi.toArray(varList)
                                     .map((v) => { return v.$underlyingTensor; });
          if (variables.length === 0) {
            variables = undefined;
          }
          // Run minimization lambda (returns a scalar):
          var result = self.$underlyingOptimizer.minimize(() => {
            return runtime.safeCall(() => {
              return functionToMinimize.app();
            }, (scalar) => {
              checkTensor(scalar);
              return scalar.$underlyingTensor;
            });
          }, true, variables);
          return buildTensorObject(result);
        })
      });
      obj = applyBrand(brandOptimizer, obj);
      obj.$underlyingOptimizer = underlyingOptimizer;
      return obj;
    }

    function trainSgd(learningRate) {
      arity(1, arguments, "train-sgd", false);
      runtime.checkNumber(learningRate);
      var rate = runtime.num_to_fixnum(learningRate);
      return buildOptimizerObject(tf.train.sgd(rate));
    }

    function trainMomentum(learningRate, momentum) {
      arity(2, arguments, "train-momentum", false);
      runtime.checkNumber(learningRate);
      runtime.checkNumber(momentum);
      var rate = runtime.num_to_fixnum(learningRate);
      var moment = runtime.num_to_fixnum(momentum);
      return buildOptimizerObject(tf.train.momentum(rate, moment));
    }

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

    function trainAdadelta(learningRate, rho, epsilon) {
      arity(3, arguments, "train-adadelta", false);
      var l = unwrapFixnumOption(learningRate);
      var r = unwrapFixnumOption(rho);
      var e = unwrapFixnumOption(epsilon);
      return buildOptimizerObject(tf.train.adadelta(l, r, e));
    }

    function trainAdam(learningRate, beta1, beta2, epsilon) {
      arity(4, arguments, "train-adam", false);
      var l = unwrapFixnumOption(learningRate);
      var b1 = unwrapFixnumOption(beta1);
      var b2 = unwrapFixnumOption(beta2);
      var e = unwrapFixnumOption(epsilon);
      return buildOptimizerObject(tf.train.adam(l, b1, b2, e));
    }

    function trainAdamax(learningRate, beta1, beta2, epsilon, decay) {
      arity(5, arguments, "train-adamax", false);
      var l = unwrapFixnumOption(learningRate);
      var b1 = unwrapFixnumOption(beta1);
      var b2 = unwrapFixnumOption(beta2);
      var e = unwrapFixnumOption(epsilon);
      var d = unwrapFixnumOption(decay);
      return buildOptimizerObject(tf.train.adam(l, b1, b2, e, d));
    }

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
      "train-sgd": F(trainSgd, "train-sgd"),
      "train-momentum": F(trainMomentum, "train-momentum"),
      "train-adagrad": F(trainAdagrad, "train-adagrad"),
      "train-adadelta": F(trainAdadelta, "train-sgd"),
      "train-adam": F(trainAdam, "train-adam"),
      "train-adamax": F(trainAdamax, "train-adamax"),
      "train-rmsprop": F(trainRmsprop, "train-rmsprop"),
    };
    var types = {
      Tensor: annTensor,
      Model: annModel,
      Sequential: annSequential,
      Layer: annLayer,
      Optimizer: annOptimizer,
    };
    var internal = {
      checkTensor: checkTensor,
      checkSequential: checkSequential,
      checkModel: checkModel,
      checkSymbolicTensor: checkSymbolicTensor,
      checkLayer: checkLayer,
      checkOptimizer: checkOptimizer
    };
    return runtime.makeModuleReturn(values, types, internal);
  }
})
