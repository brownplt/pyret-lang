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

    var brandModel = runtime.namedBrander("model", ["model: model brander"]);
    var annModel = runtime.makeBranderAnn(brandModel, "Model");

    var brandSequential = runtime.namedBrander("sequential", ["sequential: sequential brander"]);
    var annSequential = runtime.makeBranderAnn(brandSequential, "Sequential");

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
    function assertBrand(brand, val, name) {
      if (!hasBrand(brand, val)) {
        runtime.ffi.throwTypeMismatch(val, name);
      }
    }

    var unwrapObject = (obj) => { return unwrap(obj).dict; };

    var checkTensor = (val) => { runtime._checkAnn(["tensor"], annModel, val); };
    var checkSequential = (val) => { runtime._checkAnn(["sequential"], annSequential, val); };
    var checkModel = (val) => { runtime._checkAnn(["model"], annModel, val); };

    function checkMethodArity(arity, args, methodName) {
      if (args.length !== arity) {
        var $a=new Array(args.length);
        for (var $i=0;$i<args.length;$i++) {
          $a[$i]=args[$i];
        }
        throw runtime.ffi.throwArityErrorC([methodName], arity, $a, true);
      }
    }

    /**
     * Tensors
     */

    // Brand Checks

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
        }, "size"),
        "shape": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "shape");
          return runtime.ffi.makeList(self.$underlyingTensor.shape);
        }, "shape"),
        "flatten": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "flatten");
          return buildTensorObject(self.$underlyingTensor.flatten());
        }, "flatten"),
        "as-scalar": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "as-scalar");
          if (self.$underlyingTensor.size !== 1) {
            runtime.ffi.throwMessageException("Tensor was size-" +
              self.$underlyingTensor.size + " but `as-scalar` requires the " +
              "tensor to be size-1");
          }
          return buildTensorObject(self.$underlyingTensor.asScalar());
        }, "as-scalar"),
        "as-1d": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "as-1d");
          return buildTensorObject(self.$underlyingTensor.as1D());
        }, "as-1d"),
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
        }, "as-2d"),
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
        }, "as-3d"),
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
        }, "as-4d"),
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
        }, "as-type"),
        // "buffer": runtime.makeMethod0((self) => {
        //   checkMethodArity(0, arguments, "buffer");
        //   // TODO(Zachary): make TensorBuffer
        //   // return buildTensorObject(self.$underlyingTensor.asType(type));
        // })

        // "data": // Not going to implement `data` since it is async

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
        }, "data-sync"),
        // "dispose":, Probably no need for this
        "to-float": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-float");
          return buildTensorObject(self.$underlyingTensor.toFloat());
        }, "to-float"),
        "to-int": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-int");
          return buildTensorObject(self.$underlyingTensor.toInt());
        }, "to-int"),
        "to-bool": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-bool");
          return buildTensorObject(self.$underlyingTensor.toBool());
        }, "to-bool"),
        "to-variable": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-variable");
          return makeVariable(self);
        }, "to-variable"),
        // "print":,
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
        }, "reshape"),
        // "reshape-as":,
        // "expand-dims":,
        // "cumsum":,
        // "squeeze":,
        "clone": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "clone");
          return buildTensorObject(self.$underlyingTensor.clone());
        }, "clone"),
        // "to-string":
      });
      obj = applyBrand(brandTensor, obj);
      obj.$underlyingTensor = underlyingTensor;
      return obj;
    }

    function createTensor(values) {
      var underlyingTensor = tf.tensor(values, null, null);
      return buildTensorObject(underlyingTensor);
    }

    function createTensorFromArray(array) {
      arity(1, arguments, "tensor", false);
      runtime.checkArray(array);
      var fixnums = array.map((x) => { return runtime.num_to_fixnum(x); });
      return createTensor(fixnums);
    }

    function createTensor0() {
      arity(0, arguments, "tensor0", false);
      return createTensorFromArray([]);
    }

    function createTensor1(a) {
      arity(1, arguments, "tensor1", false);
      runtime.checkNumber(a);
      return createTensorFromArray([a]);
    }

    function createTensor2(a, b) {
      arity(2, arguments, "tensor2", false);
      runtime.checkNumber(a);
      runtime.checkNumber(b);
      return createTensorFromArray([a, b]);
    }

    function createTensor3(a, b, c) {
      arity(3, arguments, "tensor3", false);
      runtime.checkNumber(a);
      runtime.checkNumber(b);
      runtime.checkNumber(c);
      return createTensorFromArray([a, b, c]);
    }

    function createTensor4(a, b, c, d) {
      arity(4, arguments, "tensor4", false);
      runtime.checkNumber(a);
      runtime.checkNumber(b);
      runtime.checkNumber(c);
      runtime.checkNumber(d);
      return createTensorFromArray([a, b, c, d]);
    }

    function createTensor5(a, b, c, d, e) {
      arity(5, arguments, "tensor5", false);
      runtime.checkNumber(a);
      runtime.checkNumber(b);
      runtime.checkNumber(c);
      runtime.checkNumber(d);
      runtime.checkNumber(e);
      return createTensorFromArray([a, b, c, d, e]);
    }

    function listToTensor(values) {
      arity(1, arguments, "list-to-tensor", false);
      // A tensor can be rank 0 (just a number); otherwise, it is a List :(
      runtime.checkList(values);
      values = runtime.ffi.toArray(values);
      return createTensorFromArray(values);
    }

    function makeScalar(value) {
      arity(1, arguments, "make-scalar", false);
      runtime.checkNumber(value);
      var fixnum = runtime.num_to_fixnum(value);
      var newScalar = tf.scalar(fixnum);
      return buildTensorObject(newScalar);
    }

    function randomNormal(shape) {
      arity(1, arguments, "random-normal", false);
      runtime.checkList(shape);
      var s = runtime.ffi.toArray(shape);
      return buildTensorObject(tf.randomNormal(s));
    }

    function makeVariable(tensor) {
      arity(1, arguments, "make-variable", false);
      assertBrand(brandTensor, tensor, "Tensor");
      var newVariable = tf.variable(tensor.$underlyingTensor);
      return buildTensorObject(newVariable);
    }

    /**
     * Operations (Arithmetic)
     */

    // assertEqualShapes is not a Pyret function; it's a helper method for
    // the "strict" operation methods:
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

    function addTensors(a, b) {
      arity(2, arguments, "add-tensors", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
      return buildTensorObject(tf.add(aTensor, bTensor));
    }

    function addStrict(a, b) {
      arity(2, arguments, "strict-add-tensors", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      assertEqualShapes(a, b);
      return buildTensorObject(tf.add(aTensor, bTensor));
    }

    function subtractTensors(a, b) {
      arity(2, arguments, "subtract-tensors", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
      return buildTensorObject(tf.sub(aTensor, bTensor));
    }

    function subtractStrict(a, b) {
      arity(2, arguments, "strict-subtract-tensors", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      assertEqualShapes(a, b);
      return buildTensorObject(tf.sub(aTensor, bTensor));
    }

    function multiplyTensors(a, b) {
      arity(2, arguments, "multiply-tensors", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
      return buildTensorObject(tf.mul(aTensor, bTensor));
    }

    function multiplyStrict(a, b) {
      arity(2, arguments, "strict-multiply-tensors", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      assertEqualShapes(a, b);
      return buildTensorObject(tf.mul(aTensor, bTensor));
    }

    function divideTensors(a, b) {
      arity(2, arguments, "divide-tensors", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
      return buildTensorObject(tf.div(aTensor, bTensor));
    }

    function divideStrict(a, b) {
      arity(2, arguments, "strict-divide-tensors", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      assertEqualShapes(a, b);
      return buildTensorObject(tf.div(aTensor, bTensor));
    }

    function floorDivideTensors(a, b) {
      arity(2, arguments, "floor-divide-tensors", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
      return buildTensorObject(tf.floorDiv(aTensor, bTensor));
    }

    function maxTensor(a, b) {
      arity(2, arguments, "tensor-max", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
      return buildTensorObject(tf.maximum(aTensor, bTensor));
    }

    function maxStrict(a, b) {
      arity(2, arguments, "strict-tensor-max", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      assertEqualShapes(a, b);
      return buildTensorObject(tf.maximum(aTensor, bTensor));
    }

    function minTensor(a, b) {
      arity(2, arguments, "tensor-min", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
      return buildTensorObject(tf.minimum(aTensor, bTensor));
    }

    function minStrict(a, b) {
      arity(2, arguments, "strict-tensor-min", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      assertEqualShapes(a, b);
      return buildTensorObject(tf.minimum(aTensor, bTensor));
    }

    function moduloTensor(a, b) {
      arity(2, arguments, "tensor-modulo", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
      return buildTensorObject(tf.mod(aTensor, bTensor));
    }

    function moduloStrict(a, b) {
      arity(2, arguments, "strict-tensor-modulo", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      assertEqualShapes(a, b);
      return buildTensorObject(tf.mod(aTensor, bTensor));
    }

    function exptTensor(base, exp) {
      arity(2, arguments, "tensor-expt", false);
      assertBrand(brandTensor, base, "Tensor");
      assertBrand(brandTensor, exp, "Tensor");
      var baseTensor = base.$underlyingTensor;
      var expTensor = exp.$underlyingTensor;
      return buildTensorObject(tf.pow(baseTensor, expTensor));
    }

    function exptStrict(a, b) {
      arity(2, arguments, "strict-tensor-modulo", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      assertEqualShapes(a, b);
      return buildTensorObject(tf.pow(aTensor, bTensor));
    }

    function tensorSquaredDifference(a, b) {
      arity(2, arguments, "squared-difference", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
      return buildTensorObject(tf.squaredDifference(aTensor, bTensor));
    }

    function strictSquaredDifference(a, b) {
      arity(2, arguments, "strict-squared-difference", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      assertEqualShapes(a, b);
      return buildTensorObject(tf.squaredDifference(aTensor, bTensor));
    }

    /**
     * Operations (Basic Math)
     */

    function abs(x) {
      arity(1, arguments, "tensor-abs", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.abs(tensor));
    }

    function acos(x) {
      arity(1, arguments, "tensor-acos", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.acos(tensor));
    }

    function acosh(x) {
      arity(1, arguments, "tensor-acosh", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.acosh(tensor));
    }

    function asin(x) {
      arity(1, arguments, "tensor-asin", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.asin(tensor));
    }

    function asinh(x) {
      arity(1, arguments, "tensor-asinh", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.asinh(tensor));
    }

    function atan(x) {
      arity(1, arguments, "tensor-atan", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.atan(tensor));
    }

    function atan2(a, b) {
      arity(2, arguments, "tensor-atan2", false);
      assertBrand(brandTensor, a, "Tensor");
      assertBrand(brandTensor, b, "Tensor");
      var aTensor = a.$underlyingTensor;
      var bTensor = b.$underlyingTensor;
      return buildTensorObject(tf.atan2(aTensor, bTensor));
    }

    function atanh(x) {
      arity(1, arguments, "tensor-atanh", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.atanh(tensor));
    }

    function ceil(x) {
      arity(1, arguments, "tensor-ceil", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.atan(tensor));
    }

    function clipByValue(x, min, max) {
      arity(3, arguments, "clip-by-value", false);
      assertBrand(brandTensor, x, "Tensor");
      runtime.checkNumber(min);
      runtime.checkNumber(max);
      var tensor = x.$underlyingTensor;
      var mi = unwrap(min);
      var ma = unwrap(max);
      return buildTensorObject(tf.clipByValue(tensor, mi, ma));
    }

    function cos(x) {
      arity(1, arguments, "tensor-cos", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.cos(tensor));
    }

    function cosh(x) {
      arity(1, arguments, "tensor-cosh", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.cosh(tensor));
    }

    function elu(x) {
      arity(1, arguments, "elu", false);
      return exponentialLinearUnits(x);
    }

    function exponentialLinearUnits(x) {
      arity(1, arguments, "exponential-linear-units", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.elu(tensor));
    }

    function erf(x) {
      arity(1, arguments, "erf", false);
      return gaussError(x);
    }

    function gaussError(x) {
      arity(1, arguments, "gauss-error", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.erf(tensor));
    }

    function exp(x) {
      arity(1, arguments, "tensor-exp", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.exp(tensor));
    }

    function expm1(x) {
      arity(1, arguments, "tensor-exp-min1", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.expm1(tensor));
    }

    function floor(x) {
      arity(1, arguments, "tensor-floor", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.floor(tensor));
    }

    function leakyRelu(x, alpha) {
      arity(2, arguments, "leaky-relu", false);
      assertBrand(brandTensor, x, "Tensor");
      runtime.checkNumber(alpha);
      var tensor = x.$underlyingTensor;
      var a = unwrap(alpha);
      return buildTensorObject(tf.leakyRelu(tensor, a));
    }

    function log(x) {
      arity(1, arguments, "tensor-log", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.log(tensor));
    }

    function log1p(x) {
      arity(1, arguments, "tensor-log-plus1", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.log1p(tensor));
    }

    function logSigmoid(x) {
      arity(1, arguments, "log-sigmoid", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.logSigmoid(tensor));
    }

    function neg(x) {
      arity(1, arguments, "tensor-negate", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.neg(tensor));
    }

    function prelu(x, alpha) {
      arity(2, arguments, "parametric-relu", false);
      assertBrand(brandTensor, x, "Tensor");
      runtime.checkNumber(alpha);
      var tensor = x.$underlyingTensor;
      var a = runtime.num_to_fixnum(alpha);
      return buildTensorObject(tf.prelu(tensor, a));
    }

    function reciprocal(x) {
      arity(1, arguments, "tensor-reciprocal", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.reciprocal(tensor));
    }

    function relu(x) {
      arity(1, arguments, "relu", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.relu(tensor));
    }

    function round(x) {
      arity(1, arguments, "tensor-round", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.round(tensor));
    }

    function rsqrt(x) {
      arity(1, arguments, "reciprocal-sqrt", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.rsqrt(tensor));
    }

    function selu(x) {
      arity(1, arguments, "scaled-elu", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.selu(tensor));
    }

    function sigmoid(x) {
      arity(1, arguments, "sigmoid", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.sigmoid(tensor));
    }

    function sign(x) {
      arity(1, arguments, "signed-ones", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.sign(tensor));
    }

    function sin(x) {
      arity(1, arguments, "tensor-sin", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.sin(tensor));
    }

    function sinh(x) {
      arity(1, arguments, "tensor-sinh", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.sinh(tensor));
    }

    function softplus(x) {
      arity(1, arguments, "softplus", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.softplus(tensor));
    }

    function sqrt(x) {
      arity(1, arguments, "tensor-sqrt", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.sqrt(tensor));
    }

    function square(x) {
      arity(1, arguments, "tensor-square", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.square(tensor));
    }

    function step(x) {
      arity(1, arguments, "step", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.step(tensor));
    }

    function tan(x) {
      arity(1, arguments, "tensor-tan", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.tan(tensor));
    }

    function tanh(x) {
      arity(1, arguments, "tensor-tanh", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.tanh(tensor));
    }

    /**
     * Operations (Reduction)
     */

    function all(x) {
      arity(1, arguments, "all", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.all(tensor));
    }

    function any(x) {
      arity(1, arguments, "any", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.any(tensor));
    }

    function argMax(x) {
      arity(1, arguments, "arg-max", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.argMax(tensor));
    }

    function argMin(x) {
      arity(1, arguments, "arg-min", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.argMin(tensor));
    }

    function logSumExp(x) {
      arity(1, arguments, "log-sum-exp", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.logSumExp(tensor));
    }

    function max(x) {
      arity(1, arguments, "reduce-max", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.max(tensor));
    }

    function mean(x) {
      arity(1, arguments, "reduce-mean", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.mean(tensor));
    }

    function min(x) {
      arity(1, arguments, "reduce-min", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.min(tensor));
    }

    function sum(x) {
      arity(1, arguments, "reduce-sum", false);
      assertBrand(brandTensor, x, "Tensor");
      var tensor = x.$underlyingTensor;
      return buildTensorObject(tf.sum(tensor));
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
        }, "add"),
        "compile": runtime.makeMethod1(function(self, config) {
          checkMethodArity(2, arguments, "compile");
          runtime.checkObject(config);
          var c = unwrapObject(config);
          self.$underlyingSequential.compile(c);
          return runtime.makeNothing();
        }, "compile"),
        "evaluate": runtime.makeMethod3(function(self, x, y, config) {
          checkMethodArity(4, arguments, "evaluate");
          if (!hasBrand(brandTensor, x)) {
            runtime.ffi.throwTypeMismatch(x, "Tensor");
          }
          if (!hasBrand(brandTensor, y)) {
            runtime.ffi.throwTypeMismatch(y, "Tensor");
          }
          runtime.checkObject(config);
          var xTensor = x.$underlyingTensor;
          var yTensor = y.$underlyingTensor;
          var c = unwrapObject(config);
          var result = self.$underlyingSequential.evaluate(xTensor, yTensor, c);
          return buildTensorObject(result);
        }, "evaluate"),
        "predict": runtime.makeMethod2(function(self, x, config) {
          checkMethodArity(3, arguments, "predict");
          if (!hasBrand(brandTensor, x)) {
            runtime.ffi.throwTypeMismatch(x, "Tensor");
          }
          runtime.checkObject(config);
          var xTensor = x.$underlyingTensor;
          var c = unwrapObject(config);
          var result = self.$underlyingSequential.predict(xTensor, c);
          return buildTensorObject(result);
        }, "predict"),
        "predict-on-batch": runtime.makeMethod1(function(self, x) {
          checkMethodArity(2, arguments, "predict-on-batch");
          if (!hasBrand(brandTensor, x)) {
            runtime.ffi.throwTypeMismatch(x, "Tensor");
          }
          var xTensor = x.$underlyingTensor;
          var result = self.$underlyingSequential.predictOnBatch(xTensor, c);
          return buildTensorObject(result);
        }, "predict-on-batch"),
        "fit": runtime.makeMethod3(function(self, x, y, config, callback) {
          checkMethodArity(5, arguments, "fit");
          if (!hasBrand(brandTensor, x)) {
            runtime.ffi.throwTypeMismatch(x, "Tensor");
          }
          if (!hasBrand(brandTensor, y)) {
            runtime.ffi.throwTypeMismatch(y, "Tensor");
          }
          runtime.checkObject(config);
          var xTensor = x.$underlyingTensor;
          var yTensor = y.$underlyingTensor;
          var c = unwrapObject(config);
          c.callbacks = {onEpochEnd: async (epoch, log) => {
            console.log(epoch);
            console.log(log);
            callback.app(runtime.makeNumber(epoch), runtime.makeObject(log));
          }};
          self.$underlyingSequential.fit(xTensor, yTensor, c);
          return runtime.makeNothing();
        }, "fit")
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
     * Dense Layer
     */

    // Brand Checks

    var brandLayer = runtime.namedBrander("dense-layer", ["dense-layer: dense-layer brander"]);
    var annLayer = runtime.makeBranderAnn(brandLayer, "Layer");

    function isDenseLayer(obj) {
      arity(1, arguments, "is-dense-layer", false);
      return runtime.makeBoolean(hasBrand(brandLayer, obj));
    }

    // Constructor

    function buildDenseLayerObject(underlyingLayer) {
      var obj = O({
        // "apply":,
      });
      obj = applyBrand(brandLayer, obj);
      obj.$underlyingLayer = underlyingLayer;
      return obj;
    }

    function makeDenseLayer(config) {
      arity(1, arguments, "make-dense-layer", false);
      runtime.checkObject(config);
      var c = unwrapObject(config);
      var layer = tf.layers.dense(c);
      return buildDenseLayerObject(layer);
    }

    /**
     * Training (Optimizers)
     */

    // Brand Checks

    var brandOptimizer = runtime.namedBrander("optimizer", ["optimizer: optimizer brander"]);
    var annOptimizer = runtime.makeBranderAnn(brandOptimizer, "Optimizer");

    function isOptimizer(obj) {
      arity(1, arguments, "is-optimizer", false);
      return runtime.makeBoolean(hasBrand(brandOptimizer, obj));
    }

    // Constructors

    function buildOptimizerObject(underlyingOptimizer) {
      var obj = O({
        "minimize": runtime.makeMethod2(function(self, functionToMinimize, varList) {
          checkMethodArity(3, arguments, "minimize");
          runtime.checkList(varList);
          var variables = runtime.ffi.toArray(varList)
                                     .map((v) => { return v.$underlyingTensor; });
          if (variables.length === 0) {
            variables = null;
          }
          // Run minimization lambda:
          self.$underlyingOptimizer.minimize(function() {
            var scalar = functionToMinimize.app();
            assertBrand(brandTensor, scalar, "Tensor");
            return scalar.$underlyingTensor;
          }); //, true, variables);
          return runtime.makeNothing();
        }, "minimize")
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

    function trainMomentum() {

    }

    function trainAdagrad() {

    }

    function trainAdadelta() {

    }

    function trainAdam() {

    }

    function trainAdamax() {

    }

    function trainRmsprop() {

    }

    var values = {
      // Tensors
      "is-tensor": F(isTensor, "is-tensor"),
      "list-to-tensor": F(listToTensor, "list-to-tensor"),
      "make-scalar": F(makeScalar, "make-scalar"),
      "tensor": O({
        make: F(createTensorFromArray, "tensor:make"),
        make0: F(createTensor0, "tensor:make0"),
        make1: F(createTensor1, "tensor:make1"),
        make2: F(createTensor2, "tensor:make2"),
        make3: F(createTensor3, "tensor:make3"),
        make4: F(createTensor4, "tensor:make4"),
        make5: F(createTensor5, "tensor:make5")
      }),
      "random-normal": F(randomNormal, "random-normal"),
      "make-variable": F(makeVariable, "make-variable"),

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

      // Models (Generic)
      "is-model": F(isModel, "is-model"),
      "make-model": F(makeModel, "make-model"),

      // Models (Sequential)
      "is-sequential": F(isSequential, "is-sequential"),
      "make-sequential": F(makeSequential, "make-sequential"),

      // Layers
      "is-dense-layer": F(isDenseLayer, "is-dense-layer"),
      "make-dense-layer": F(makeDenseLayer, "make-dense-layer"),

      // Training (Optimizers)
      "is-optimizer": F(isOptimizer, "is-optimizer"),
      "train-sgd": F(trainSgd, "train-sgd"),
    };
    var types = {
      Tensor: annTensor,
      Model: annModel,
      Sequential: annSequential,
      Layer: annLayer,
      Optimizer: annOptimizer,
    };
    var internal = {
      checkTensor: checkTensor
    };
    return runtime.makeModuleReturn(values, types, internal);
  }
})
