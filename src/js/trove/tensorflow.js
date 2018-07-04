({
  requires: [],
  nativeRequires: ["@tensorflow/tfjs"],
  provides: {},
  theModule: function(runtime, namespace, uri, tf) {

    /**
     * Tensorflow Brands and Annotations
     */
    var brandTensor = runtime.namedBrander("tensor", ["tensor"]);
    var annTensor = runtime.makeBranderAnn(brandTensor, "Tensor");

    var brandSequential = runtime.namedBrander("sequential", ["sequential"]);
    var brandModel = runtime.namedBrander("model", ["model"]);

    var annSequential = runtime.makeBranderAnn(brandSequential, "Sequential");
    var annModel = runtime.makeBranderAnn(brandModel, "Model");

    /**
     * Runtime Helpers
     */
    var O = runtime.makeObject;
    var F = runtime.makeFunction;
    var arity = runtime.checkArity;
    var get = runtime.getField;
    var unwrap = runtime.unwrap;

    var applyBrand = (brand, val) => { return get(brand, "brand").app(val); };

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
     * Tensorflow
     */
    function makeTensor(underlyingTensor) {
      var obj = O({
        "size": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "size");
          return runtime.makeNumber(self.$underlyingTensor.size);
        }),
        "flatten": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "flatten");
          return makeTensor(self.$underlyingTensor.flatten());
        }),
        "as-scalar": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "as-scalar");
          if (self.$underlyingTensor.size !== 1) {
            runtime.ffi.throwMessageException("Tensor was size-" +
              self.$underlyingTensor.size + " but `as-scalar` requires the " +
              "tensor to be size-1");
          }
          return makeTensor(self.$underlyingTensor.asScalar());
        }),
        "as-1d": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "as-1d");
          return makeTensor(self.$underlyingTensor.as1D());
        }),
        "as-2d": runtime.makeMethod2(function(self, rows, columns) {
          checkMethodArity(3, arguments, "as-2d");
          runtime.checkNumber(rows);
          runtime.checkNumber(columns);
          var r = unwrap(rows);
          var c = unwrap(columns);
          return makeTensor(self.$underlyingTensor.as2D(r, c));
        }),
        "as-3d": runtime.makeMethod3(function(self, rows, columns, depth) {
          checkMethodArity(4, arguments, "as-3d");
          runtime.checkNumber(rows);
          runtime.checkNumber(columns);
          runtime.checkNumber(depth);
          var r = unwrap(rows);
          var c = unwrap(columns);
          var d = unwrap(depth);
          return makeTensor(self.$underlyingTensor.as3D(r, c, d));
        }),
        "as-4d": runtime.makeMethod4(function(self, rows, columns, depth1, depth2) {
          checkMethodArity(5, arguments, "as-4d");
          runtime.checkNumber(rows);
          runtime.checkNumber(columns);
          runtime.checkNumber(depth1);
          runtime.checkNumber(depth2);
          var r = unwrap(rows);
          var c = unwrap(columns);
          var d1 = unwrap(depth1);
          var d2 = unwrap(depth2);
          return makeTensor(self.$underlyingTensor.as4D(r, c, d1, d2));
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
          return makeTensor(self.$underlyingTensor.asType(type));
        }),
        // "buffer": runtime.makeMethod0((self) => {
        //   checkMethodArity(0, arguments, "buffer");
        //   // TODO(Zachary): make TensorBuffer
        //   // return makeTensor(self.$underlyingTensor.asType(type));
        // })

        // "data": // Not going to implement `data` since it is async

        "data-sync": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "data-sync");
          // .dataSync returns a TypedArray, so convert it to a normal JSArray
          // so we can then convert it to a Pyret List:
          var typedArrayData = self.$underlyingTensor.dataSync();
          var arrayData = Array.from(typedArrayData);
          return runtime.ffi.makeList(arrayData);
        }),
        // "dispose":, Probably no need for this
        "to-float": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-float");
          return makeTensor(self.$underlyingTensor.toFloat());
        }),
        "to-int": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-int");
          return makeTensor(self.$underlyingTensor.toInt());
        }),
        "to-bool": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "to-bool");
          return makeTensor(self.$underlyingTensor.toBool());
        }),
        // "print":,
        "reshape": runtime.makeMethod0(function(self, newShape) {
          checkMethodArity(2, arguments, "reshape");
          runtime.checkList(newShape);
          var ns = runtime.toArray(newShape);
          return makeTensor(self.$underlyingTensor.reshape(ns));
        }),
        // "reshape-as":,
        // "expand-dims":,
        // "cumsum":,
        // "squeeze":,
        "clone": runtime.makeMethod0(function(self) {
          checkMethodArity(1, arguments, "clone");
          return makeTensor(self.$underlyingTensor.clone());
        }),
        // "to-string":
      });
      obj = applyBrand(brandTensor, obj);
      obj.$underlyingTensor = underlyingTensor;
      return obj;
    }

    function createTensor(values) {
      arity(1, arguments, "make-tensor", false);
      // A tensor can be rank 0 (just a number); otherwise, it is a List :(
      if (runtime.ffi.isList(values)) {
        values = runtime.ffi.toArray(values);
      }
      else {
        runtime.checkNumber(values);
      }
      var underlyingTensor = tf.tensor(values, null, null);
      return makeTensor(underlyingTensor);
    }

    var values = {
      "make-tensor": F(createTensor, "make-tensor")
    };
    var types = {
      Tensor: annTensor
    };

    return runtime.makeModuleReturn(values, types);
  }
})
