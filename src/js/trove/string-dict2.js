define(["js/runtime-util", "js/namespace", "js/ffi-helpers"], function(util, Namespace, ffi) {
  return util.memoModule("string-dict2", function(runtime, namespace) {
    return runtime.loadJSModules(namespace, [ffi], function(F) {

      var O = runtime.makeObject;
      var F = runtime.makeFunction;
      var arity = runtime.checkArity;
      var get = runtime.getField;

      var brandMutable = runtime.namedBrander("string-dict");
      var brandImmutable = runtime.namedBrander("immutable-string-dict");

      var annMutable = runtime.makeBranderAnn(brandMutable, "StringDict");
      var annImmutable = runtime.makeBranderAnn(brandImmutable, "ImmutableStringDict");

      function applyBrand(brand, val) {
        return get(brand, "brand").app(val);
      }
      function hasBrand(brand, val) {
        return get(brand, "test").app(val);
      }

      // Prepend a space to avoid conflicting with built-in names
      function internalKey(s) {
        return " " + s;
      }
      // Remove it when internal keys need to be shown to the user
      function userKey(s) {
        return s.slice(1);
      }

      function cloneDict(dict) {
        var keys = Object.keys(dict);
        var newDict = {};
        for (var i = 0; i < keys.length; i++) {
          newDict[keys[i]] = dict[keys[i]];
        }
        return newDict;
      }

      function makeStringDict(underlyingDict, mutableP) {
        // NOTE(joe): getSD/setSD etc are internal to
        // makeStringDict because they need to close over underlyingDict
        var myBrand = mutableP ? brandMutable : brandImmutable;
        var toreprPrefix = mutableP ? "string-dict" : "immutable-string-dict";

        var getSD = runtime.makeMethodFromFun(function(_, key) {
          runtime.checkArity(2, arguments, "get");
          runtime.checkString(key);
          var mkey = internalKey(key);
          if(!underlyingDict[mkey]) {
            runtime.ffi.throwMessageException("Key " + key + " not found");
          }
          return underlyingDict[mkey]
        });

        var setSD;

        if (mutableP) {
          setSD = runtime.makeMethodFromFun(function(self, key, val) {
            runtime.checkArity(3, arguments, "get");
            runtime.checkString(key);
            runtime.checkPyretVal(val);
            underlyingDict[internalKey(key)] = val;
            return self;
          });
        } else {
          setSD = runtime.makeMethodFromFun(function(self, key, val) {
            runtime.checkArity(3, arguments, "get");
            runtime.checkString(key);
            runtime.checkPyretVal(val);
            var newDict = cloneDict(underlyingDict);
            newDict[internalKey(key)] = val;
            return makeStringDict(newDict, false);
          });
        }

        if (mutableP) {
          removeSD = runtime.makeMethodFromFun(function(self, key) {
            runtime.checkArity(2, arguments, "remove");
            runtime.checkString(key);
            delete underlyingDict[internalKey(key)];
            return self;
          });
        } else {
          removeSD = runtime.makeMethodFromFun(function(self, key) {
            runtime.checkArity(2, arguments, "remove");
            runtime.checkString(key);
            var newDict = cloneDict(underlyingDict);
            delete newDict[internalKey(key)];
            return makeStringDict(newDict, false);
          });
        }

        var hasKeySD = runtime.makeMethodFromFun(function(_, key) {
          runtime.checkArity(2, arguments, "has-key");
          runtime.checkString(key);
          var mkey = internalKey(key);
          if (underlyingDict[mkey]) {
            return runtime.makeBoolean(true);
          } else {
            return runtime.makeBoolean(false);
          }
        });

        var keysSD = runtime.makeMethodFromFun(function(self) {
          runtime.checkArity(1, arguments, "keys");
          var keys = Object.keys(underlyingDict);
          return runtime.ffi.makeList(keys.map(function(mkey) {
            return runtime.makeString(userKey(mkey));
          }));
        });

        var torepr = runtime.makeMethodFromFun(function(self, recursiveToRepr) {
          runtime.checkArity(2, arguments, "torepr");
          var keys = Object.keys(underlyingDict);
          var elts = [];
          function combine(elts) {
            //return "[string-dict: " + elts.join(", ") + "]";
            return "[" + toreprPrefix + ": " + elts.join(", ") + "]";
          }
          function toreprElts() {
            if (keys.length === 0) { return combine(elts); }
            else {
              var thisKey = keys.pop();
              // The function recursiveToRepr is a callback for rendering
              // sub-elements of collections.  If we call it on anything other
              // than flat primitives, we need to use the following safeCall
              // calling convention, which makes this work with the stack
              // compilation strategy for Pyret.
              return runtime.safeCall(function() {
                return recursiveToRepr.app(underlyingDict[thisKey]);
              },
              function(result /* stringification of element */) {
                elts.push(recursiveToRepr.app(userKey(thisKey)));
                elts.push(result);
                return toreprElts();
              });
            }
          }
          return toreprElts();
        });

        var equals = runtime.makeMethodFromFun(function(self, other, recursiveEquality) {
          runtime.checkArity(3, arguments, "equals");
          if (!hasBrand(myBrand, other)) {
            return runtime.ffi.notEqual.app("");
          } else {
            var keys = Object.keys(underlyingDict);
            var otherKeysLength = runtime.ffi.toArray(get(other, "keys").app()).length;
            // var otherKeysLength = get(get(other,"keys").app(),"length").app();
            function eqElts() {
              if (keys.length === 0) {
                return runtime.ffi.equal;
              } else {
                var thisKey = keys.pop();
                return runtime.safeCall(function() {
                  return recursiveEquality.app(underlyingDict[thisKey],
                      get(other,"get").app(userKey(thisKey)));
                },
                function (result) {
                  if (runtime.ffi.isNotEqual(result)) {
                    return result;
                  } else {
                    return eqElts();
                  }
                  /*
                  return runtime.combineEquality(result,
                      eqElts());
                  */
                });
              }
            }
            if (keys.length !== otherKeysLength) {
              return runtime.ffi.notEqual.app("");
            } else {
              return eqElts();
            }
          }
        });

        var NYI = runtime.makeMethodFromFun(function(self) {
          runtime.ffi.throwMessageException("Not yet implemented");
        });

        obj = O({
          get: getSD,
          set: setSD,
          remove: removeSD,
          'keys': keysSD,
            'has-key': hasKeySD,
          _equals: equals,
          _torepr: torepr
        });

        return applyBrand(myBrand, obj);
      }

      function createMutableStringDict() {
        arity(0, arguments, "make-string-dict");
        return makeStringDict({}, true);
      }

      function createMutableStringDictFromArray(array) {
        arity(1, arguments, "string-dict");
        runtime.checkArray(array);
        var dict = {};
        var len = array.length;
        if(len % 2 !== 0) {
          runtime.ffi.throwMessageException("Expected an even number of arguments to constructor for mutable dictionaries, got array of length " + len);
        }
        for(var i = 0; i < len; i += 2) {
          var key = array[i];
          var val = array[i + 1];
          runtime.checkString(key);
          dict[internalKey(key)] = val;
        }
        return makeStringDict(dict, true);
      }

      function createImmutableStringDict() {
        arity(0, arguments, "make-immutable-string-dict");
        return makeStringDict({}, false);
      }

      function createImutableStringDictFromArray(array) {
        arity(1, arguments, "immutable-string-dict");
        runtime.checkArray(array);
        var dict = {};
        var len = array.length;
        if(len % 2 !== 0) {
          runtime.ffi.throwMessageException("Expected an even number of arguments to constructor for immutable dictionaries, got array of length " + len);
        }
        for(var i = 0; i < len; i += 2) {
          var key = array[i];
          var val = array[i + 1];
          runtime.checkString(key);
          dict[internalKey(key)] = val;
        }
        return makeStringDict(dict, false);
      }

      var NYIF = F(function() {
        runtime.ffi.throwMessageException("Not yet implemented");
      });

      return O({
        "provide-plus-types": O({
          types: {
            StringDict: annMutable,
            ImmutableStringDict: annImmutable
          },
          values: O({
            "make-string-dict": F(createMutableStringDict),
            "string-dict": O({
              make: F(createMutableStringDictFromArray)
            }),
            "make-immutable-string-dict": F(createImmutableStringDict),
            "immutable-string-dict": O({
              make: F(createImutableStringDictFromArray)
            })
          })
        }),
        "answer": runtime.nothing
      });

    });
  });
});
