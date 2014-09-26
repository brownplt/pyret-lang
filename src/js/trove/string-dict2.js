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
        var newDict = Object.create(null);
        for (var i = 0; i < keys.length; i++) {
          newDict[keys[i]] = dict[keys[i]];
        }
        return newDict;
      }

      function makeImmutableStringDict(underlyingObj) {

        var getSD = runtime.makeMethodFromFun(function(_, key) {
          runtime.checkArity(2, arguments, 'get');
          runtime.checkString(key);
          var mkey = internalKey(key);
          var val = underlyingObj[mkey];
          if (val === undefined) {
            runtime.ffi.throwMessageException('Key ' + key + ' not found');
          }
          if (!underlyingObj.hasOwnProperty(mkey)) {
            underlyingObj[mkey] = val;
          }
          return val;
        });

        var setSD = runtime.makeMethodFromFun(function(_, key, val) {
          runtime.checkArity(3, arguments, 'set');
          runtime.checkString(key);
          runtime.checkPyretVal(val);
          var newObj = Object.create(underlyingObj);
          var mkey = internalKey(key);
          newObj[mkey] = val;
          return makeImmutableStringDict(newObj);
        });

        var removeSD = runtime.makeMethodFromFun(function(_, key) {
          runtime.checkArity(2, arguments, 'remove');
          runtime.checkString(key);
          var newObj = Object.create(underlyingObj);
          var mkey = internalKey(key);
          newObj[mkey] = undefined;
          return makeImmutableStringDict(newObj);
        });

        var hasKeySD = runtime.makeMethodFromFun(function(_, key) {
          runtime.checkArity(2, arguments, 'has-key');
          runtime.checkString(key);
          var mkey = internalKey(key);
          if (mkey in underlyingObj) {
            return runtime.makeBoolean(true);
          } else {
            return runtime.makeBoolean(false);
          }
        });

        function getAllKeys() {
          var keys = [];
          for (var key in underlyingObj) {
            keys.push(key);
          }
          return keys;
        }

        var keysSD = runtime.makeMethodFromFun(function(_) {
          runtime.checkArity(1, arguments, 'keys');
          var keys = getAllKeys();
          return runtime.ffi.makeList(keys.map(function(mkey) {
            return runtime.makeString(userKey(mkey));
          }));
        });

        var countSD = runtime.makeMethodFromFun(function(_) {
          runtime.checkArity(1, arguments, 'count');
          var num = 0;
          for (var prop in underlyingObj) {
            num++;
          }
          return runtime.makeNumber(num);
        });

        var toreprSD = runtime.makeMethodFromFun(function(_, recursiveToRepr) {
          runtime.checkArity(2, arguments, 'torepr');
          var elts = [];
          var keys = getAllKeys();
          function combine(elts) {
            return '[immutable-string-dict: ' + elts.join(', ') + ']';
          }
          function toreprElts() {
            if (keys.length === 0) {
              return combine(elts);
            } else {
              var thisKey = keys.pop();
              return runtime.safeCall(function() {
                return recursiveToRepr.app(underlyingObj[thisKey]);
              },
              function (result) {
                elts.push(recursiveToRepr.app(userKey(thisKey)));
                elts.push(result);
                return toreprElts();
              });
            }
          }
          return toreprElts();
        });

        var equalsSD = runtime.makeMethodFromFun(function(_, other, recursiveEquality) {
          runtime.checkArity(3, arguments, 'equals');
          if (!hasBrand(brandImmutable, other)) {
            return runtime.ffi.notEqual.app('');
          } else {
            var keys = getAllKeys();
            var otherKeysLength = get(other, 'count').app();
            function equalsHelp() {
              if (keys.length === 0) {
                return runtime.ffi.equal;
              } else {
                var thisKey = keys.pop();
                return runtime.safeCall(function() {
                  return recursiveEquality.app(underlyingObj[thisKey],
                      get(other, 'get').app(userKey(thisKey)));
                },
                function (result) {
                  if (runtime.ffi.isNotEqual(result)) {
                    return result;
                  } else {
                    return equalsHelp();
                  }
                });
              }
            }
            if (keys.length !== otherKeysLength) {
              return runtime.ffi.notEqual.app('');
            } else {
              return equalsHelp();
            }
          }
        });

        obj = O({
          get: getSD,
          set: setSD,
          remove: removeSD,
          keys: keysSD,
          count: countSD,
            'has-key': hasKeySD,
          _equals: equalsSD,
          _torepr: toreprSD
        });

        return applyBrand(brandImmutable, obj);

      }

      function makeMutableStringDict(underlyingDict) {
        // NOTE(joe): getSD/setSD etc are internal to
        // makeStringDict because they need to close over underlyingDict

        var getSD = runtime.makeMethodFromFun(function(_, key) {
          runtime.checkArity(2, arguments, "get");
          runtime.checkString(key);
          var mkey = internalKey(key);
          if(!underlyingDict[mkey]) {
            runtime.ffi.throwMessageException("Key " + key + " not found");
          }
          return underlyingDict[mkey]
        });

        var setSD = runtime.makeMethodFromFun(function(self, key, val) {
          runtime.checkArity(3, arguments, "set");
          runtime.checkString(key);
          runtime.checkPyretVal(val);
          underlyingDict[internalKey(key)] = val;
          return self;
        });

        var removeSD = runtime.makeMethodFromFun(function(self, key) {
          runtime.checkArity(2, arguments, "remove");
          runtime.checkString(key);
          delete underlyingDict[internalKey(key)];
          return self;
        });

        var hasKeySD = runtime.makeMethodFromFun(function(_, key) {
          runtime.checkArity(2, arguments, "has-key");
          runtime.checkString(key);
          var mkey = internalKey(key);
          if (mkey in underlyingDict) {
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

        var countSD = runtime.makeMethodFromFun(function(_) {
          runtime.checkArity(1, arguments, "count");
          return runtime.makeNumber(Object.keys(underlyingDict).length);
        });

        var toreprSD = runtime.makeMethodFromFun(function(self, recursiveToRepr) {
          runtime.checkArity(2, arguments, "torepr");
          var keys = Object.keys(underlyingDict);
          var elts = [];
          function combine(elts) {
            //return "[string-dict: " + elts.join(", ") + "]";
            return "[string-dict: " + elts.join(", ") + "]";
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

        var equalsSD = runtime.makeMethodFromFun(function(self, other, recursiveEquality) {
          runtime.checkArity(3, arguments, "equals");
          if (!hasBrand(brandMutable, other)) {
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
          'count': countSD,
            'has-key': hasKeySD,
          _equals: equalsSD,
          _torepr: toreprSD
        });

        return applyBrand(brandMutable, obj);
      }

      function createMutableStringDict() {
        arity(0, arguments, "make-string-dict");
        var dict = Object.create(null);
        return makeMutableStringDict(dict);
      }

      function createMutableStringDictFromArray(array) {
        arity(1, arguments, "string-dict");
        runtime.checkArray(array);
        var dict = Object.create(null);
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
        return makeMutableStringDict(dict);
      }

      function createImmutableStringDict() {
        arity(0, arguments, "make-immutable-string-dict");
        var dict = {};
        return makeImmutableStringDict(dict);
      }

      function createImmutableStringDictFromArray(array) {
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
        return makeImmutableStringDict(dict);
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
                make: F(createImmutableStringDictFromArray)
              })
          })
        }),
        "answer": runtime.nothing
      });

    });
  });
});
