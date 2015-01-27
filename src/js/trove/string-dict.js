define(["js/runtime-util", "js/namespace", "js/ffi-helpers"], function(util, Namespace, ffi) {
  return util.memoModule("string-dict2", function(runtime, namespace) {
    return runtime.loadJSModules(namespace, [ffi], function(F) {

      var O = runtime.makeObject;
      var F = runtime.makeFunction;
      var arity = runtime.checkArity;
      var get = runtime.getField;

      var brandMutable = runtime.namedBrander("mutable-string-dict");
      var brandImmutable = runtime.namedBrander("string-dict");

      var annMutable = runtime.makeBranderAnn(brandMutable, "MutableStringDict");
      var annImmutable = runtime.makeBranderAnn(brandImmutable, "StringDict");

      var checkMSD = function(v) { runtime._checkAnn(["string-dict"], annMutable, v); };
      var checkISD = function(v) { runtime._checkAnn(["string-dict"], annImmutable, v); };

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

      function makeImmutableStringDict(underlyingDict) {

        var getISD = runtime.makeMethod1(function(_, key) {
          runtime.checkArity(2, arguments, 'get');
          runtime.checkString(key);
          var mkey = internalKey(key);
          var val = underlyingDict[mkey];
          if (val === undefined) {
            return runtime.ffi.makeNone();
          } else {
            if (!Object.prototype.hasOwnProperty.call(underlyingDict, mkey)) {
              underlyingDict[mkey] = val;
            }
            return runtime.ffi.makeSome(val);
          }
        });

        var getValueISD = runtime.makeMethod1(function(_, key) {
          runtime.checkArity(2, arguments, 'get-value');
          runtime.checkString(key);
          var mkey = internalKey(key);
          var val = underlyingDict[mkey];
          if (val === undefined) {
            runtime.ffi.throwMessageException('Key ' + key + ' not found');
          }
          if (!Object.prototype.hasOwnProperty.call(underlyingDict, mkey)) {
            underlyingDict[mkey] = val;
          }
          return val;
        });

        var setISD = runtime.makeMethod2(function(_, key, val) {
          runtime.checkArity(3, arguments, 'set');
          runtime.checkString(key);
          runtime.checkPyretVal(val);
          var newObj = Object.create(underlyingDict);
          var mkey = internalKey(key);
          newObj[mkey] = val;
          return makeImmutableStringDict(newObj);
        });

        var mergeISD = runtime.makeMethod1(function(self, other) {
          runtime.checkArity(2, arguments, "merge");
          checkISD(other);
          var otherKeys = runtime.getField(other, "keys-list").app();
          var otherKeysArr = runtime.ffi.toArray(otherKeys);
          if(otherKeysArr.length === 0) { return self; }
          var newObj = Object.create(underlyingDict);
          for(var i = 0; i < otherKeysArr.length; i++) {
            var mkey = internalKey(otherKeysArr[i])
            newObj[mkey] = runtime.getField(other, "get-value").app(otherKeysArr[i]);
          }
          return makeImmutableStringDict(newObj);
        });

        var removeISD = runtime.makeMethod1(function(_, key) {
          runtime.checkArity(2, arguments, 'remove');
          runtime.checkString(key);
          var newObj = Object.create(underlyingDict);
          var mkey = internalKey(key);
          newObj[mkey] = undefined;
          return makeImmutableStringDict(newObj);
        });

        var hasKeyISD = runtime.makeMethod1(function(_, key) {
          runtime.checkArity(2, arguments, 'has-key');
          runtime.checkString(key);
          var mkey = internalKey(key);
          var val = underlyingDict[mkey];
          if (val !== undefined) {
            if (!Object.prototype.hasOwnProperty.call(underlyingDict, mkey)) {
              underlyingDict[mkey] = val;
            }
            return runtime.makeBoolean(true);
          } else {
            return runtime.makeBoolean(false);
          }
        });

        function getAllKeys() {
          var keys = [];
          for (var key in underlyingDict) {
            if (underlyingDict[key] !== undefined) {
              keys.push(key);
            }
          }
          return keys;
        }

        var keysISD = runtime.makeMethod0(function(_) {
          runtime.checkArity(1, arguments, 'keys');
          var keys = getAllKeys();
          return runtime.ffi.makeTreeSet(keys.map(function(mkey) {
            return runtime.makeString(userKey(mkey));
          }));
        });

        var keysListISD = runtime.makeMethod0(function(_) {
          runtime.checkArity(1, arguments, 'keys-list');
          var keys = getAllKeys();
          return runtime.ffi.makeList(keys.map(function(mkey) {
            return runtime.makeString(userKey(mkey));
          }));
        });

        var countISD = runtime.makeMethod0(function(_) {
          runtime.checkArity(1, arguments, 'count');
          var num = 0;
          for (var key in underlyingDict) {
            if (underlyingDict[key] !== undefined) {
              num++;
            }
          }
          return runtime.makeNumber(num);
        });

        var toreprISD = runtime.makeMethod1(function(_, recursiveToRepr) {
          runtime.checkArity(2, arguments, 'torepr');
          var elts = [];
          var keys = getAllKeys();
          function combine(elts) {
            return '[string-dict: ' + elts.join(', ') + ']';
          }
          function toreprElts() {
            if (keys.length === 0) {
              return combine(elts);
            } else {
              var thisKey = keys.pop();
              return runtime.safeCall(function() {
                return recursiveToRepr.app(underlyingDict[thisKey]);
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

        var equalsISD = runtime.makeMethod2(function(_, other, recursiveEquality) {
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
                if (!get(other, 'has-key').app(userKey(thisKey))) {
                  return runtime.ffi.notEqual.app('');
                } else {
                  return runtime.safeCall(function() {
                    return recursiveEquality.app(underlyingDict[thisKey],
                        get(other, 'get-value').app(userKey(thisKey)));
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
            }
            if (keys.length !== otherKeysLength) {
              return runtime.ffi.notEqual.app('');
            } else {
              return equalsHelp();
            }
          }
        });

        var unfreezeISD = runtime.makeMethod0(function(_) {
          runtime.checkArity(1, arguments, 'unfreeze');
          var dict = Object.create(null);
          for (var mkey in underlyingDict) {
            dict[mkey] = underlyingDict[mkey];
          }
          return makeMutableStringDict(dict);
        });

        obj = O({
          get: getISD,
          'get-value': getValueISD,
          set: setISD,
          merge: mergeISD,
          remove: removeISD,
          keys: keysISD,
          "keys-list": keysListISD,
          count: countISD,
            'has-key': hasKeyISD,
          _equals: equalsISD,
          _torepr: toreprISD,
          unfreeze: unfreezeISD
        });

        return applyBrand(brandImmutable, obj);

      }

      function makeMutableStringDict(underlyingDict, sealed) {
        // NOTE(joe): getMSD/setMSD etc are internal to
        // makeMutableStringDict because they need to close over underlyingDict

        var getMSD = runtime.makeMethod1(function(_, key) {
          runtime.checkArity(2, arguments, 'get-now');
          runtime.checkString(key);
          var mkey = internalKey(key);
          var val = underlyingDict[mkey];
          if (val === undefined) {
            return runtime.ffi.makeNone();
          } else {
            return runtime.ffi.makeSome(val);
          }
        });

        var getValueMSD = runtime.makeMethod1(function(_, key) {
          runtime.checkArity(2, arguments, "get-value-now");
          runtime.checkString(key);
          var mkey = internalKey(key);
          var val = underlyingDict[mkey];
          if (val === undefined) {
            runtime.ffi.throwMessageException("Key " + key + " not found");
          }
          return val;
        });

        var setMSD = runtime.makeMethod2(function(self, key, val) {
          runtime.checkArity(3, arguments, "set-now");
          if (sealed) {
            runtime.ffi.throwMessageException("Cannot modify sealed string dict");
          }
          runtime.checkString(key);
          runtime.checkPyretVal(val);
          underlyingDict[internalKey(key)] = val;
          return runtime.nothing;
        });

        var removeMSD = runtime.makeMethod1(function(self, key) {
          runtime.checkArity(2, arguments, "remove-now");
          if (sealed) {
            runtime.ffi.throwMessageException("Cannot modify sealed string dict");
          }
          runtime.checkString(key);
          delete underlyingDict[internalKey(key)];
          return runtime.nothing;
        });

        var hasKeyMSD = runtime.makeMethod1(function(_, key) {
          runtime.checkArity(2, arguments, "has-key-now");
          runtime.checkString(key);
          var mkey = internalKey(key);
          if (mkey in underlyingDict) {
            return runtime.makeBoolean(true);
          } else {
            return runtime.makeBoolean(false);
          }
        });

        var keysMSD = runtime.makeMethod0(function(self) {
          runtime.checkArity(1, arguments, "keys-now");
          var keys = Object.keys(underlyingDict);
          return runtime.ffi.makeTreeSet(keys.map(function(mkey) {
            return runtime.makeString(userKey(mkey));
          }));
        });

        var countMSD = runtime.makeMethod0(function(_) {
          runtime.checkArity(1, arguments, "count-now");
          return runtime.makeNumber(Object.keys(underlyingDict).length);
        });

        var toreprMSD = runtime.makeMethod1(function(self, recursiveToRepr) {
          runtime.checkArity(2, arguments, "torepr");
          var keys = Object.keys(underlyingDict);
          var elts = [];
          function combine(elts) {
            //return "[string-dict: " + elts.join(", ") + "]";
            return "[mutable-string-dict: " + elts.join(", ") + "]";
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

        var equalsMSD = runtime.makeMethod2(function(self, other, recursiveEquality) {
          runtime.checkArity(3, arguments, "equals");
          if (!hasBrand(brandMutable, other)) {
            return runtime.ffi.notEqual.app("");
          } else {
            var keys = Object.keys(underlyingDict);
            var otherKeysLength = get(other, "count-now").app();
            function eqElts() {
              if (keys.length === 0) {
                return runtime.ffi.equal;
              } else {
                var thisKey = keys.pop();
                if (!get(other, 'has-key-now').app(userKey(thisKey))) {
                  return runtime.ffi.notEqual.app('');
                } else {
                  return runtime.safeCall(function() {
                    return recursiveEquality.app(underlyingDict[thisKey],
                        get(other, 'get-value-now').app(userKey(thisKey)));
                  },
                  function (result) {
                    if (runtime.ffi.isNotEqual(result)) {
                      return result;
                    } else {
                      return eqElts();
                    }
                  });
                }
              }
            }
            if (keys.length !== otherKeysLength) {
              return runtime.ffi.notEqual.app("");
            } else {
              return eqElts();
            }
          }
        });

        var freezeMSD = runtime.makeMethod0(function(_) {
          runtime.checkArity(1, arguments, 'freeze');
          var dict = Object.create(null);
          for (var mkey in underlyingDict) {
            dict[mkey] = underlyingDict[mkey];
          }
          return makeImmutableStringDict(dict);
        });

        var sealMSD = runtime.makeMethod0(function(_) {
          runtime.checkArity(1, arguments, 'seal');
          return makeMutableStringDict(underlyingDict, true);
        });

        var NYI = runtime.makeMethodFromFun(function(self) {
          runtime.ffi.throwMessageException("Not yet implemented");
        });

        obj = O({
          'get-now': getMSD,
          'get-value-now': getValueMSD,
          'set-now': setMSD,
          'remove-now': removeMSD,
          'keys-now': keysMSD,
          'count-now': countMSD,
          'has-key-now': hasKeyMSD,
          _equals: equalsMSD,
          _torepr: toreprMSD,
          freeze: freezeMSD,
          seal: sealMSD
        });

        return applyBrand(brandMutable, obj);
      }

      function createMutableStringDict() {
        arity(0, arguments, "make-mutable-string-dict");
        var dict = Object.create(null);
        return makeMutableStringDict(dict);
      }

      function createMutableStringDictFromArray(array) {
        arity(1, arguments, "mutable-string-dict");
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
        arity(0, arguments, "make-string-dict");
        var dict = Object.create(null);
        return makeImmutableStringDict(dict);
      }

      function createImmutableStringDictFromArray(array) {
        arity(1, arguments, "string-dict");
        runtime.checkArray(array);
        var dict = Object.create(null);
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
            MutableStringDict: annMutable,
            StringDict: annImmutable
          },
          values: O({
            "make-mutable-string-dict": F(createMutableStringDict),
            "mutable-string-dict": O({
              make: F(createMutableStringDictFromArray)
            }),
            "make-string-dict": F(createImmutableStringDict),
            "string-dict": O({
              make: F(createImmutableStringDictFromArray)
            })
          })
        }),
        "answer": runtime.nothing
      });

    });
  });
});
