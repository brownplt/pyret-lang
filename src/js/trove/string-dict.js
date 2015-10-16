define(["js/runtime-util", "js/type-util", "js/namespace", "js/ffi-helpers", "trove/valueskeleton"], function(util, t, Namespace, ffi, valueskeleton) {
  var sdOfA = t.tyapp(t.localType("StringDict"), [t.tyvar("a")]);
  var msdOfA = t.tyapp(t.localType("MutableStringDict"), [t.tyvar("a")]);
  return util.definePyretModule(
    "string-dict",
    [],
    {
      values:
      {
        "make-string-dict": t.forall(["a"], sdOfA),
        "string-dict":
          t.record({
            "make":
              // NOTE(joe): any for RawArray instantiation until we have tuples
              t.forall(["a"],
                t.arrow([t.tyapp(t.builtinName("RawArray"), [t.any])], sdOfA))
          }),
        "string-dict-of":
          t.forall(["a"],
            t.arrow(
              [
                t.tyapp(t.libName("lists", "List"), [t.builtinName("String")]),
                t.tyvar("a")
              ],
              sdOfA)),
        "make-mutable-string-dict": t.forall(["a"], t.arrow([], msdOfA)),
        "mutable-string-dict":
          t.record({
            "make":
              t.forall(["a"], t.arrow([t.tyapp(t.builtinName("RawArray"), [t.any])], msdOfA))
          })
      },
      aliases: {},
      datatypes: {
        StringDict: t.dataType(
          "StringDict",
          ["a"],
          [],
          {
            "get": t.arrow([t.string], t.tyapp(t.libName("option", "Option"), [t.tyvar("a")])),
            "get-value": t.arrow([t.string], t.tyvar("a")),
            "set": t.arrow([t.string, t.tyvar("a")], sdOfA),
            "merge": t.arrow([sdOfA], sdOfA),
            "remove": t.arrow([t.string], sdOfA),
            "keys": t.arrow([], t.tyapp(t.libName("sets", "TreeSet"), [t.string])),
            "keys-list": t.arrow([], t.tyapp(t.libName("lists", "List"), [t.string])),
            "count": t.arrow([], t.number),
            "has-key": t.arrow([t.string], t.boolean),
            "unfreeze": t.arrow([], msdOfA),
            // TODO(joe): _output and _equals
          }
        ),
        MutableStringDict: t.dataType(
          "MutableStringDict",
          ["a"],
          [],
          {
            "get-now": t.arrow([t.string], t.tyapp(t.libName("option", "Option"), [t.tyvar("a")])),
            "get-value-now": t.arrow([t.string], t.tyvar("a")),
            "set-now": t.arrow([t.string, t.tyvar("a")], t.nothing),
            "remove-now": t.arrow([t.string], t.nothing),
            "keys-now": t.arrow([], t.tyapp(t.libName("sets", "TreeSet"), [t.string])),
            "keys-list-now": t.arrow([], t.tyapp(t.libName("lists", "List"), [t.string])),
            "count-now": t.arrow([], t.number),
            "has-key-now": t.arrow([t.string], t.boolean),
            "freeze": t.arrow([], sdOfA),
            "seal": t.arrow([], msdOfA),
            // TODO(joe): _output and _equals
          }
        )
      }
    },
    function(runtime, namespace /* no pyret dependencies */) {
    return runtime.loadJSModules(namespace, [ffi], function(F) {
    return runtime.loadModulesNew(namespace, [valueskeleton], function(VSlib) {

      var O = runtime.makeObject;
      var F = runtime.makeFunction;
      var arity = runtime.checkArity;
      var get = runtime.getField;

      var VS = get(VSlib, "values");

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
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['get'], 2, $a); }
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
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['get-value'], 2, $a); }
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
          if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['set'], 3, $a); }
          runtime.checkString(key);
          runtime.checkPyretVal(val);
          var newObj = Object.create(underlyingDict);
          var mkey = internalKey(key);
          newObj[mkey] = val;
          return makeImmutableStringDict(newObj);
        });

        var mergeISD = runtime.makeMethod1(function(self, other) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["merge"], 2, $a); }
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
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['remove'], 2, $a); }
          runtime.checkString(key);
          var newObj = Object.create(underlyingDict);
          var mkey = internalKey(key);
          newObj[mkey] = undefined;
          return makeImmutableStringDict(newObj);
        });

        var hasKeyISD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['has-key'], 2, $a); }
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
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['keys'], 1, $a); }
          var keys = getAllKeys();
          return runtime.ffi.makeTreeSet(keys.map(function(mkey) {
            return runtime.makeString(userKey(mkey));
          }));
        });

        var keysListISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['keys-list'], 1, $a); }
          var keys = getAllKeys();
          return runtime.ffi.makeList(keys.map(function(mkey) {
            return runtime.makeString(userKey(mkey));
          }));
        });

        var countISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['count'], 1, $a); }
          var num = 0;
          for (var key in underlyingDict) {
            if (underlyingDict[key] !== undefined) {
              num++;
            }
          }
          return runtime.makeNumber(num);
        });

        var outputISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['_output'], 1, $a); }
          var elts = [];
          var keys = getAllKeys();
          var vsValue = get(VS, "vs-value");
          for (var i = 0; i < keys.length; i++) {
            elts.push(vsValue.app(userKey(keys[i])));
            elts.push(vsValue.app(underlyingDict[keys[i]]));
          }
          return get(VS, "vs-collection").app(
            runtime.makeString("string-dict"),
            runtime.ffi.makeList(elts));
        });

        var equalsISD = runtime.makeMethod2(function(self, other, recursiveEquality) {
          if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['equals'], 3, $a); }
          if (!hasBrand(brandImmutable, other)) {
            return runtime.ffi.notEqual.app('', self, other);
          } else {
            var keys = getAllKeys();
            var otherKeysLength = get(other, 'count').app();
            function equalsHelp() {
              if (keys.length === 0) {
                return runtime.ffi.equal;
              } else {
                var thisKey = keys.pop();
                if (!get(other, 'has-key').app(userKey(thisKey))) {
                  return runtime.ffi.notEqual.app('', self, other);
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
              return runtime.ffi.notEqual.app('', self, other);
            } else {
              return equalsHelp();
            }
          }
        });

        var unfreezeISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['unfreeze'], 1, $a); }
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
          _output: outputISD,
          unfreeze: unfreezeISD
        });

        return applyBrand(brandImmutable, obj);

      }

      function makeMutableStringDict(underlyingDict, sealed) {
        // NOTE(joe): getMSD/setMSD etc are internal to
        // makeMutableStringDict because they need to close over underlyingDict

        var getMSD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['get-now'], 2, $a); }
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
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["get-value-now"], 2, $a); }
          runtime.checkString(key);
          var mkey = internalKey(key);
          var val = underlyingDict[mkey];
          if (val === undefined) {
            runtime.ffi.throwMessageException("Key " + key + " not found");
          }
          return val;
        });

        var setMSD = runtime.makeMethod2(function(self, key, val) {
          if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["set-now"], 3, $a); }
          if (sealed) {
            runtime.ffi.throwMessageException("Cannot modify sealed string dict");
          }
          runtime.checkString(key);
          runtime.checkPyretVal(val);
          underlyingDict[internalKey(key)] = val;
          return runtime.nothing;
        });

        var removeMSD = runtime.makeMethod1(function(self, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["remove-now"], 2, $a); }
          if (sealed) {
            runtime.ffi.throwMessageException("Cannot modify sealed string dict");
          }
          runtime.checkString(key);
          delete underlyingDict[internalKey(key)];
          return runtime.nothing;
        });

        var hasKeyMSD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["has-key-now"], 2, $a); }
          runtime.checkString(key);
          var mkey = internalKey(key);
          if (mkey in underlyingDict) {
            return runtime.makeBoolean(true);
          } else {
            return runtime.makeBoolean(false);
          }
        });

        var keysMSD = runtime.makeMethod0(function(self) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["keys-now"], 1, $a); }
          var keys = Object.keys(underlyingDict);
          return runtime.ffi.makeTreeSet(keys.map(function(mkey) {
            return runtime.makeString(userKey(mkey));
          }));
        });

        var keysListMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['keys-list-now'], 1, $a); }
          var keys = Object.keys(underlyingDict);
          return runtime.ffi.makeList(keys.map(function(mkey) {
            return runtime.makeString(userKey(mkey));
          }));
        });

        var countMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["count-now"], 1, $a); }
          return runtime.makeNumber(Object.keys(underlyingDict).length);
        });

        var toreprMSD = runtime.makeMethod1(function(self, recursiveToRepr) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["torepr"], 2, $a); }
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
        var outputMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['_output'], 1, $a); }
          var elts = [];
          var keys = Object.keys(underlyingDict);
          var vsValue = get(VS, "vs-value");
          for (var i = 0; i < keys.length; i++) {
            elts.push(vsValue.app(userKey(keys[i])));
            elts.push(vsValue.app(underlyingDict[keys[i]]));
          }
          return get(VS, "vs-collection").app(
            runtime.makeString("mutable-string-dict"),
            runtime.ffi.makeList(elts));
        });

        var equalsMSD = runtime.makeMethod2(function(self, other, recursiveEquality) {
          if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["equals"], 3, $a); }
          if (!hasBrand(brandMutable, other)) {
            return runtime.ffi.notEqual.app("", self, other);
          } else {
            var keys = Object.keys(underlyingDict);
            var otherKeysLength = get(other, "count-now").app();
            function eqElts() {
              if (keys.length === 0) {
                return runtime.ffi.equal;
              } else {
                var thisKey = keys.pop();
                if (!get(other, 'has-key-now').app(userKey(thisKey))) {
                  return runtime.ffi.notEqual.app('', self, other);
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
              return runtime.ffi.notEqual.app("", self, other);
            } else {
              return eqElts();
            }
          }
        });

        var freezeMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['freeze'], 1, $a); }
          var dict = Object.create(null);
          for (var mkey in underlyingDict) {
            dict[mkey] = underlyingDict[mkey];
          }
          return makeImmutableStringDict(dict);
        });

        var sealMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['seal'], 1, $a); }
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
          'keys-list-now': keysListMSD,
          'count-now': countMSD,
          'has-key-now': hasKeyMSD,
          _equals: equalsMSD,
          _output: outputMSD,
          freeze: freezeMSD,
          seal: sealMSD
        });

        return applyBrand(brandMutable, obj);
      }

      function isMutableStringDict(obj) {
        arity(1, arguments, "is-mutable-string-dict")
        return runtime.makeBoolean(hasBrand(brandMutable, obj))
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

      function isImmutableStringDict(obj) {
        arity(1, arguments, "is-immutable-string-dict")
        return runtime.makeBoolean(hasBrand(brandImmutable, obj))
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
          var ikey = internalKey(key);
          if (dict[ikey] !== undefined) {
            runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + key);
          }
          dict[internalKey(key)] = val;
        }
        return makeImmutableStringDict(dict);
      }

      function createConstImmutableStringDict(names, val) {
        arity(2, arguments, "string-dict-of");
        runtime.checkList(names);
        var arr = runtime.ffi.toArray(names);
        var dict = Object.create(null);
        arr.forEach(function(k) {
          dict[internalKey(k)] = val;
        });
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
            "is-mutable-string-dict": F(isMutableStringDict),
            "make-string-dict": F(createImmutableStringDict),
            "string-dict": O({
              make: F(createImmutableStringDictFromArray)
            }),
            "string-dict-of": F(createConstImmutableStringDict),
            "is-string-dict": F(isImmutableStringDict)
          })
        }),
        "answer": runtime.nothing
      });

    });
    });
  });
});
