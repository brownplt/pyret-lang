({
  requires:
    [
      { "import-type": "builtin", name: "valueskeleton" }
    ],
  nativeRequires: [],
  provides: {
    shorthands: {
      "dOfAB": ["tyapp", ["local", "StringDict"], [["tid", "a"], ["tid", "b"]]],
      "Equality": { tag: "name",
                    origin: { "import-type": "uri", uri: "builtin://equality" },
                    name: "EqualityResult" },
      "VS": { tag: "name",
                    origin: { "import-type": "uri", uri: "builtin://valueskeleton" },
                    name: "ValueSkeleton" },
    },
    values: {
      "make-dict": ["forall", ["a", "b"], ["arrow", [], "dOfAB"]],
    },
    aliases: {
      "Dict":  {
        tag: "name",
        origin: {"import-type": "$ELF"},
        name: "Dict"
      }
    },
    datatypes: {
      "Dict": ["data", "Dict", ["a", "b"], [], {
        "get-now": ["arrow", [["tid", "a"]], ["Option", ["tid", "b"]]],
        "get-value-now": ["arrow", [["tid", "a"]], ["tid", "b"]],
        "set-now": ["arrow", [["tid", "a"], ["tid", "b"]], "dOfAB"],
        "_equals": ["arrow", ["dOfAB", ["arrow", ["Any", "Any"], "Equality"]], "Equality"],
        "_output":  ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
      }],
    }
  },
  theModule: function(runtime, namespace, uri, VSlib){
    var O = runtime.makeObject;
    var F = runtime.makeFunction;
    var arity = runtime.checkArity;
    var get = runtime.getField;
    var VS = get(VSlib, "values");

    var brandMutable = runtime.namedBrander("dict", ["dict: mutable dict brander"]);
    var annMutable = runtime.makeBranderAnn(brandMutable, "Dict");
    var checkMSD = function(v) { runtime._checkAnn(["dict"], annMutable, v); };

    function applyBrand(brand, val) {
      return get(brand, "brand").app(val);
    }
    function hasBrand(brand, val) {
      return get(brand, "test").app(val);
    }

    function createDict() {
      const contents = new Map();
      var thePyretValue = null;
      function setNow(key, val) {
        if(runtime.isRoughnum(key) || runtime.isFunction(key) || runtime.isMethod(key)) {
          runtime.ffi.throwMessageException("Cannot use roughnum, function, or method as a key in a map");
        }
        contents.set(key, val);
        return thePyretValue;
      }
      function getNow(key) {
        if(contents.has(key)) {
          return runtime.ffi.makeSome(contents.get(key));
        }
        else {
          return runtime.ffi.makeNone();
        }
      }
      function getValueNow(key) {
        if(contents.has(key)) {
          return contents.get(key);
        }
        else {
          runtime.ffi.throwMessageException("Key not found");
        }
      }
      thePyretValue = applyBrand(brandMutable, O({
        "set-now": F(setNow),
        "get-now": F(getNow),
        "get-value-now": F(getValueNow)
      }));
      return thePyretValue;
    }

    var vals = {
      "make-dict": F(createDict, "make-mutable-string-dict"),
    };
    var types = {
      Dict: annMutable
    };
    var internal = {
      checkMSD: checkMSD
    };
    return runtime.makeModuleReturn(vals, types, internal);
  }
})
