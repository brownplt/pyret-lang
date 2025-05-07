({ 
  requires: [
    { "import-type": "builtin", "name": "s-exp-structs" }
  ],
  nativeRequires: ["s-expression"],
  provides: {
    shorthands: {
      "S-Exp": {
        tag: "name",
        origin: { "import-type": "uri", uri: "builtin://s-exp-structs" },
        name: "S-Exp"
      },
      "pred": ["arrow", ["Any"], "Boolean"]
    },
    values: {
      "s-list": ["arrow", [["List", "S-Exp"]], "S-Exp"],
      "s-num": ["arrow", ["Number"], "S-Exp"],
      "s-str": ["arrow", ["String"], "S-Exp"],
      "s-sym": ["arrow", ["String"], "S-Exp"],
      "is-s-list": "pred",
      "is-s-num": "pred",
      "is-s-str": "pred",
      "is-s-sym": "pred",
      "read-s-exp": ["arrow", ["String"], "S-Exp"]
    },
    types: {
      "S-Exp": "S-Exp"
    }
  },
  theModule: function(RUNTIME, NAMESPACE, uri, sstruct, sexp) {
    var gf = RUNTIME.getField;
    var vals = gf(sstruct, "values");
    var typs = gf(sstruct, "types");
    function readSexp(s) {
      RUNTIME.checkArity(1, arguments, "s-exp", false);
      RUNTIME.checkString(s);
      // Wrap in quotes to satisfy parser for simple atoms like "a"
      var jsVal = new sexp("(" + s + ")");
      var sList = gf(vals, "s-list");
      var sStr = gf(vals, "s-str");
      var sNum = gf(vals, "s-num");
      var sSym = gf(vals, "s-sym");
      var list = function(l) { return sList.app(RUNTIME.ffi.makeList(l)); }
      var str = function(s) { return sStr.app(RUNTIME.makeString(s)); }
      var num = function(nstr) { return sNum.app(RUNTIME.makeNumberFromString(nstr)); }
      var sym = function(x) { return sSym.app(RUNTIME.makeString(x)); }
      function convert(v) {
        if(v instanceof String) {
          return str(String(v));
        } else if (typeof v === "string") {
          if(RUNTIME.string_isnumber(v)) {
            return num(v);
          } else {
            if(v.indexOf("'") !== -1 || v.indexOf('"') !== -1) {
              RUNTIME.ffi.throwMessageException("Invalid s-expression: " + s);
            }
            return sym(v);
          }
        }
        else if(Array.isArray(v)) {
          if(v.length === 0) { return list([]); }
          if(v[0] === "quote") {
            RUNTIME.ffi.throwMessageException("Invalid s-expression: Single quotation mark (') and keyword 'quote' not supported" + s);
          }
          return list(v.map(convert));
        }
        else {
          RUNTIME.ffi.throwMessageException("Invalid s-expression: " + s);
        }
      }
      if(Array.isArray(jsVal) && jsVal.length === 1) {
        return convert(jsVal[0]);
      }
      else {
        RUNTIME.ffi.throwMessageException("Invalid s-expression: " + s);
      }
    }
    var values = {
      "s-list": gf(vals, "s-list"),
      "s-num": gf(vals, "s-num"),
      "s-str": gf(vals, "s-str"),
      "s-sym": gf(vals, "s-sym"),
      "is-s-list": gf(vals, "is-s-list"),
      "is-s-num": gf(vals, "is-s-num"),
      "is-s-str": gf(vals, "is-s-str"),
      "is-s-sym": gf(vals, "is-s-sym"),
      "read-s-exp": RUNTIME.makeFunction(readSexp)
    };
    var types = {
      "S-Exp": typs["S-Exp"]
    };
    return RUNTIME.makeModuleReturn(values, types);
  }
})
