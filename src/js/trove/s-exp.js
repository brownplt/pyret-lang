define(["js/runtime-util", "s-expression", "trove/s-exp-structs"], function(util, sexp, sexpStruct) {
  return util.memoModule("s-exp", function(RUNTIME, NAMESPACE) {
    var gf = RUNTIME.getField;
    return RUNTIME.loadModulesNew(NAMESPACE, [sexpStruct], function(sstruct) {
      var vals = gf(sstruct, "values");
      var typs = gf(sstruct, "types");
      function readSexp(s) {
        RUNTIME.checkString(s);
        RUNTIME.checkArity(1, arguments);
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
          if(Array.isArray(v)) {
            if(v.length === 0) { return list([]); }
            if(v[0] === "quote") {
              RUNTIME.ffi.throwMessageException("Invalid s-expression: Single quotation mark (') and keyword 'quote' not supported" + s);
            }
            return list(v.map(convert));
          }
          else if(RUNTIME.string_isnumber(v)) {
            return num(v);
          }
          else if(typeof v === "string") {
            if(v.length > 1) {
              var first = v[0];
              var last = v[v.length - 1];
              if (first === "\"" || last === "\"") {
                if (!(first === "\"" && last === "\"")) {
                  RUNTIME.ffi.throwMessageException("Invalid s-expression: String without matching double quotes " + v);

                }
                else {
                  return str(v.slice(1, v.length - 1));
                }
              }
            }
            if(v.indexOf("'") !== -1) {
              RUNTIME.ffi.throwMessageException("Invalid s-expression: Single quotation mark (') and keyword 'quote' not supported " + v);
            }
            else {
              return sym(v);
            }
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
      return RUNTIME.makeObject({
        answer: RUNTIME.nothing,
        "provide-plus-types": RUNTIME.makeObject({
          "values": RUNTIME.makeObject({
            "s-list": gf(vals, "s-list"),
            "s-num": gf(vals, "s-num"),
            "s-str": gf(vals, "s-str"),
            "s-sym": gf(vals, "s-sym"),
            "is-s-list": gf(vals, "is-s-list"),
            "is-s-num": gf(vals, "is-s-num"),
            "is-s-str": gf(vals, "is-s-str"),
            "is-s-sym": gf(vals, "is-s-sym"),
            "read-s-exp": RUNTIME.makeFunction(readSexp)
          }),
          "types": {
            "S-Exp": typs["S-Exp"]
          }
        })
      });
    });
  });
});
