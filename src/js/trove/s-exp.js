define(["js/runtime-util", "fs", "js/ffi-helpers", "s-expression"], function(util, fs, ffiLib, sexp) {
  return util.memoModule("s-exp", function(RUNTIME, NAMESPACE) {
    function readSexp(s) {
      RUNTIME.checkString(s);
      RUNTIME.checkArity(1, arguments);
      // Wrap in quotes to satisfy parser for simple atoms like "a"
      var jsVal = new sexp("(" + s + ")");
      var str = RUNTIME.makeString;
      function convert(v) {
        if(Array.isArray(v)) {
          if(v.length === 0) { return RUNTIME.ffi.makeList([]); }
          if(v[0] === "quote") {
            RUNTIME.ffi.throwMessageException("Invalid s-expression: Single quotation mark (') and keyword 'quote' not supported" + s);
          }
          return RUNTIME.ffi.makeList(v.map(convert));  
        }
        else if(RUNTIME.string_isnumber(v)) {
          return RUNTIME.makeNumberFromString(v);
        }
        else if(typeof v === "string") {
          if(v.indexOf("'") !== -1) {
            RUNTIME.ffi.throwMessageException("Invalid s-expression: Single quotation mark (') and keyword 'quote' not supported" + s);
          }
          if(v.length > 0 && v[0] === "\"") {
            return RUNTIME.ffi.makeList([str("string"), str(v.slice(1, v.length - 1))]);
          }
          else {
            return str(v);
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
      provide: RUNTIME.makeObject({
        "read-sexp": RUNTIME.makeFunction(readSexp)
      })
    });
  });
});
