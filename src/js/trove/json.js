define(["js/runtime-util"], function(util) {
  return util.definePyretModule("json",
    [],
    [
      util.modBuiltin("string-dict"),
      util.modBuiltin("json-structs")
    ],
    {},
    function(RUNTIME, NAMESPACE, sdict, jstruct) {
      console.error("sdict: ", sdict);
      console.error("jstruct: ", jstruct);
      var gf = RUNTIME.getField;
      var vals = gf(jstruct, "values");
      var typs = gf(jstruct, "types");
      var sdvals = gf(sdict, "values");
      function readJSON(s) {
        RUNTIME.checkString(s);
        RUNTIME.checkArity(1, arguments);
        var jsVal = JSON.parse(s);
        var jObj = gf(vals, "j-obj");
        var jArr = gf(vals, "j-arr");
        var jStr = gf(vals, "j-str");
        var jNum = gf(vals, "j-num");
        var jBool = gf(vals, "j-bool");
        var jNull = gf(vals, "j-null");
        var sdMake = gf(gf(sdvals, "string-dict"), "make");
        var str = function(s) { return jStr.app(RUNTIME.makeString(s)); }
        var num = function(n) { return jNum.app(RUNTIME.makeNumber(n)); }
        var bool = function(b) { return jBool.app(RUNTIME.makeBoolean(b ? RUNTIME.pyretTrue : RUNTIME.pyretFalse)); }
        var arr = function(a) { return jArr.app(RUNTIME.ffi.makeList(a)) }
        var nul = function(v) { return jNull; }
        function convert(v) {
          if(v === null) {
            return nul(v);
          } else if(typeof v === "string") {
            return str(v);
          } else if (typeof v === "number") {
            return num(v);
          } else if (typeof v === "boolean") {
            return bool(v);
          } else if(Array.isArray(v)) {
            return arr(v.map(convert));
          } else if(typeof v === "object") {
            var a = [];
            for (var key in v) {
              // Just to make sure no new-fangled Symbols made it through.
              if(typeof key === "string") {
                a.push(key);
                a.push(convert(v[key]));
              } else {
                RUNTIME.ffi.throwMessageException("Invalid key " + v + " in JSON: " + s);
              }
            }
            return jObj.app(sdMake.app(a));
          } else {
            RUNTIME.ffi.throwMessageException("Invalid JSON: " + s);
          }
        }
        return convert(jsVal);
      }
      return RUNTIME.makeObject({
        answer: RUNTIME.nothing,
        "provide-plus-types": RUNTIME.makeObject({
          "values": RUNTIME.makeObject({
            "j-obj": gf(vals, "j-obj"),
            "j-arr": gf(vals, "j-arr"),  
            "j-num": gf(vals, "j-num"),
            "j-str": gf(vals, "j-str"),
            "j-bool": gf(vals, "j-bool"),
            "j-null" : gf(vals, "j-null"),
            "is-j-obj": gf(vals, "is-j-obj"),
            "is-j-arr": gf(vals, "is-j-arr"),
            "is-j-num": gf(vals, "is-j-num"),
            "is-j-str": gf(vals, "is-j-str"),
            "is-j-bool": gf(vals, "is-j-bool"),
            "is-j-null": gf(vals, "is-j-null"),
            "read-json": RUNTIME.makeFunction(readJSON),
            "tojson": gf(vals, "tojson")
          }),
          "types": {
            "JSON": typs["JSON"]
          }
        })
      });
    });
});
