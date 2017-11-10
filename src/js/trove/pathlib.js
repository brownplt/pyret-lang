({
  requires: [],
  nativeRequires: ["path"],
  provides: {
    values: {
      "normalize": "tany",
      "join": "tany",
      "resolve": "tany",
      "relative": "tany",
      "dirname": "tany",
      "extname": "tany",
      "basename": "tany",
      "path-sep": "tany"
    },
    aliases: {},
    datatypes: {}
  },
  theModule: function(RUNTIME, NAMESPACE, uri, path) {
    var values = {
      "normalize": RUNTIME.makeFunction(function(p) {
        RUNTIME.ffi.checkArity(1, arguments, "normalize", false);
        RUNTIME.checkString(p);
        var s = RUNTIME.unwrap(p);
        return RUNTIME.makeString(path.normalize(s));
      }),
      "join": RUNTIME.makeFunction(function(p1, p2) {
        RUNTIME.ffi.checkArity(2, arguments, "join", false);
        RUNTIME.checkString(p1);
        RUNTIME.checkString(p2);
        var s1 = RUNTIME.unwrap(p1);
        var s2 = RUNTIME.unwrap(p2);
        return RUNTIME.makeString(path.join(s1, s2));
      }),
      "resolve": RUNTIME.makeFunction(function(p) {
        RUNTIME.ffi.checkArity(1, arguments, "resolve", false);
        RUNTIME.checkString(p);
        var s = RUNTIME.unwrap(p);
        return RUNTIME.makeString(path.resolve(s));
      }),
      "relative": RUNTIME.makeFunction(function(from, to) {
        RUNTIME.ffi.checkArity(2, arguments, "relative", false);
        RUNTIME.checkString(from);
        RUNTIME.checkString(to);
        var sfrom = RUNTIME.unwrap(from);
        var sto = RUNTIME.unwrap(to);
        return RUNTIME.makeString(path.relative(sfrom, sto));
      }),
      "dirname": RUNTIME.makeFunction(function(p) {
        RUNTIME.ffi.checkArity(1, arguments, "dirname", false);
        RUNTIME.checkString(p);
        var s = RUNTIME.unwrap(p);
        return RUNTIME.makeString(path.dirname(s));
      }),
      "extname": RUNTIME.makeFunction(function(p) {
        RUNTIME.ffi.checkArity(1, arguments, "extname", false);
        RUNTIME.checkString(p);
        var s = RUNTIME.unwrap(p);
        return RUNTIME.makeString(path.extname(s));
      }),
      "basename": RUNTIME.makeFunction(function(p, ext) {
        RUNTIME.ffi.checkArity(2, arguments, "basename", false);
        RUNTIME.checkString(p);
        RUNTIME.checkString(ext);
        var s = RUNTIME.unwrap(p);
        var sext = RUNTIME.unwrap(ext);
        return RUNTIME.makeString(path.basename(s, sext));
      }),
      "path-sep": RUNTIME.makeString(path.sep)
    };
    return RUNTIME.makeModuleReturn(values, {}, {});
  }    
})

