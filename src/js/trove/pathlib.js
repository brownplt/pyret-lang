define(["js/runtime-util", "path", "js/ffi-helpers"], function(util, path, ffiLib) {

  return util.memoModule("pathlib", function(RUNTIME, NAMESPACE) {
    return RUNTIME.loadJSModules(NAMESPACE, [ffiLib], function(ffi) {
      return RUNTIME.makeObject({
          provide: RUNTIME.makeObject({
              "normalize": RUNTIME.makeFunction(function(p) {
                  ffi.checkArity(1, arguments, "normalize");
                  RUNTIME.checkString(p);
                  var s = RUNTIME.unwrap(p);
                  return RUNTIME.makeString(path.normalize(s));
                }),
              "join": RUNTIME.makeFunction(function(p1, p2) {
                  ffi.checkArity(2, arguments, "join");
                  RUNTIME.checkString(p1);
                  RUNTIME.checkString(p2);
                  var s1 = RUNTIME.unwrap(p1);
                  var s2 = RUNTIME.unwrap(p2);
                  return RUNTIME.makeString(path.join(s1, s2));
                }),
              "resolve": RUNTIME.makeFunction(function(p) {
                  ffi.checkArity(1, arguments, "resolve");
                  RUNTIME.checkString(p);
                  var s = RUNTIME.unwrap(p);
                  return RUNTIME.makeString(path.resolve(s));
                }),
              "relative": RUNTIME.makeFunction(function(from, to) {
                  ffi.checkArity(2, arguments, "relative");
                  RUNTIME.checkString(from);
                  RUNTIME.checkString(to);
                  var sfrom = RUNTIME.unwrap(from);
                  var sto = RUNTIME.unwrap(to);
                  return RUNTIME.makeString(path.relative(sfrom, sto));
                })
            }),
          answer: NAMESPACE.get("nothing")
        });
    });
  });    
});

