define(["builtin-libs/list"], function(L) {

  return function(RUNTIME, NAMESPACE) {
    L = RUNTIME.getField(L(RUNTIME, NAMESPACE), "provide");
    function makeList(arr) {
      var lst = RUNTIME.getField(L, "empty");
      for(var i = arr.length - 1; i >= 0; i--) {
        lst = RUNTIME.getField(L, "link").app(arr[i], lst); 
      }
      return lst;
    }

    return RUNTIME.makeObject({
      provide: RUNTIME.makeObject({
        "command-line-arguments": RUNTIME.makeFunction(function() {
          return makeList(process.argv);
        }),
      }),
      answer: NAMESPACE.get("nothing")
    });
  };
});

