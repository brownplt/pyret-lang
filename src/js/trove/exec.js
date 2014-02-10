define(["requirejs", "../js/ffi-helpers", "../js/runtime-anf"], function(rjs, ffi, runtimeLib) {

  return function(RUNTIME, NAMESPACE) {
    var F = ffi(RUNTIME, NAMESPACE);

    function exec(jsStr) {
      RUNTIME.checkIf(jsStr, RUNTIME.isString);
      var str = RUNTIME.unwrap(jsStr);
      var oldDefine = rjs.define;
      var name = RUNTIME.unwrap(NAMESPACE.get("gensym").app(RUNTIME.makeString("module")));

      var newRuntime = runtimeLib.makeRuntime({ stdout: function(str) { console.log(str); } });

      function OMGBADIDEA(name, src) {
        var define = function(libs, fun) {
          oldDefine(name, libs, fun);
        }
        eval(src);
      }
      OMGBADIDEA(name, str);
      RUNTIME.pauseStack(function(restarter) {
          require([name], function(a) {
              newRuntime.run(a, newRuntime.namespace, function(r) {
                  console.log("The result was: ", r);
                  var result;
                  if(newRuntime.isSuccessResult(r)) {
                    result = RUNTIME.makeSuccessResult(
                        RUNTIME.makeString(newRuntime.toReprJS(r)));
                  } else {
                    if (result && result.exn && result.exn.exn && result.exn.exn.s && result.exn.stack && result.exn.pyretStack) {
                      console.error(result.exn.exn.s)
                      console.error(result.exn.stack);
                      console.error(result.exn.pyretStack);
                    }
                    result = RUNTIME.makeFailureResult(RUNTIME.makeMessageException("Pyret terminated with an error"));
                  }
                  restarter(result);
                });
            });
        });
    }

    return RUNTIME.makeObject({
      provide: RUNTIME.makeObject({
        exec: RUNTIME.makeFunction(exec)
      }),
      answer: NAMESPACE.get("nothing")
    });
  };
});

