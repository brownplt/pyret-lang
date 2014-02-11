define(["requirejs", "../js/ffi-helpers", "../js/runtime-anf"], function(rjs, ffi, runtimeLib) {

  return function(RUNTIME, NAMESPACE) {
    var F = ffi(RUNTIME, NAMESPACE);

    function exec(jsStr) {
      RUNTIME.checkIf(jsStr, RUNTIME.isString);
      var str = RUNTIME.unwrap(jsStr);
      var oldDefine = rjs.define;
      var name = RUNTIME.unwrap(NAMESPACE.get("gensym").app(RUNTIME.makeString("module")));

      var newRuntime = runtimeLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });

      function OMGBADIDEA(name, src) {
        var define = function(libs, fun) {
          oldDefine(name, libs, fun);
        }
        eval(src);
      }
      OMGBADIDEA(name, str);
      RUNTIME.pauseStack(function(restarter) {
          require([name], function(a) {
              newRuntime.run(a, newRuntime.namespace, {sync: true}, function(r) {
                  var result;
                  if(newRuntime.isSuccessResult(r)) {
                    result = RUNTIME.makeSuccessResult(
                        RUNTIME.makeString(newRuntime.toReprJS(r)));
                  } else {
                    if (r && r.exn && r.exn.exn && r.exn.exn.s && r.exn.stack && r.exn.pyretStack) {
                      console.error(r.exn.exn.s)
                      console.error(r.exn.stack);
                      console.error(JSON.stringify(r.exn.pyretStack, null, "  "));
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

