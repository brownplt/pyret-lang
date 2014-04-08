define(["requirejs", "js/secure-loader", "js/ffi-helpers", "js/runtime-anf", "trove/checker"], function(rjs, loader, ffi, runtimeLib, checkerLib) {

  return function(RUNTIME, NAMESPACE) {
    var F = ffi(RUNTIME, NAMESPACE);

    function execWithDir(jsStr, modnameP, loaddirP, checkAllP, params) {
      RUNTIME.checkIf(jsStr, RUNTIME.isString);
      RUNTIME.checkIf(modnameP, RUNTIME.isString);
      RUNTIME.checkIf(loaddirP, RUNTIME.isString);
      RUNTIME.checkIf(checkAllP, RUNTIME.isBoolean);
      var str = RUNTIME.unwrap(jsStr);
      var modname = RUNTIME.unwrap(modnameP);
      var loaddir = RUNTIME.unwrap(loaddirP);
      var checkAll = RUNTIME.unwrap(checkAllP);
      var argsArray = F.toArray(params).map(RUNTIME.unwrap);
      return exec(str, modname, loaddir, checkAll, argsArray);
    }

    function exec(str, modname, loaddir, checkAll, args) {
      var name = RUNTIME.unwrap(NAMESPACE.get("gensym").app(RUNTIME.makeString("module")));
      rjs.config({ baseUrl: loaddir });

      var newRuntime = runtimeLib.makeRuntime({ 
        stdout: function(str) { process.stdout.write(str); },
        stderr: function(str) { process.stderr.write(str); }
      });
      var fnew = ffi(newRuntime, newRuntime.namespace);
      newRuntime.setParam("command-line-arguments", args);

      var checker = newRuntime.getField(checkerLib(newRuntime, newRuntime.namespace), "provide");
      var currentChecker = newRuntime.getField(checker, "make-check-context").app(newRuntime.makeString(modname), newRuntime.makeBoolean(checkAll));
      newRuntime.setParam("current-checker", currentChecker);

      function makeResult(execRt, callingRt, r) {
        if(execRt.isSuccessResult(r)) {
          var pyretResult = r.result;
          return callingRt.makeObject({
              "success": callingRt.makeBoolean(true),
              "render-check-results": callingRt.makeFunction(function() {
                var toCall = execRt.getField(checker, "render-check-results");
                var checks = execRt.getField(pyretResult, "checks");
                callingRt.pauseStack(function(restarter) {
                    execRt.run(function(rt, ns) {
                        return toCall.app(checks);
                      }, execRt.namespace, {sync: true},
                      function(printedCheckResult) {
                        if(execRt.isSuccessResult(printedCheckResult)) {
                          if(execRt.isString(printedCheckResult.result)) {
                            restarter.resume(callingRt.makeString(execRt.unwrap(printedCheckResult.result)));
                          }
                        }
                        else if(execRt.isFailureResult(printedCheckResult)) {
                          console.error(printedCheckResult);
                          console.error(printedCheckResult.exn);
                          restarter.resume(callingRt.makeString("There was an exception while formatting the check results"));
                        }
                      });
                  });
              })
            });
        }
        else if(execRt.isFailureResult(r)) {
          return callingRt.makeObject({
              "success": callingRt.makeBoolean(false),
              "failure": r.exn.exn,
              "render-error-message": callingRt.makeFunction(function() {
                callingRt.pauseStack(function(restarter) {
                  execRt.runThunk(function() {
                    return execRt.getField(r.exn.exn, "tostring").app()
                  }, function(v) { return restarter.resume(v.result); })
                });
              })
            });
        }
      }

      loader.goodIdea(name, str);

      /* pauseStack clears the stack of this runtime, and closes over it
         in the restarter continuation, passing it in here.  Calling
         restarter(value) will restart RUNTIME's current stack as if value was
         the returned value from this function call (the call to exec()) */

      RUNTIME.pauseStack(function(restarter) {

          /* This require instantiates the new (anonymous) module we're
             evaluating.  The instantiated module is a function that takes a
             runtime and a namespace and starts the eval'd program: it is
             a suitable argument to run() */

          require([name], function(a) {

              /* run() starts the anonymous module's evaluation on a new stack
                 (created by newRuntime).  Once the evaluated program finishes
                 (if it ever does), the continuation is called with r as either
                 a Success or Failure Result from newRuntime. */

              newRuntime.run(a, newRuntime.namespace, {sync: true}, function(r) {

                  /* makeResult handles turning values from the new runtime into values that
                     the calling runtime understands (since they don't share
                     the same instantiation of all the Pyret constructors like PObject, or the
                     same brands */

                  var wrappedResult = makeResult(newRuntime, RUNTIME, r);

                  /* This restarts the calling stack with the new value, which
                     used constructors from the calling runtime.  From the point of view of the
                     caller, wrappedResult is the return value of the call to exec() */
                  restarter.resume(wrappedResult);
                });
            });
        });
    }

    return RUNTIME.makeObject({
      provide: RUNTIME.makeObject({
        exec: RUNTIME.makeFunction(execWithDir)
      }),
      answer: NAMESPACE.get("nothing")
    });
  };
});

