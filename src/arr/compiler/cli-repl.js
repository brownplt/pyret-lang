({
  requires: [],
  provides: {},
  nativeRequires: ["repl"],
  theModule: function(runtime, namespace, uri, nativeRepl) {
    function startRepl(pyretRepl, restarter) {
      var runInteractions = runtime.getField(pyretRepl, "run-interaction");
      function runUsingPyretRepl(src, context, filename, callback) {
        return runtime.runThunk(function() {
            return runInteractions.app(src); 
          },
          function(result) {
            if(runtime.isSuccessResult(result)) {
              callback(null, result.result.dict.v.val.result.result.dict.answer);
            }
            else {
              callback(null, result);
            }
          });
      }
      var restartInteractions = runtime.getField(pyretRepl, "restart-interactions");
      return runtime.safeCall(function() {
          return restartInteractions.app("");
        },
        function(ans) {
          var replServer = nativeRepl.start({eval: runUsingPyretRepl});
          replServer.on('exit', () => {
            console.log('Received "exit" event from repl!');
            restarter.resume("repl done");
          });
        });
    }
    return runtime.makeModuleReturn({
      start: runtime.makeFunction(function(pyretRepl, options) {
        return runtime.pauseStack(function(restarter) {
          return runtime.runThunk(function() {
            return startRepl(pyretRepl, restarter);
          },
          function(result) {
            // Ignored intentionally, since replServer just returns
          })
        });
      })
    }, {});

  }
})
