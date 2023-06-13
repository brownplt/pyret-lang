({
    requires: [],
    provides: {
      values: {
        "input": "tany"
      }
    },
    nativeRequires: ["readline"],
    theModule: function(RUNTIME, NAMESPACE, uri, readline) {
      function Input(msg) {
          return RUNTIME.pauseStack(function(restarter) {
            const rl = readline.createInterface({
              input: process.stdin,
              output: process.stdout
            });

            new Promise(resolve => {
              rl.question(msg, input => resolve(input));
            }).then(result => { 
              restarter.resume(RUNTIME.makeString(result));
            }).catch(error => {
              restarter.resume(RUNTIME.makeString(error));
            })
          })
      };
  
      var vals = {
          "input": RUNTIME.makeFunction(function(input) {
            RUNTIME.ffi.checkArity(1, arguments, "input", false);
            RUNTIME.checkString(input);
            return Input(input);
          }, "input")
      };
  
      return RUNTIME.makeModuleReturn(vals, {});
    }
  })
  
  