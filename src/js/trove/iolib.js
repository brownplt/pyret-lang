({
  requires: [],
  provides: {
    values: {
      // TODO: is tany correct?
      "prompt": "tany",
      // TODO: is tany correct?
      "wrap-input-test": "tany"
    }
  },
  nativeRequires: ["readline"],
  theModule: function(RUNTIME, NAMESPACE, uri, readline) {
    function Input(simulateIO = () => {}) {
        return RUNTIME.pauseStack(function(restarter) {
          const rl = readline.createInterface({ input: RUNTIME.stdin });
          
          // input does not need to display anything
          questionPromise = new Promise(resolve => rl.question('', input => resolve(input)));
          
          simulateIO(rl)
          
          questionPromise
            .then(result => restarter.resume(RUNTIME.makeString(result)))
            .catch(error => {
              // TODO: write a test for this
              restarter.resume(RUNTIME.makeString(error));
            })
            .finally(x => rl.close())
        })
    };

    function Prompt(msg) {
      // TODO: call this correctly
      NAMESPACE.bindings.print.app(msg + "\n");
      return Input();
    };

    function wrapInputTest(testInput) {
      return Input(simulateIO = (rl) => rl.write(testInput + '\n'));
    }

    var vals = {
        "input": RUNTIME.makeFunction(function() {
          RUNTIME.ffi.checkArity(0, arguments, "input", false);
          return Input();
        }, "input"),
        "prompt": RUNTIME.makeFunction(function(input1) {
          RUNTIME.ffi.checkArity(1, arguments, "prompt", false);
          RUNTIME.checkString(input1);
          return Prompt(input1);
        }, "prompt"),
        "wrap-input-test": RUNTIME.makeFunction(function(input1) {
          RUNTIME.ffi.checkArity(1, arguments, "wrap-input-test", false);
          RUNTIME.checkString(input1);
          return wrapInputTest(input1);
        }, "wrap-input-test")
    };

    return RUNTIME.makeModuleReturn(vals, {});
  }
})

