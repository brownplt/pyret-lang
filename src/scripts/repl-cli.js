var r = require("requirejs");
var readlineLib = require("readline");

r.config({
  paths: {
    trove: "../../build/phase1/trove",
    js: "../../build/phase1/js",
    compiler: "../../build/phase1/arr/compiler"
  }
});

r(["q", "js/repl-lib", "js/runtime-anf", "compiler/compile-structs.arr", "./error-ui"], function(Q, replLib, runtimeLib, csLib, errorUI) {
  var runtime = runtimeLib.makeRuntime({});
  var readline = readlineLib.createInterface({
    input: process.stdin,
      output: process.stdout
  });
  var get = runtime.getField;

  runtime.loadModules(runtime.namespace, [csLib], function(cs) {
    var sb = get(cs, "standard-builtins");
    var repl = replLib.create(runtime, runtime.namespace, sb, { name: "repl-cli", dialect: "Pyret"});
    var res = repl.restartInteractions("");
    readline.setPrompt(">", 0);

    /*TODO: see checksUI for check drawing format
     * Iterate over the check results
     * Keep track of the total number of check tests, and total passed
     * Keep track of number of check blocks errored
     * Save strings for each check block with name and results
     * If all results passed say so, otherwise print accordingly */

    readline.on("line", function(cmd) {
      res = res.then(function(_) {
        return repl.run(cmd);
      });

      res.then(function(new_res) {
        if(runtime.isSuccessResult(new_res)) {
          //TODO: turn this into a handler
          console.log(get(new_res.result, "answer"));
        }
        else {
	  var exception = new_res.exn;
	  errorUI.drawError(runtime, exception);
        }
      });
    }).on('close', function() {
      process.exit(0);
    });

    readline.prompt();
  });
})
