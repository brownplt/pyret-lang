/*global define */
/*jslint unparam: true, todo: true, node: true*/

var r = require("requirejs");

r.config({
  paths: {
    trove: "../../build/phase1/trove",
    js: "../../build/phase1/js",
    compiler: "../../build/phase1/arr/compiler"
  }
});

r(["q", "js/repl-lib", "js/runtime-anf", "compiler/compile-structs.arr", "./input-ui", "./output-ui", "./error-ui", "./check-ui"], function(Q, replLib, runtimeLib, csLib, inputLib, outputUI, errorUI, checkUI) {
  var runtime = runtimeLib.makeRuntime({});
  var inputUI = inputLib(runtime);
  var get = runtime.getField;

  runtime.loadModules(runtime.namespace, [csLib], function(cs) {
    var sb = get(cs, "standard-builtins");
    var repl = replLib.create(runtime, runtime.namespace, sb, { name: "repl-cli", dialect: "Pyret"});
    var res = repl.restartInteractions("");

    inputUI.on("command", function(cmd) {
      res = res.then(function(_) {
        return repl.run(cmd, "interactions:" + inputUI.getLineNumber());
      });

      res.then(function(new_res) {
        if(runtime.isSuccessResult(new_res)) {
          //TODO: turn this into a handler
	  console.log("\n" + outputUI.renderValue(runtime, get(new_res.result, "answer")));
	  console.log(checkUI.drawCheckResults(runtime, get(new_res.result, "checks")));
        }
        else {
	  var exception = new_res.exn;
	  console.log("\n" + errorUI.drawError(runtime, exception));
        }

        inputUI.prompt();
      });
    }).on('close', function() {
      process.exit(0);
    });

    inputUI.prompt();
  });
});
