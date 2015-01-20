/*global define */
/*jslint unparam: true, node: true*/

var r = require("requirejs");

r.config({
  paths: {
    trove: "../../build/phase1/trove",
    js: "../../build/phase1/js",
    compiler: "../../build/phase1/arr/compiler"
  }
});

r(["q", "js/repl-lib", "js/runtime-anf", "compiler/compile-structs.arr", "./input-ui", "./output-ui", "./error-ui", "./check-ui"], function(Q, replLib, runtimeLib, csLib, inputLib, outputLib, errorUI, checkUI) {
  var runtime = runtimeLib.makeRuntime({});
  var inputUI = inputLib(runtime);
  var outputUI = outputLib('default');
  var renderer = new outputUI.Renderer();
  var get = runtime.getField;

  runtime.loadModules(runtime.namespace, [csLib], function(cs) {
    var sb = get(cs, "standard-builtins");
    var repl = replLib.create(runtime, runtime.namespace, sb, { name: "repl-cli", dialect: "Pyret"});
    var res = repl.restartInteractions("");

    inputUI.on("command", function(cmd) {
      inputUI.setListen(false);

      res = res.then(function(_) {
        return repl.run(cmd, "interactions:" + inputUI.getInteractionsNumber());
      });

      //Note(ben), executes when the previous res is "ready"
      res.then(function(new_res) {
        if(runtime.isSuccessResult(new_res)) {
	  renderer.drawAndPrintAnswer(runtime, get(new_res.result, "answer"));
	  checkUI.drawAndPrintCheckResults(runtime, get(new_res.result, "checks"));
        }
        else {
	  var exception = new_res.exn;
	  errorUI.drawAndPrintError(runtime, exception);
        }

        inputUI.prompt();
      });
    }).on('close', function() {
      process.exit(0);
    });

    inputUI.prompt();
  });
});
