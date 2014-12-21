var r = require("requirejs");
var readline = require('readline');

r.config({
  paths: {
    trove: "../../build/phase1/trove",
    js: "../../build/phase1/js",
    compiler: "../../build/phase1/arr/compiler"
  }
});

r(["q", "js/ffi-helpers", "js/repl-lib", "js/runtime-anf", "compiler/compile-structs.arr"], function(Q, ffiLib, repl, runtime, compile_lib) {
  var rt = runtime.makeRuntime({});
  var rl = readline.createInterface({
    input: process.stdin,
      output: process.stdout
  });
  var ffi = ffiLib(rt, rt.namespace);
  var gf = rt.getField;
  var cases = ffi.cases;

  rt.loadModules(rt.namespace, [compile_lib], function(cs) {
    var sb = gf(cs, "standard-builtins");
    var rp = repl.create(rt, rt.namespace, sb, { name: "repl-cli", dialect: "Pyret"});
    var res = rp.restartInteractions("");
    rl.setPrompt(">", 0); //make this a constant

    function drawCompileErrors(e) {
      //TODO: update draw functions to print relevant info
      function drawUnboundId(e) {
        console.log(e);
      }

      function drawDefault(e) {
        return function() {
          console.log(e);
        }
      }

      //TODO: add more draw functions

      function drawCompileError(e) {
        cases(gf(cs, "CompileError"), "CompileError", e, {
          "unbound-id": drawUnboundId,
          "else": drawDefault(e)
        });
      }

      e.forEach(drawCompileError);
    }

    //TODO: see errorsUI for other error draws

    /*TODO: see checksUI for check drawing format
     * Iterate over the check results
     * Keep track of the total number of check tests, and total passed
     * Keep track of number of check blocks errored
     * Save strings for each check block with name and results
     * If all results passed say so, otherwise print accordingly */

    rl.on("line", function(cmd) {
      res = res.then(function(_) {
        return rp.run(cmd);
      });

      res.then(function(new_res) {
        if(rt.isSuccessResult(new_res)) {
          //TODO: turn this into a handler
          //TODO: get the checks field and render checks (should be okay if none)
          console.log(gf(new_res.result, "answer"));
        }
        else {
          //TODO: turn this into a handler as well
          var exceptions = new_res.exn.exn;
          drawCompileErrors(exceptions);
        }

        rl.prompt();
      });
    }).on('close', function() {
      process.exit(0);
    });
    rl.prompt();
  });
})
