var R = require("requirejs");


R([process.argv[2], "js/runtime-anf"], function(mainModule, RT) {
  var rt = RT.makeRuntime({
    initialGas: Number(process.argv[3]) || 500,
    stdout: function(str) { process.stdout.write(str); }
  });
  rt.setParam("command-line-arguments", process.argv.slice(2));
  rt.run(mainModule, rt.namespace, function(result) {
    if(rt.isSuccessResult(result)) {
      //        console.log(result);
      //        console.log(rt.getField(result.result, "answer"));
      process.exit(0);
    } else if (rt.isFailureResult(result)) {
      console.error('Pyret terminated with an error');
      if (result && result.exn && result.exn.exn && result.exn.exn.s && result.exn.stack && result.exn.pyretStack) {
        console.error(result.exn.exn.s)
        console.error(result.exn.stack);
        console.error(result.exn.pyretStack);
      } else {
        console.error(result);
      }
      process.exit(1);
    }
  });
});
