var R = require("requirejs");


R([process.argv[2], "runtime-anf"], function(mainModule, RT) {
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
        console.error(result);
        console.error(result.exn.stack);
        console.error(result.exn.pyretStack);
        process.exit(1);
     }
  });
});
