define(["js/runtime-anf", "arr/compiler/pyret.arr"], function(RT, pyret) {
  var rt = RT.makeRuntime({
      initialGas: 500,
      stdout: function(str) { process.stdout.write(str); }
    });
  rt.setParam("command-line-arguments", process.argv.slice(1));
  rt.run(pyret, rt.namespace, function(result) {
     if(rt.isSuccessResult(result)) {
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
