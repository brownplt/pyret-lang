define(["js/runtime-anf", "arr/compiler/pyret.arr"], function(RT, pyret) {
  var rt = RT.makeRuntime({
    initialGas: 500,
    stdout: function(str) { process.stdout.write(str); }
  });
  rt.setParam("command-line-arguments", process.argv.slice(1));
  rt.run(pyret, rt.namespace, {sync: true}, function(result) {
     if(rt.isSuccessResult(result)) {
        process.exit(0);
     } else if (rt.isFailureResult(result)) {
        console.error('Pyret terminated with an error:\n' + result.exn.exn.s + "\nStack:\n" 
                      + String(result.exn.stack) + "\nPyret stack:\n" + JSON.stringify(result.exn.pyretStack, null, "  "));
        process.exit(1);
     }
  });
});
