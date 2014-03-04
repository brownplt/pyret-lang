define(["js/runtime-anf", "arr/compiler/pyret.arr"], function(RT, pyret) {
  var rt = RT.makeRuntime({
    initialGas: 500,
    stdout: function(str) { process.stdout.write(str); },
    stderr: function(str) { process.stderr.write(str); }
  });
  rt.setParam("command-line-arguments", process.argv.slice(1));
  rt.run(pyret, rt.namespace, {sync: true}, function(result) {
     if(rt.isSuccessResult(result)) {
        process.exit(0);
     } else if (rt.isFailureResult(result)) {
        var exnStack = result.exn.stack; result.exn.stack = undefined;
        var pyretStack = result.exn.pyretStack; result.exn.pyretStack = undefined;
        console.error('Pyret terminated with an error:\n' + String(result).substring(0, 500) + "\nStack:\n" 
                      + String(exnStack) + "\nPyret stack:\n" + JSON.stringify(pyretStack, null, "  "));
        process.exit(1);
     }
  });
});
