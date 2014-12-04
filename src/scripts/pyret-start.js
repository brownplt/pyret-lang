define(["requirejs", "js/runtime-anf", "compiler/pyret.arr"], function(r, RT, pyret) {
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
        rt.run(function(_, _) {
          return rt.toReprJS(result.exn.exn, "tostring");
        }, rt.namespace, {sync: true}, function(printResult) {
          if(rt.isFailureResult(printResult)) {
            console.error('Pyret terminated with an unprintable error:\n' + JSON.stringify(result) + "\nStack:\n" 
                          + JSON.stringify(exnStack) + "\nPyret stack:\n" + rt.printPyretStack(pyretStack));
            process.exit(1);
          } else {
            console.error('Pyret terminated with an error:\n' + printResult.result + "\nStack:\n"  +
                          "\nPyret stack:\n" + rt.printPyretStack(pyretStack));

          }
        });
     }
  });
});
