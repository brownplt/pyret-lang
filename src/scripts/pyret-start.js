define(["requirejs", "js/runtime-anf", "compiler/pyret.arr"], function(r, RT, pyret) {
  var rt = RT.makeRuntime({
    initialGas: 500,
    stdout: function(str) { process.stdout.write(str); },
    stderr: function(str) { process.stderr.write(str); }
  });
  rt.setParam("command-line-arguments", process.argv.slice(1));
  function printStack(stack) {
    stackStr = stack.map(function(val) {
      if (val instanceof Array && val.length == 7) {
        return (val[0] + ": line " + val[1] + ", column " + val[2]);
      } else {
        return JSON.stringify(val);
      }
    });
    return "  " + stackStr.join("\n  ");
  }
  rt.run(pyret, rt.namespace, {sync: true}, function(result) {
     if(rt.isSuccessResult(result)) {
        process.exit(0);
     } else if (rt.isFailureResult(result)) {
        var exnStack = result.exn.stack; result.exn.stack = undefined;
        var pyretStack = result.exn.pyretStack; result.exn.pyretStack = undefined;
        console.error('Pyret terminated with an error:\n' + String(result).substring(0, 500) + "\nStack:\n" 
                      + String(exnStack) + "\nPyret stack:\n" + printStack(pyretStack));
        process.exit(1);
     }
  });
});
