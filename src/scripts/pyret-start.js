define(["requirejs", "js/runtime-anf", "compiler/pyret.arr", "trove/render-error-display"], function(r, RT, pyret, rendererrorLib) {
  var rt = RT.makeRuntime({
    initialGas: 600,
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
      if (rt.isObject(result.exn.exn) && rt.hasField(result.exn.exn, "render-reason")) {
        rt.run(function(_, _) {
          return rt.getColonField(result.exn.exn, "render-reason").full_meth(result.exn.exn);
        }, rt.namespace, {sync: true}, function(outputResult) {
          if (rt.isFailureResult(outputResult)) {
            console.error('While trying to report that Pyret terminated with an error:\n' + JSON.stringify(result)
                          + "\nPyret encountered an error rendering that error:\n" + JSON.stringify(outputResult)
                          + '\nStack:\n' + JSON.stringify(exnStack)
                          + "\nPyret stack:\n" + rt.printPyretStack(pyretStack, true));
            process.exit(1);
          } else {
            rt.loadModulesNew(rt.namespace, [rendererrorLib], function(rendererrorLib) {
              var rendererror = rt.getField(rendererrorLib, "values");
              var displayToString = rt.getField(rendererror, "display-to-string");
              rt.run(function(_, _) {
                return rt.getField(rendererror, "display-to-string").app(
                  outputResult.result, 
                  rt.namespace.get("tostring"), 
                  rt.ffi.makeList(pyretStack.map(rt.makeSrcloc)));
              }, rt.namespace, {sync: true}, function(printResult) {
                if(rt.isFailureResult(printResult)) {
                  console.error('While trying to report that Pyret terminated with error:\n' + JSON.stringify(result)
                                + "\nstringifying that error produced another error:\n" + JSON.stringify(printResult)
                                + "\nStack:\n" + JSON.stringify(exnStack)
                                + "\nPyret stack:\n" + rt.printPyretStack(pyretStack));
                  process.exit(1);
                } else {
                  console.error('Pyret terminated with an error:\n' + printResult.result + "\nStack:\n"  +
                                "\nPyret stack:\n" + rt.printPyretStack(pyretStack));
                  process.exit(1);
                }
              });
            });
          }
        });
      } else {
        console.error(result.exn.exn);
        process.exit(1);
      }
    }
  });
});
