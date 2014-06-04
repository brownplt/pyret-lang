var R = require("requirejs");

R.config({
  paths: {
    trove: "./trove",
    js: "./js",
    compiler: "./arr/compiler"
  }
});

R([process.argv[2], "js/runtime-anf", "trove/checker"], function(mainModule, RT, checkerLib) {
  var rt = RT.makeRuntime({
    initialGas: Number(process.argv[3]) || 500,
    stdout: function(str) { process.stdout.write(str); },
    stderr: function(str) { process.stderr.write(str); }
  });
  
  rt.loadModulesNew([checker], function(checker) {
    var checker = rt.getField(checker, "values");
    rt.setParam("current-checker", rt.getField(checker, "make-check-context").app(rt.makeString(process.argv[2])));
    rt.setParam("command-line-arguments", process.argv.slice(2));
    rt.run(mainModule, rt.namespace, {sync: true}, function(result) {
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

  })
});
