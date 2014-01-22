var R = require("requirejs");

R([process.argv[2], "runtime-anf"], function(mainModule, RT) {
  var rt = RT.makeRuntime({ stdout: function(str) { console.log(str); } });
  rt.run(mainModule, rt.namespace, function(result) {
     if(rt.isSuccessResult(result)) {
        console.log(result);
        console.log(rt.getField(result.result, "answer"));
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
