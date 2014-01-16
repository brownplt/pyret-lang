var R = require("requirejs");
var RT = require("../runtime-anf.js").PYRET_ANF;

R([process.argv[2]], function(mainModule) {
  var rt = RT.makeRuntime({ stdout: function(str) { console.log(str); } });
  rt.run(mainModule, rt.namespace, function(result) {
     if(rt.isSuccessResult(result)) {
        console.log(result);
        process.exit(0);
     } else if (rt.isFailureResult(result)) {
        console.error('Pyret terminated with an error');
        console.error(result);
        process.exit(1);
     }
  });
});
