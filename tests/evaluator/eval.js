define([
    "js/runtime-anf",
    "../../build/phase2/arr/compiler/compile-structs.arr",
    "../../build/phase2/arr/compiler/compile.arr"],
function(rtLib, csLib, compLib) {
  console.log("in eval.js");
  var r = require("requirejs");

  function evalPyret(runtime, src, options, ondone) {

    function OMGBADIDEA(name, src) {
      var evalstr = "(function(define) { " + src + " })";
      eval(evalstr)(function(deps, body) { r.define(name, deps, body); });
    }

    function getExports(lib) {
      return runtime.getField(lib(runtime, runtime.namespace), "provide");
    }
    function randomName() { 
      return "anon" + Math.floor(Math.random() * 10000000);
    }
    function s(str) { return runtime.makeString(str); }
    function gf(obj, fld) { return runtime.getField(obj, fld); }

    var cs = getExports(csLib);
    var comp = getExports(compLib);
    var name = options.name || randomName();
    runtime.run(function(_, namespace) {
        return runtime.safeCall(function() {
            return gf(comp, "compile-js").app(
                s(src),
                s(name),
                gf(cs, "standard-builtins"),
                { "blowup": "true", get dict() { throw "Not a Pyret value!";} }
              );
          },
          function(compiled) {
            return runtime.safeCall(function() {
                return gf(gf(compiled, "code"), "pyret-to-js-runnable").app();
              },
              function(compileResult) {
                return runtime.unwrap(compileResult);
              });
          });
      },
      runtime.namespace,
      { sync: true, initialGas: 5000 },
      function(result) {
        if(runtime.isFailureResult(result)) {
          ondone(result);
        }
        else {
          OMGBADIDEA(name, result.result); 
          setTimeout(function() {
            r([name], function(a) {
                var sync = options.sync || true;
                var gas = options.gas || 5000;
                runtime.run(a, runtime.namespace, {sync: sync, initialGas: gas}, ondone);
            });
          }, 0);
        }
      });
  }

  return {
    evalPyret: evalPyret
  };
  
});
