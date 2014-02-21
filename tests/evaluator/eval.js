define([
    "js/runtime-anf",
    "js/ffi-helpers",
    "compiler/compile-structs.arr",
    "compiler/compile.arr",
    "trove/checker"],
function(rtLib, ffiHelpersLib, csLib, compLib, checkerLib) {
  var r = require("requirejs");
  function randomName() { 
    return "anon" + Math.floor(Math.random() * 10000000);
  }

  function compilePyret(runtime, src, options, ondone) {
    function getExports(lib) {
      return runtime.getField(lib(runtime, runtime.namespace), "provide");
    }
    function s(str) { return runtime.makeString(str); }
    function gf(obj, fld) { return runtime.getField(obj, fld); }

    var ffi = ffiHelpersLib(runtime, runtime.namespace);
    var cs = getExports(csLib);
    var comp = getExports(compLib);
    var checker = getExports(checkerLib);
    var name = options.name || randomName();

    var currentChecker = gf(checker, "make-check-context").app(runtime.makeString(name));
    runtime.setParam("current-checker", currentChecker);

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
                if (runtime.unwrap(gf(cs, "is-ok").app(compiled)) === true) {
                  return runtime.unwrap(gf(gf(compiled, "code"), "pyret-to-js-runnable").app());
                }
                else if (runtime.unwrap(gf(cs, "is-err").app(compiled)) === true) {
                  throw ffi.toArray(gf(compiled, "problems"));
                }
                else {
                  console.error(compiled);
                  throw new Error("Unknown result type while compiling: ", compiled);
                }
              },
              function(compileResult) {
                return compileResult;
              });
          });
        },
        runtime.namespace,
        { sync: true, initialGas: 5000 },
        ondone
      );
  }

  function evalPyret(runtime, src, options, ondone) {
    function OMGBADIDEA(name, src) {
      var evalstr = "(function(define) { " + src + " })";
      eval(evalstr)(function(deps, body) { r.define(name, deps, body); });
    }
    if (!options.name) { options.name = randomName(); }
    compilePyret(
        runtime,
        src,
        options,
        function(result) {
          if(runtime.isFailureResult(result)) {
            ondone(result);
          }
          else {
            if (typeof result.result !== 'string') {
              throw new Error("Non-string result from compilation: " + result.result);
            }
            OMGBADIDEA(options.name, result.result); 
            r([options.name], function(a) {
                var sync = options.sync || true;
                var gas = options.gas || 5000;
                runtime.run(a, runtime.namespace, {sync: sync, initialGas: gas}, ondone);
              });
          }
        }
      );
  }

  return {
    compilePyret: compilePyret,
    evalPyret: evalPyret
  };
  
});
