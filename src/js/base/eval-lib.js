define([
    "js/secure-loader",
    "js/runtime-anf",
    "js/ffi-helpers",
    "js/dialects-lib",
    "compiler/compile-structs.arr",
    "compiler/compile.arr",
    "trove/parse-pyret",
    "trove/checker"],
function(rtLib, ffiHelpersLib, dialectsLib, csLib, compLib, parseLib, checkerLib) {
function(loader, rtLib, dialectsLib, ffiHelpersLib, csLib, compLib, parseLib, checkerLib) {
  if(requirejs.isBrowser) {
    var r = requirejs;
    var define = window.define;
  }
  else {
    var r = require("requirejs");
    var define = r.define;
  }
  function randomName() { 
    return "anon" + Math.floor(Math.random() * 10000000);
  }

  function compilePyret(runtime, ast, options, ondone) {
    function getExports(lib) {
      return runtime.getField(lib(runtime, runtime.namespace), "provide");
    }
    function s(str) { return runtime.makeString(str); }
    function gf(obj, fld) { return runtime.getField(obj, fld); }

    var ffi = ffiHelpersLib(runtime, runtime.namespace);
    var dialects = dialectsLib(runtime, runtime.namespace);
    var cs = getExports(csLib);
    var comp = getExports(compLib);
    var name = options.name || randomName();
    var compileEnv = options.compileEnv || gf(cs, "standard-builtins");

    runtime.run(function(_, namespace) {
        return runtime.safeCall(function() {
            return gf(comp, "compile-js-ast").app(
                ast,
                s(name),
                compileEnv,
                runtime.makeObject({
                  "check-mode": runtime.pyretTrue,
                  "allow-shadowed": runtime.pyretFalse
                })
              );
          },
          function(compiled) {
            return runtime.safeTail(function() {
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
              });
          });
        },
        runtime.namespace,
        { sync: ('sync' in options) ? options.sync : true, initialGas: 500 },
        ondone
      );
  }

  function compileSrcPyret(runtime, src, options, ondone) {
    parsePyret(runtime, src, options, function(parsed) {
      compilePyret(runtime, parsed, options, ondone);
    });
  }

  function parsePyret(runtime, src, options, ondone) {
    var pp = runtime.getField(parseLib(runtime, runtime.namespace), "provide");
    var dialects = dialectsLib(runtime, runtime.namespace);
    if (!options.name) { options.name = randomName(); }
    return ondone(runtime.getField(pp, "parse-dialect").app(
      runtime.makeString(options.dialect || dialects.defaultDialect), 
      runtime.makeString(src), 
      runtime.makeString(options.name)));
  }

  function evalPyret(runtime, src, options, ondone) {
    parsePyret(runtime, src, options, function(parsed) {
      evalParsedPyret(runtime, parsed, options, ondone);
    });
  }

  function evalParsedPyret(runtime, ast, options, ondone) {
    if (!options.name) { options.name = randomName(); }
    var modname = randomName();
    var namespace = options.namespace || runtime.namespace;
    function getExports(lib) {
      return runtime.getField(lib(runtime, runtime.namespace), "provide");
    }
    var checker = getExports(checkerLib);
    var currentChecker = runtime.getField(checker, "make-check-context").app(runtime.makeString(options.name), runtime.makeBoolean(false));
    runtime.setParam("current-checker", currentChecker);

    compilePyret(
        runtime,
        ast,
        options,
        function(result) {
          if(runtime.isFailureResult(result)) {
            ondone(result);
          }
          else {
            if (typeof result.result !== 'string') {
              throw new Error("Non-string result from compilation: " + result.result);
            }
            loader.goodIdea(modname, result.result); 
            r([modname], function(a) {
                var sync = false;
                var gas = options.gas || 5000;
                runtime.run(a, namespace, {sync: sync, initialGas: gas}, ondone);
              });
          }
        }
      );
  }

  return {
    compilePyret: compilePyret,
    evalPyret: evalPyret,
    parsePyret: parsePyret,
    compileSrcPyret: compileSrcPyret,
    evalParsedPyret: evalParsedPyret
  };
  
});
