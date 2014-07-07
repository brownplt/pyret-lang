define([
    "js/secure-loader",
    "js/runtime-anf",
    "js/dialects-lib",
    "js/ffi-helpers",
    "compiler/compile-structs.arr",
    "compiler/compile.arr",
    "trove/parse-pyret",
    "trove/checker"],
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
    function s(str) { return runtime.makeString(str); }
    function gf(obj, fld) { return runtime.getField(obj, fld); }

    return runtime.safeCall(function() {
      return ffiHelpersLib(runtime, runtime.namespace);
    }, function(ffi) {
      return runtime.safeCall(function() {
        return dialectsLib(runtime, runtime.namespace);
      },
      function(dialects) {
        runtime.loadModules(runtime.namespace, [csLib, compLib], function(cs, comp) {
          var name = options.name || randomName();
          var compileEnv = options.compileEnv || gf(cs, "standard-builtins");

          runtime.run(function(_, namespace) {
              return runtime.safeCall(function() {
                  return gf(comp, "compile-js-ast").app(
                      gf(comp, "start"),
                      ast,
                      s(name),
                      compileEnv,
                      runtime.makeObject({
                        "check-mode": runtime.pyretTrue,
                        "allow-shadowed": runtime.pyretFalse,
                        "collect-all": runtime.pyretFalse,
                        "ignore-unbound": runtime.pyretFalse
                      })
                    );
                },
                function(compPhase) {
                  var compiled = gf(compPhase, "result");
                  return runtime.safeTail(function() {
                      if (runtime.unwrap(gf(cs, "is-ok").app(compiled)) === true) {
                        return runtime.unwrap(gf(gf(compiled, "code"), "pyret-to-js-runnable").app());
                      }
                      else if (runtime.unwrap(gf(cs, "is-err").app(compiled)) === true) {
                        throw ffi.toArray(gf(compiled, "problems"));
                      }
                      else {
                        throw new Error("Unknown result type while compiling: ", compiled);
                      }
                    });

                });
              },
              runtime.namespace,
              { sync: ('sync' in options) ? options.sync : true },
              ondone)
        });

      });
    });
  }

  function compileSrcPyret(runtime, src, options, ondone) {
    parsePyret(runtime, src, options, function(parsed) {
      if(runtime.isSuccessResult(parsed)) {
        compilePyret(runtime, parsed.result, options, ondone);
      } else {
        ondone(parsed);
      }
    });
  }

  function parsePyret(runtime, src, options, ondone) {
    return runtime.runThunk(function() {
      return runtime.loadModulesNew(runtime.namespace, [parseLib], function(parseLib) {
        var pp = runtime.getField(parseLib, "values");
        return runtime.safeCall(function() {
          return dialectsLib(runtime, runtime.namespace);
        }, function(dialects) {
          if (!options.name) { options.name = randomName(); }
            return runtime.getField(pp, "parse-dialect").app(
                      runtime.makeString(options.dialect || dialects.defaultDialect), 
                      runtime.makeString(src), 
                      runtime.makeString(options.name));
        });
      });
    }, ondone);
  }

  function evalPyret(runtime, src, options, ondone) {
    parsePyret(runtime, src, options, function(parsed) {
      if(runtime.isSuccessResult(parsed)) {
        evalParsedPyret(runtime, parsed.result, options, ondone);
      } else {
        ondone(parsed);
      }
    });
  }

  function evalParsedPyret(runtime, ast, options, ondone) {
    if (!options.name) { options.name = randomName(); }
    var modname = randomName();
    var namespace = options.namespace || runtime.namespace;
    runtime.loadModules(runtime.namespace, [checkerLib], function(checker) {
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
              var compiledModule = loader.goodIdea(runtime, modname, result.result);
              compiledModule.then(function(mod) {
                var sync = false;
                runtime.run(mod, namespace, {sync: sync}, ondone);
              });
              compiledModule.fail(function(err) {
                ondone(runtime.makeFailureResult(err));
              });
            }
          }
        );
    });
  }

  return {
    compilePyret: compilePyret,
    evalPyret: evalPyret,
    parsePyret: parsePyret,
    compileSrcPyret: compileSrcPyret,
    evalParsedPyret: evalParsedPyret
  };
  
});
