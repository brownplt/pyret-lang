define([
    "q",
    "js/secure-loader",
    "js/runtime-anf",
    "js/dialects-lib",
    "js/ffi-helpers",
    "compiler/compile-structs.arr",
    "compiler/compile.arr",
    "compiler/repl-support.arr",
    "trove/parse-pyret",
    "trove/checker"],
function(q, loader, rtLib, dialectsLib, ffiHelpersLib, csLib, compLib, replLib, parseLib, checkerLib) {
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

  // How long to wait for all modules to load in milliseconds
  var TIMEOUT_MS = 10000;

  function runCompilePyret(runtime, ast, options, ondone) {
    runtime.runThunk(function() { return compilePyret(runtime, ast, options); }, ondone);
  }

  function compilePyret(runtime, ast, options) {
    function s(str) { return runtime.makeString(str); }
    function gf(obj, fld) { return runtime.getField(obj, fld); }

    return runtime.safeCall(function() {
      return ffiHelpersLib(runtime, runtime.namespace);
    }, function(ffi) {
      return runtime.safeCall(function() {
        return dialectsLib(runtime, runtime.namespace);
      },
      function(dialects) {
        return runtime.loadModules(runtime.namespace, [csLib, compLib], function(cs, comp) {
          var name = options.name || randomName();
          var compileEnv = options.compileEnv || gf(cs, "standard-builtins");
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

            },
            "compiling JS ast");
        });
      },
      "loading dialects");
    },
    "loading ffi-helpers library");
  }

  function runCompileSrcPyret(runtime, src, options, ondone) {
    runtime.runThunk(function() { return compileSrcPyret(runtime, src, options); }, ondone);
  }

  function compileSrcPyret(runtime, src, options) {
    return runtime.safeCall(function() {
      return parsePyret(runtime, src, options);
    }, function(answer) {
      return compilePyret(runtime. answer, options);
    });
  }

  function runParsePyret(runtime, src, options, ondone) {
    runtime.runThunk(function() { return parsePyret(runtime, src, options); }, ondone);
  }

  function parsePyret(runtime, src, options) {
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
      },
      "loading dialects to parse Pyret");
    });
  }

  function runEvalPyret(runtime, src, options, ondone) {
    runtime.runThunk(function() { return evalPyret(runtime, src, options); }, ondone);
  }

  function evalPyret(runtime, src, options) {
    return runtime.safeCall(function() {
      return parsePyret(runtime, src, options);
    },
    function(answer) {
      return evalParsedPyret(runtime, parsed.result, options);
    });
  }

  function runLoadSpecialImports(runtime, ast, options, ondone) {
    runtime.runThunk(function() {
      return loadSpecialImports(runtime, ast, options);
    }, ondone);
  }

  function loadSpecialImports(runtime, ast, options) {
    // NOTE(joe):
    // We're about to do something async (request getSpecialImport), so stash
    // the stack so we don't lose our place in the middle of module loading.
    // Note the lack of returns -- the return value flows out through restarter.resume()
    return runtime.loadModules(runtime.namespace, [replLib], function(repl) {
      var getImports = runtime.getField(repl, "get-special-imports");
      return runtime.safeCall(function() {
        console.log("Loading specials");
        return getImports.app(ast);
      }, function(imports) {
        var jsImports = runtime.ffi.toArray(imports);
        console.log("Loading specials: ", jsImports);
        runtime.pauseStack(function(restarter) {
          console.log("Stack paused ", restarter);
          var allImports = q.all(jsImports.map(function(i) { return options.getSpecialImport(runtime, i); }));
          var moduleLoads = [];
          for(var i = 0; i < jsImports.length; i++) { moduleLoads[i] = q.defer(); }
          var loaded = q.all(moduleLoads.map(function(ml) { return ml.promise; }));
          console.log("Loading: ", moduleLoads);
          allImports.then(function(astAndNames) {
            astAndNames.forEach(function(astAndName, i) {
              // Cached (or already visited in DAG), so don't reload
              if(astAndName === "loaded") {
                moduleLoads[i].resolve("loaded");
                return;
              }
              var newOptions = Object.create(options);
              newOptions.name = astAndName.name;
              // NOTE(joe):
              // Calling this for side effect of loading, and failures will propagate
              // on their own via the timeout on loaded below
              runtime.runThunk(function() { loadParsedPyret(runtime, astAndName.ast, newOptions); }, function(result) {
                if(runtime.isSuccessResult(result)) {
                  moduleLoads[i].resolve(result.result);
                }
                else {
                  moduleLoads[i].reject(result.exn);
                }
              });
            });
          });
          q.timeout(loaded, TIMEOUT_MS);
          loaded.then(function(v) {
            console.log("Done: ", v);
            restarter.resume(v);
          });
          loaded.fail(function(err) {
            console.log("Module load failed: ", v);
            restarter.error(err);
          });
        });
      });
    });
  }

  function loadParsedPyret(runtime, ast, options) {
    if (!options.hasOwnProperty("name")) { options.name = randomName(); }
    var modname = options.name;
    var namespace = options.namespace || runtime.namespace;
    runtime.pauseStack(function(restarter) {
      console.log("Loading parsed py", restarter);
      runtime.runThunk(function() {
        return runtime.safeCall(function() {
          console.log("Loading special imports", options);
          return loadSpecialImports(runtime, ast, options);
        }, function(_) {
          console.log("Compiling parsed py", options);
          return compilePyret(runtime, ast, options);
        });
      }, function(result) {
        console.log("Finished compiling parsed py", options);
        if(runtime.isFailureResult(result)) {
          restarter.error(result.exn);
          return;
        }
        if (typeof result.result !== 'string') {
          throw new Error("Non-string result from compilation: " + result.result);
        }
        var compiledModule = loader.goodIdea(runtime, modname, result.result);
        compiledModule.then(function(mod) { restarter.resume(mod) });
        compiledModule.fail(function(err) { restarter.error(err) });
      });
    });
  }

  function runEvalParsedPyret(runtime, ast, options, ondone) {
    runtime.runThunk(function() { evalParsedPyret(runtime, ast, options); }, ondone);
  }

  function evalParsedPyret(runtime, ast, options) {
    if (!options.name) { options.name = randomName(); }
    return runtime.safeCall(function() {
      return loadParsedPyret(runtime, ast, options);
    }, function(mod) {
      return runtime.loadModules(runtime.namespace, [checkerLib], function(checker) {
        var currentChecker = runtime.getField(checker, "make-check-context").app(runtime.makeString(options.name), runtime.makeBoolean(false));
        runtime.setParam("current-checker", currentChecker);
        var sync = false;
        var namespace = options.namespace || runtime.namespace;
        runtime.pauseStack(function(restarter) {
          runtime.run(mod, namespace, {}, function(result) {
            if(runtime.isSuccessResult(result)) { restarter.resume(result.result); }
            else {
              restarter.error(result.exn);
            }
          });
        });
      });
    });
  }

  return {
    runCompilePyret: runCompilePyret,
    compilePyret: compilePyret,
    runEvalPyret: runEvalPyret,
    evalPyret: evalPyret,
    runParsePyret: runParsePyret,
    parsePyret: parsePyret,
    runCompileSrcPyret: runCompileSrcPyret,
    compileSrcPyret: compileSrcPyret,
    runEvalParsedPyret: runEvalParsedPyret,
    evalParsedPyret: evalParsedPyret
  };
  
});
