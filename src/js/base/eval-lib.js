define([
    "q",
    "js/secure-loader",
    "js/runtime-anf",
    "compiler/compile-structs.arr",
    "compiler/compile.arr",
    "compiler/repl-support.arr",
    "trove/parse-pyret",
    "trove/checker",
    "js/runtime-util"],
function(q, loader, rtLib, csLib, compLib, replLib, parseLib, checkerLib, util) {
  if(util.isBrowser()) {
    var r = requirejs;
  }
  else {
    var r = require("requirejs");
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

      return runtime.loadModules(runtime.namespace, [csLib, compLib], function(cs, comp) {
        var name = options.name || randomName();
        var compileEnv = options.compileEnv || gf(cs, "standard-builtins");
        var compileLibs = options.compileLibs || gf(cs, "standard-imports");
        return runtime.safeCall(function() {
            return gf(comp, "compile-js-ast").app(
                gf(comp, "start"),
                ast,
                s(name),
                compileEnv,
                compileLibs,
                runtime.makeObject({
                  "check-mode": runtime.pyretTrue,
                  "allow-shadowed": runtime.pyretFalse,
                  "proper-tail-calls": options.properTailCalls || true,
                  "collect-all": runtime.pyretFalse,
                  "type-check": runtime.makeBoolean(options.typeCheck || false),
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
                  // NOTE(joe): reverse added to get compile errors in the right order
                  // for the UI reporting
                  throw runtime.ffi.toArray(gf(compiled, "problems")).reverse();
                }
                else {
                  throw new Error("Unknown result type while compiling: ", compiled);
                }
              });

          },
          "compiling JS ast");
    });
  }

  function runCompileSrcPyret(runtime, src, options, ondone) {
    runtime.runThunk(function() { return compileSrcPyret(runtime, src, options); }, ondone);
  }

  function compileSrcPyret(runtime, src, options) {
    return runtime.safeCall(function() {
      return parsePyret(runtime, src, options);
    }, function(answer) {
      return compilePyret(runtime, answer, options);
    });
  }

  function runParsePyret(runtime, src, options, ondone) {
    runtime.runThunk(function() { return parsePyret(runtime, src, options); }, ondone);
  }

  function parsePyret(runtime, src, options) {
    return runtime.loadModulesNew(runtime.namespace, [parseLib], function(parseLib) {
      var pp = runtime.getField(parseLib, "values");
      if (!options.name) { options.name = randomName(); }
        return runtime.getField(pp, "surface-parse").app(
                  runtime.makeString(src), 
                  runtime.makeString(options.name));
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
      return evalParsedPyret(runtime, answer, options);
    });
  }

  function runLoadSpecialImports(runtime, ast, options, ondone) {
    runtime.runThunk(function() {
      return loadSpecialImports(runtime, ast, options);
    }, ondone);
  }

  function loadSpecialImports(runtime, ast, options) {
    return runtime.loadModules(runtime.namespace, [replLib], function(repl) {
      var getImports = runtime.getField(repl, "get-special-imports");
      return runtime.safeCall(function() {
        return getImports.app(ast);
      }, function(imports) {
        var jsImports = runtime.ffi.toArray(imports);
        // NOTE(joe):
        // We're about to do something async (request getSpecialImport), so stash
        // the stack so we don't lose our place in the middle of module loading.
        // Note the lack of returns -- the return value flows out through
        // restarter.resume() or restarter.error()
        runtime.pauseStack(function(restarter) {
          var allImports = q.all(jsImports.map(function(i) { return options.getSpecialImport(runtime, i); }));
          var moduleLoads = [];
          for(var i = 0; i < jsImports.length; i++) { moduleLoads[i] = q.defer(); }
          var loaded = q.all(moduleLoads.map(function(ml) { return ml.promise; }));
          allImports.then(function(codeAndNames) {
            function loadCode(i) {
              if(i >= codeAndNames.length) { return; }
              var codeAndName = codeAndNames[i];
              // NOTE(joe): Value "loaded" means Cached (or already visited in
              // DAG), so don't reload
              if(codeAndName === "loaded") {
                moduleLoads[i].resolve("loaded");
                loadCode(i + 1);
                return;
              }
              var newOptions = Object.create(options);
              newOptions.name = codeAndName.name;
              // NOTE(joe):
              // Calling this for side effect of loading, and failures will propagate
              // on their own via the timeout on loaded below
              runtime.runThunk(function() {
                return runtime.safeCall(function() {
                  return parsePyret(runtime, codeAndName.code, newOptions);
                }, function(ast) {
                  return runtime.safeCall(function() {
                    return runtime.getField(repl, "wrap-for-special-import").app(ast);
                  }, function(safeAst) {
                    // NOTE(joe): Don't actually care about return here, because
                    // Success/FailureResult below is the signal we need
                    return loadParsedPyret(runtime, safeAst, newOptions);
                  });
                });
              }, function(result) {
                if(runtime.isSuccessResult(result)) {
                  moduleLoads[i].resolve(result.result);
                  loadCode(i + 1);
                }
                else {
                  moduleLoads[i].reject(result.exn);
                  // Don't call loadCode; we can stop as soon as the first error happens
                }
              });
            }
            loadCode(0);
          });
          q.timeout(loaded, TIMEOUT_MS);
          loaded.then(function(v) {
            restarter.resume(v);
          });
          loaded.fail(function(err) {
            restarter.error(err);
          });
          allImports.fail(function(err) {
            restarter.error(err);
          });
        });
      });
    });
  }

  function runLoadParsedPyret(runtime, ast, options, ondone) {
    runtime.runThunk(function() { return loadParsedPyret(runtime, ast, options); }, ondone);
  }
  
  function loadParsedPyret(runtime, ast, options) {
    if (!options.hasOwnProperty("name")) { options.name = randomName(); }
    var modname = options.name;
    var namespace = options.namespace || runtime.namespace;
    runtime.pauseStack(function(restarter) {
      runtime.runThunk(function() {
        return runtime.safeCall(function() {
          return loadSpecialImports(runtime, ast, options);
        }, function(_) {
          return compilePyret(runtime, ast, options);
        });
      }, function(result) {
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
    evalParsedPyret: evalParsedPyret,
    loadParsedPyret: loadParsedPyret,
    runLoadParsedPyret: runLoadParsedPyret
  };
  
});
