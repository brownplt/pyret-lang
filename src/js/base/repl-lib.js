define(["q", "js/eval-lib", "compiler/repl-support.arr"], function(Q, eval, rs) {

  var defer = function(f) { setTimeout(f, 0); }
  function createRepl(runtime, namespace, initialCompileEnv, options) {
    var setImmediate = function(f) { setTimeout(f, 0); };
    var runImmediate = function(f, then) {
      setImmediate(function() {
        runtime.runThunk(f, then);
      });
    }
    var dialect = options.dialect || "Pyret";
    var mainName = options.name || "repl-main";
    var typeCheck = options.typeCheck || false;
    return runtime.loadModules(namespace, [rs], function(replSupport) {
      var toRun = [];
      var somethingRunning = false;
      function get(obj, fld) { return runtime.getField(obj, fld); }
      
      // adding `exit` function into the environment
      var exitFunction = runtime.makeFunction(function() {
        runtime.checkArity(0, arguments, 'exit');
        process.exit();
      })
      var exitCodeFunction = runtime.makeFunction(function(exitcode) {
        runtime.checkArity(1, arguments, 'exit-code');
        runtime.checkNumber(exitcode);
        process.exit(exitcode);
      })
      namespace = namespace.set('exit', exitFunction)
      namespace = namespace.set('quit', exitFunction)
      namespace = namespace.set('exit-code', exitCodeFunction)
      initialCompileEnv = get(replSupport, "add-global-binding").app(initialCompileEnv, "exit");
      initialCompileEnv = get(replSupport, "add-global-binding").app(initialCompileEnv, "quit");
      initialCompileEnv = get(replSupport, "add-global-binding").app(initialCompileEnv, "exit-code");
      
      var mainCompileEnv = initialCompileEnv;
      var initialReplCompileEnv = get(replSupport, "drop-module-bindings").app(mainCompileEnv);
      var replCompileEnv = initialReplCompileEnv;
      
      function evaluate(toEval) {
        if (toEval.beforeRun) { toEval.beforeRun(); }
        var envToUse = toEval.isMain ? mainCompileEnv : replCompileEnv;
        eval.runEvalParsedPyret(runtime, toEval.ast, { sync: false, name: toEval.name, namespace: namespace, compileEnv: envToUse, getSpecialImport: options.getSpecialImport, typeCheck: typeCheck }, 
          function(result) {
            if(runtime.isSuccessResult(result)) {
              var provided = get(get(result.result, "provide-plus-types"), "values");
              runtime.getFields(provided).forEach(function(f) {
                  namespace = namespace.set(f, get(provided, f));
                  replCompileEnv = get(replSupport, "add-global-binding").app(replCompileEnv, runtime.makeString(f));
                });
              var providedTypes = get(get(result.result, "provide-plus-types"), "types");
              Object.keys(providedTypes).forEach(function(f) {
                namespace = namespace.set("$type$" + f, providedTypes[f]);
                replCompileEnv = get(replSupport, "add-global-type-binding").app(replCompileEnv, runtime.makeString(f));
              });
            }
            toEval.onRun(result);
          });
      }
      function makeEvaluator(toEval) {
        return function() { evaluate(toEval); };
      }
      function runIfFree() {
        if (!somethingRunning && toRun.length > 0) {
          var thisRun = toRun.pop();
          somethingRunning = true;
          defer(makeEvaluator(thisRun));
        }
      }
      function makeResumer(deferred) {
        return function(result) {
          somethingRunning = false;
          runIfFree();
          return deferred.resolve(result);
        };
      }
      function restartInteractions(code) {
        var deferred = Q.defer();
        toRun = [];
        eval.runParsePyret(runtime, code, { name: mainName, dialect: dialect, typeCheck: typeCheck }, function(astResult) {
          if(runtime.isSuccessResult(astResult)) {
            runImmediate(function() {
              return get(replSupport, "make-provide-for-repl-main").app(astResult.result, initialCompileEnv);
            },
            function(result) {
              if(!runtime.isSuccessResult(result)) {
                deferred.resolve(result);
              }
              else {
                toRun.unshift({
                    isMain: true,
                    ast:  result.result,
                    beforeRun: function() {
                      replCompileEnv = initialReplCompileEnv;
                    },
                    name: mainName,
                    onRun: makeResumer(deferred)
                  });
                runIfFree();
              }
            });
          } else {
            deferred.resolve(astResult);
          }
        });
        return deferred.promise;
      }
      function run(code, name) {
        var deferred = Q.defer();
        if (typeof name === "undefined") { name = "interactions "; }
        eval.runParsePyret(runtime, code, { name: name, dialect: dialect, typeCheck: typeCheck }, function(astResult) {
          if(runtime.isSuccessResult(astResult)) {
            runImmediate(function() {
              return get(replSupport, "make-provide-for-repl").app(astResult.result);
            },
            function(result) {
              if(!runtime.isSuccessResult(result)) {
                deferred.resolve(result);
              }
              toRun.unshift({
                  isMain: false,
                  ast: result.result,
                  name: name,
                  onRun: makeResumer(deferred)
                });
              runIfFree();
             });
          } else {
            deferred.resolve(astResult);
          }
        });
        return deferred.promise;
      }
      function pause(afterPause) {
        runtime.schedulePause(function(resumer) {
          afterPause(resumer);
        });
      }
      function stop() {
        runtime.breakAll();
        toRun = [];
        somethingRunning = false;
      }
      return {
        restartInteractions: restartInteractions,
        run: run,
        pause: pause,
        stop: stop,
        runtime: runtime
      }
    });
  }

  return {
    create: createRepl
  }

});
