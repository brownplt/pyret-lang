define(["q", "js/eval-lib", "compiler/repl-support.arr"], function(Q, eval, rs) {

  var defer = function(f) { setTimeout(f, 0); }
  function createRepl(runtime, namespace, initialCompileEnv, options) {
    var dialect = options.dialect || "Pyret";
    var mainName = options.name || "repl-main";
    return runtime.loadModules(namespace, [rs], function(replSupport) {
      var toRun = [];
      var somethingRunning = false;
      function get(obj, fld) { return runtime.getField(obj, fld); }
      var mainCompileEnv = initialCompileEnv;
      var initialReplCompileEnv = get(replSupport, "drop-module-bindings").app(mainCompileEnv);
      var replCompileEnv = initialReplCompileEnv;
      
      function evaluate(toEval) {
        if (toEval.beforeRun) { toEval.beforeRun(); }
        var envToUse = toEval.isMain ? mainCompileEnv : replCompileEnv;
        eval.evalParsedPyret(runtime, toEval.ast, { sync: false, name: toEval.name, namespace: namespace, compileEnv: envToUse }, 
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
        eval.parsePyret(runtime, code, { name: mainName, dialect: dialect }, function(astResult) {
          if(runtime.isSuccessResult(astResult)) {
            runtime.runThunk(function() {
              return get(replSupport, "make-provide-for-repl-main").app(astResult.result, initialCompileEnv);
            },
            function(result) {
              if(!runtime.isSuccessResult(result)) { throw "Bad result in repl-lib"; }
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
              }
            });
          } else {
            deferred.resolve(astResult);
          }
          runIfFree();
        });
        return deferred.promise;
      }
      function run(code) {
        var deferred = Q.defer();
        var name = "latest interactions";
        eval.parsePyret(runtime, code, { name: name, dialect: dialect }, function(astResult) {
          if(runtime.isSuccessResult(astResult)) {
            runtime.runThunk(function() {
              return get(replSupport, "make-provide-for-repl").app(astResult.result);
            },
            function(result) {
              if(!runtime.isSuccessResult(result)) { throw "Bad result in repl-lib"; }
              toRun.unshift({
                  isMain: false,
                  ast: result.result,
                  name: name,
                  onRun: makeResumer(deferred)
                });
             });
          } else {
            deferred.resolve(astResult);
          }
          runIfFree();
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
