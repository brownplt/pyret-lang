define(["q", "./eval-lib", "compiler/compile-structs.arr", "compiler/repl-support.arr"], function(Q, eval, cs, rs) {

  var defer = function(f) { setTimeout(f, 0); }
  function createRepl(runtime, namespace, options) {
    var mainName = options.name || "replMain";
    return runtime.loadModules(namespace, [cs, rs], function(compileStructs, replSupport) {
      var toRun = [];
      var somethingRunning = false;
      function get(obj, fld) { return runtime.getField(obj, fld); }
      var initialCompileEnv = get(compileStructs, "standard-builtins");
      var replCompileEnv = initialCompileEnv;
      
      function evaluate(toEval) {
        if (toEval.beforeRun) { toEval.beforeRun(); }
        eval.evalParsedPyret(runtime, toEval.ast, { sync: false, name: toEval.name, namespace: namespace, compileEnv: replCompileEnv }, 
          function(result) {
            if(runtime.isSuccessResult(result)) {
              var provided = get(result.result, "provide");
              runtime.getFields(provided).forEach(function(f) {
                  namespace = namespace.set(f, get(provided, f));
                  replCompileEnv = get(replSupport, "add-global-binding").app(replCompileEnv, runtime.makeString(f));
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
        eval.parsePyret(runtime, code, { name: mainName }, function(astResult) {
          if(runtime.isSuccessResult(astResult)) {
            toRun.unshift({
                ast: get(replSupport, "make-provide-all").app(astResult.result),
                beforeRun: function() { replCompileEnv = initialCompileEnv; },
                name: mainName,
                onRun: makeResumer(deferred)
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
        var name = "replRun";
        eval.parsePyret(runtime, code, { name: name }, function(astResult) {
          if(runtime.isSuccessResult(astResult)) {
            toRun.unshift({
                ast: get(replSupport, "make-provide-all").app(astResult.result),
                name: name,
                onRun: makeResumer(deferred)
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
      return {
        restartInteractions: restartInteractions,
        run: run,
        pause: pause
      }
    });
  }

  return {
    create: createRepl
  }

});
