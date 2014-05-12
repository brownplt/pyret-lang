define(["q", "js/eval-lib", "compiler/repl-support.arr", "js/dialects-lib"], function(Q, eval, rs, dialectsLib) {

  var defer = function(f) { setTimeout(f, 0); }
  function createRepl(runtime, namespace, initialCompileEnv, options) {
    var dialect = options.dialect || "Pyret";
    var mainName = options.name || "repl-main";
    return runtime.loadModules(namespace, [rs], function(replSupport) {
      var toRun = [];
      var somethingRunning = false;
      function get(obj, fld) { return runtime.getField(obj, fld); }
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
        toRun = [];
        eval.parsePyret(runtime, code, { name: mainName, dialect: dialect }, function(astResult) {
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
        var name = "latest interactions";
        eval.parsePyret(runtime, code, { name: name, dialect: dialect }, function(astResult) {
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
      function stop() {
        runtime.breakAll();
        toRun = [];
        somethingRunning = false;
      }
      return {
        restartInteractions: restartInteractions,
        run: run,
        pause: pause,
        stop: stop
      }
    });
  }

  return {
    create: createRepl
  }

});
