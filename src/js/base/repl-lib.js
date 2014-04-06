define(["q", "./eval-lib", "compiler/compile-structs.arr", "compiler/repl-support.arr"], function(Q, eval, cs, rs) {

  var defer = function(f) { setTimeout(f, 0); }
  function createRepl(dialect, runtime, namespace) {
    var toRun = [];
    var somethingRunning = false;
    function get(obj, fld) { return runtime.getField(obj, fld); }
    var compileStructs = get(cs(runtime, namespace), "provide");
//    var standardCompileEnv = get(compileStructs, "standard-builtins");
    var initialCompileEnv = get(compileStructs, "standard-builtins");
    var replCompileEnv = initialCompileEnv;
    var replSupport = get(rs(runtime, namespace), "provide");
    
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
      var name = "replMain";
      eval.parsePyret(dialect, runtime, code, { name: name }, function(ast) {
        toRun.unshift({
            ast: get(replSupport, "make-provide-all").app(ast),
            beforeRun: function() { replCompileEnv = initialCompileEnv; },
            name: name,
            onRun: makeResumer(deferred)
          });
        runIfFree();
      });
      return deferred.promise;
    }
    function run(code) {
      var deferred = Q.defer();
      var name = "replRun";
      eval.parsePyret(dialect, runtime, code, { name: name }, function(ast) {
        toRun.unshift({
            ast: get(replSupport, "make-provide-all").app(ast),
            name: name,
            onRun: makeResumer(deferred)
          });
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
  }

  return {
    create: createRepl
  }

});
