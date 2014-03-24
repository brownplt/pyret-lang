define(["q", "../../tests/evaluator/eval", "compiler/compile-structs.arr", "compiler/repl-support.arr"], function(Q, eval, cs, rs) {

  var defer = function(f) { setTimeout(f, 0); }
  function createRepl(runtime, namespace) {
    var toRun = [];
    var somethingRunning = false;
    function get(obj, fld) { return runtime.getField(obj, fld); }
    var compileStructs = get(cs(runtime, namespace), "provide");
//    var standardCompileEnv = get(compileStructs, "standard-builtins");
    var replCompileEnv = get(compileStructs, "standard-builtins");
    var replSupport = get(rs(runtime, namespace), "provide");
    
    function evaluate(toEval) {
      eval.evalParsedPyret(runtime, toEval.ast, { name: toEval.name, namespace: namespace, compileEnv: replCompileEnv }, 
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
      eval.parsePyret(runtime, code, { name: name }, function(ast) {
        toRun.unshift({
            ast: get(replSupport, "make-provide-all").app(ast),
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
      eval.parsePyret(runtime, code, { name: name }, function(ast) {
        toRun.unshift({
            ast: get(replSupport, "make-provide-all").app(ast),
            name: name,
            onRun: makeResumer(deferred)
          });
        runIfFree();
      });
      return deferred.promise;
    }
    return {
      restartInteractions: restartInteractions,
      run: run
    }
  }

  return {
    create: createRepl
  }

});
