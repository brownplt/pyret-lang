define(["q", "trove/load-lib", "compiler/repl-support.arr", "compiler/compile-lib.arr"], function(Q, load, support, compile) {

  function createRepl(runtime, initNamespace, initCompileEnv, fetchDefs, compileContext, importHandlers) {
    var safeP = function(f, args, optDesc) {
      var result = Q.defer();
      var desc = optDesc || "safeP-locator-repl";
      runtime.safeCall(
          function() { return f.app.apply(null, args); },
          function(r) {
            result.resolve(r);
          },
          desc);
      return result.promise;
    }
    var gf = runtime.getField;
    var repl = Q.defer();
    runtime.loadModules(runtime.namespace, [support, compile], function(s, c) {
      var locatorP = safeP(gf(s, "make-repl-definitions-locator"), [
          "definitions",
          "pyret://definitions",
          runtime.makeFunction(fetchDefs),
          initCompileEnv
        ]);
      var finderP = safeP(gf(s, "make-definitions-finder"), [importHandlers]);
      var supportP = Q.all([locatorP, finderP]);
      var libP = finderP.then(function(finder) {
        return safeP(gf(c, "make-compile-lib"), [finder]);
      });
      var support = Q.all([libP, locatorP]);
      support.spread(function(lib, locator) {
        repl.resolve({
          restartInteractions: function() {
            var resultP = Q.defer();
            runtime.runThunk(function() {
              return runtime.safeCall(function() {
                return gf(lib, "compile-worklist").app(locator, compileContext);
              }, function(worklist) {
                return runtime.safeTail(function() {
                  return gf(c, "compile-and-run-worklist").app(lib, worklist);
                });
              });
            }, function(result) {
              resultP.resolve(result);
            });
            return resultP.promise;
          }
        });
      });
      support.fail(function(e) { repl.fail(e); });
    });
    return repl.promise;
  }

  return { create: createRepl };

});
