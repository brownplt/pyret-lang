define(["./eval", "../runtime/matchers", "js/ffi-helpers"], function(e, matchers, ffiLib) {
  console.log("eval-matchers body");
  var count = 0;
  function makeEvalCheckers(jasmine, runtime) {
    matchers.addPyretMatchers(jasmine);
    function gf(o, s) { return runtime.getField(o, s); }
    var pending = [];
    var ffi = ffiLib(runtime, runtime.namespace);

    function checkEvalsTo(str, answer) {
      var i = pending.push(false);
      e.evalPyret(runtime, str, {}, function(result) {
        expect(result).toBeSuccess(runtime);
        var actual = gf(result.result, "answer");
        expect(actual).toBeSameAs(runtime, answer);
        pending[i - 1] = true;
      });
    }
    function checkEvalPred(str, pred) {
      var i = pending.push(false);
      e.evalPyret(runtime, str, {}, function(result) {
        expect(result).toBeSuccess(runtime);
        if (runtime.isSuccessResult(result)) {
          var actual = gf(result.result, "answer");
          expect(actual).toPassPredicate(pred);
        }
        pending[i - 1] = true;
      });
    }
    function checkError(str, exnPred) {
      var i = pending.push(false);
      e.evalPyret(runtime, str, {}, function(result) {
        expect(result).toBeFailure(runtime);
        if (runtime.isFailureResult(result)) {
          var exn = result.exn;
          expect(exn).toPassPredicate(exnPred);
        }
        pending[i - 1] = true;
      });
    }
    function checkCompileErrorMsg(str, exnMsg) {
      function findInArray(arr) {
        for (var i = 0; i < arr.length; i++) {
          if (runtime.isString(arr[i])) {
            if (runtime.unwrap(arr[i]).indexOf(exnMsg) !== -1)
              return true;
          } else { // need to check is-err
            var actMsg = runtime.getField(arr[i], "msg");
            if (runtime.isString(actMsg)) {
              if (runtime.unwrap(actMsg).indexOf(exnMsg) !== -1)
                return true;
            }
          }
        }
        return false;
      }
      return checkCompileError(str, findInArray);
    }
    function checkCompileError(str, exnPred) {
      var i = pending.push(false);
      e.compilePyret(runtime, str, {}, function(result) {
        expect(result).toBeFailure(runtime);
        var problems = result.exn;
        // Compiling returns a string or an array of 
        // Pyret objects detailing problems
        expect(problems).toBeInstanceOf(Array);
        if (problems instanceof Array) {
          expect(problems).toPassPredicate(exnPred);
        }
        pending[i - 1] = true;
      });
    }
    function wait(done) {
      setTimeout(function() {
          if(pending.filter(function(p) { return p === false; }).length === 0) {
            done();
          }
          else {
            setTimeout(function() { wait(done); });
          }
        },
        100);
    }
    return {
      checkCompileError: checkCompileError,
      checkCompileErrorMsg: checkCompileErrorMsg,
      checkEvalsTo: checkEvalsTo,
      checkEvalPred: checkEvalPred,
      checkError: checkError,
      wait: wait
    };
  }

  return {
    makeEvalCheckers: makeEvalCheckers
  };

});
