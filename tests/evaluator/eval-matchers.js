define(["./eval", "../runtime/matchers", "js/ffi-helpers"], function(e, matchers) {
  console.log("eval-matchers body");

  function makeEvalCheckers(jasmine, runtime) {
    matchers.addPyretMatchers(jasmine);
    function gf(o, s) { return runtime.getField(o, s); }
    var pending = [];

    function checkEvalsTo(str, answer) {
      var i = pending.push(false);
      e.evalPyret(runtime, str, {}, function(result) {
        expect(result).toBeSuccess(runtime);
        var actual = gf(result.result, "answer");
        console.log("Comparing ", runtime.toReprJS(actual), runtime.toReprJS(answer));
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
    function checkErrorMsg(str, exnMsg) {
      var i = pending.push(false);
      e.evalPyret(runtime, str, {}, function(result) {
        expect(result).toBeFailure(runtime);
        if (runtime.isFailureResult(result)) {
          expect(result.exn).toMatchError(runtime, exnMsg);
        }
        pending[i - 1] = true;
      });
    }
    function checkCompileError(str, exnPred) {
      var i = pending.push(false);
      e.compilePyret(runtime, str, {}, function(result) {
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
      checkEvalsTo: checkEvalsTo,
      checkEvalPred: checkEvalPred,
      checkError: checkError,
      checkErrorMsg: checkErrorMsg,
      wait: wait
    };
  }

  return {
    makeEvalCheckers: makeEvalCheckers
  };

});
