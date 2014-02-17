define(["./eval", "../runtime/matchers"], function(e, matchers) {
  console.log("eval-matchers body");

  function makeEvalCheckers(jasmine, runtime) {
    matchers.addPyretMatchers(jasmine);
    function gf(o, s) { return runtime.getField(o, s); }
    var pending = [];

    function checkEvalsTo(str, answer) {
      var i = pending.push(false);
      console.log(pending);
      e.evalPyret(runtime, str, {}, function(result) {
        var actual = gf(result.result, "answer");
        expect(result).toBeSuccess(runtime);
        expect(actual).toBeSameAs(runtime, answer);
        pending[i - 1] = true;
      });
    }
    function wait(done) {
      setTimeout(function() {
          if(pending.filter(function(p) { return p === false; }).length === 0) {
            console.log("Done",  pending);
            done();
          }
          else {
            setTimeout(function() { wait(done); });
          }
        },
        100);
    }
    return {
      checkEvalsTo: checkEvalsTo,
      wait: wait
    };
  }

  return {
    makeEvalCheckers: makeEvalCheckers
  };

});
