var r = require("requirejs")
define(["js/runtime-anf", "./eval-matchers"], function(rtLib, e) {

  var _ = require('jasmine-node');
  var rt;
  var P;

  function performTest() {

    beforeEach(function() {
      rt = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
      P =  e.makeEvalCheckers(this, rt);
    });
    describe("If", function() {
      it("should dispatch on true", function(done) {

        console.log("checking evals to");
        P.checkEvalsTo("if true: 5 else: 10 end", rt.makeNumber(5));

        P.wait(done);

      });
    });
  }
  return { performTest: performTest };
});
