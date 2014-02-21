var r = require("requirejs")
define(["js/runtime-anf", "./eval-matchers"], function(rtLib, e) {

  var _ = require('jasmine-node');
  var rt;
  var P;
  var same;

  function performTest() {

    beforeEach(function() {
      rt = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
      P =  e.makeEvalCheckers(this, rt);
      checks = P.checkEvalTests;
    });
    describe("Check mode", function() {
      it("should work for simple is", function(done) {
        
        checks("check: 5 is 5;", function(results) {
          console.log(rt.toReprJS(results));
          return true;
        });

        checks(
"fun square(x): x * x\n " +
"where:\n" +
"  square(4) is 16\n" +
"  square(5) satisfies fun(n): n == 25 end\n" +
"end\n",
        function(results) {
          console.log(rt.toReprJS(results));
          return true;
        });

        P.wait(done);
      });
    });

  }
  return {performTest: performTest};
});

