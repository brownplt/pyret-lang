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
      err = P.checkError;
    });
    describe("field-not-found", function() {
      it("should signal an error for missed field lookups", function(done) {

        err("{}.x", function(e) {
          return rt.unwrap(e.exn).indexOf("field x not found") !== -1;
        });

        P.wait(done);

      });
    });

    describe("compiler", function() {
      it("should signal an error when the compile fails", function(done) {
        err("fun(): x = 5 y = 10 end", function(e) {
            expect(e.length).toEqual(1);
            return true;
          });
        P.wait(done);
      });
    });

  }
  return { performTest: performTest };
});
