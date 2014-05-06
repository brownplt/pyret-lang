define(["js/runtime-anf", "./eval-matchers", "../../src/js/base/eval-lib"], function(rtLib, e, pyeval) {

  var _ = require('jasmine-node');
  var rt;
  var P;
  var same;

  function performTest() {

    beforeEach(function() {
      rt1 = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
      rt2 = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
      P =  e.makeEvalCheckers(this, rt1);
    });

    describe("Modules", function() {
      function reportsNames(names) {
        return function(err) {
          expect(err.exn).toPassPredicate(rt1.ffi.errPred("is-module-load-failure"));
          expect(rt1.ffi.toArray(rt1.getField(err.exn, "names"))).toEqual(names);
          return true;
        };
      }
      it("should give a meaningful error on loading bogus modules", function(done) {
        P.checkError("import bogus-module as B", reportsNames(["trove/bogus-module"]));
        P.checkError("import bogus-module as B\n import bogus2 as B2", reportsNames(["trove/bogus-module"]));
        P.checkError("import error as actually-exists\n import bogus2 as B2", reportsNames(["trove/bogus2"]));
        P.wait(done);

      });
    });

    describe("Managed execution", function() {
      it("should error if run is called twice on the same runtime", function() {
        rt1.run(function() {
          rt1.run(function() { /* no-op */ }, rt1.namespace, {}, function(fail) {
            expect(fail).toPassPredicate(rt1.isFailureResult);
            expect(fail.exn).toPassPredicate(function(e) {
              expect(e).toBeMessageExn(rt1, "already running");
            });
          });
        }, rt1.namespace, {}, function() { });
      });
/*
      it("", function(done) {
        var trace = ["beginning"];
        var prog1 = function() {
          return rt1.safeCall(function() {
              expect(trace).toEqual(["beginning"]);
              trace.push("called");
              rt1.pauseStack(function(restarter) {
                expect(trace).toEqual(["beginning", "called"]);
              });
            }, function(v) {

            });
        };
        rt1.run(prog1, rt1.namespace, {sync: false}, function(r) {

        });
      }, 30000);
*/
    });

  }
  return {performTest: performTest};
});

