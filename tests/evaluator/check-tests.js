var r = require("requirejs")
define(["js/runtime-anf", "./eval-matchers", "../../src/js/base/ffi-helpers"], function(rtLib, e, ffiLib) {

  var _ = require('jasmine-node');
  var rt;
  var P;
  var same;
  var err;
  var tests;
  var ffi;

  function performTest() {

    beforeEach(function() {
      rt = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
      P =  e.makeEvalCheckers(this, rt);
      same = P.checkEvalsTo;
      err = P.checkError;
      test = P.checkEvalTests;
      ffi = ffiLib(rt, rt.namespace);

      checkPassed = function(results) {
        var summary = ffi.checkResultsSummary(results);
        return summary.passed === rt.makeNumber(1)
          && summary.total === rt.makeNumber(1);
      };

      checkFailed = function(results) {
        var summary = ffi.checkResultsSummary(results);
        return summary.failed === rt.makeNumber(1)
          && summary.total === rt.makeNumber(1);
      };

      checkMessage = function(message) {
        return function(results) {
          var summary = ffi.checkResultsSummary(results);
          return summary.message.indexOf(message) > -1;
        };
      };
    });

    describe("is", function() {
      it("should check equality correctly", function(done) {
        test("check: 2 is 2 end",     checkPassed);
        test("check: 2 is 3 end",     checkFailed);
        test("check: 2 is-not 2 end", checkFailed);
        test("check: 2 is-not 3 end", checkPassed);
        P.wait(done);
      });
      it("should use refinements correctly", function(done) {
        test("check: 2 is%(_ < _) 2 end",     checkFailed);
        test("check: 2 is%(_ < _) 3 end",     checkPassed);
        test("check: 2 is-not%(_ < _) 2 end", checkPassed);
        test("check: 2 is-not%(_ < _) 3 end", checkFailed);
        P.wait(done);
      });
      it("should give good error messages", function(done) {
        test("check: 2 is 891 end",             checkMessage("not equal"));
        test("check: 2 is-not 2 end",           checkMessage("not different"));
        test("check: 2 is%(_ < _) -891 end",    checkMessage("not equal (using custom equality)"));
        test("check: 2 is-not%(_ < _) 891 end", checkMessage("not different (using custom equality)"));
        test("check: 2 is%(_ + _) 3 end",       checkMessage("boolean"));
        test("check: 2 is-not%(_ + _) 3 end",   checkMessage("boolean"));
        P.wait(done);
      });
    });

    describe("satisfies", function() {
      it("should use its predicate", function(done) {
        test("check: 2 satisfies      _ < 3 end", checkPassed);
        test("check: 2 satisfies      _ < 2 end", checkFailed);
        test("check: 2 satisfies-not  _ < 3 end", checkFailed);
        test("check: 2 satisfies-not  _ < 2 end", checkPassed);
        P.wait(done);
      });
      it("should give good error messages", function(done) {
        test("check: 2 satisfies      _ < 2 end", checkMessage("Predicate failed"));
        test("check: 2 satisfies-not  _ < 3 end", checkMessage("Predicate succeeded"));
        P.wait(done);
      });
    });

    describe("raises", function() {
      it("should catch exceptions", function(done) {
        test("check: raise('oops') raises 'op' end", checkPassed);
        test("check: raise('oops') raises 'po' end", checkFailed);
        P.wait(done);
      });
      it("should fail when no exception is raised", function(done) {
        test("check: 'oops' raises 'op' end", checkFailed);
        P.wait(done);
      });
      it("should give good error messages", function(done) {
        test("check: raise('oops') raises 'po' end", checkMessage("unexpected exception"));
        test("check: 'oops'        raises 'op' end", checkMessage("No exception raised"));
        P.wait(done);
      });
    });
  }
  return { performTest: performTest };
});
