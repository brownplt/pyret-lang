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

    function testGroup(label, thunk) {
      it(label, function(done) {
        thunk();
        P.wait(done);
      });
    }

    describe("is/is-not", function() {
      testGroup("should check equality correctly", function() {
        test("check: 2 is 2 end",     checkPassed);
        test("check: 2 is 3 end",     checkFailed);
        test("check: 2 is-not 2 end", checkFailed);
        test("check: 2 is-not 3 end", checkPassed);
      });
      testGroup("should use refinements correctly", function() {
        test("check: 2 is%(_ < _) 2 end",     checkFailed);
        test("check: 2 is%(_ < _) 3 end",     checkPassed);
        test("check: 2 is-not%(_ < _) 2 end", checkPassed);
        test("check: 2 is-not%(_ < _) 3 end", checkFailed);
      });
      testGroup("should give good error messages", function() {
        test("check: 2 is 891 end",             checkMessage("not equal"));
        test("check: 2 is-not 2 end",           checkMessage("not different"));
        test("check: 2 is%(_ < _) -891 end",    checkMessage("not equal (using custom equality)"));
        test("check: 2 is-not%(_ < _) 891 end", checkMessage("not different (using custom equality)"));
        test("check: 2 is%(_ + _) 3 end",       checkMessage("boolean"));
        test("check: 2 is-not%(_ + _) 3 end",   checkMessage("boolean"));
      });
    });

    describe("satisfies/violates", function() {
      testGroup("should use its predicate", function() {
        test("check: 2 satisfies _ < 3 end", checkPassed);
        test("check: 2 satisfies _ < 2 end", checkFailed);
        test("check: 2 violates  _ < 3 end", checkFailed);
        test("check: 2 violates  _ < 2 end", checkPassed);
      });
      testGroup("should give good error messages", function() {
        test("check: 2 satisfies _ < 2 end", checkMessage("Predicate failed"));
        test("check: 2 violates  _ < 3 end", checkMessage("Predicate succeeded"));
      });
    });

    describe("raises", function() {
      testGroup("should succeed on the given exception", function() {
        test("check: raise('oops') raises 'op' end", checkPassed);
      });
      testGroup("should fail on different exception", function() {
        test("check: raise('oops') raises 'po' end", checkFailed);
      });
      testGroup("should fail on no exception", function() {
        test("check: 'oops' raises 'op' end", checkFailed);
      });
      testGroup("should give good error messages", function() {
        test("check: raise('oops') raises 'po' end", checkMessage("unexpected exception"));
        test("check: 'oops'        raises 'op' end", checkMessage("No exception raised"));
      });
    });

    describe("raises-other-than", function() {
      testGroup("should succeed on different exception", function() {
        test("check: raise('spoo') raises-other-than 'op' end", checkPassed);
      });
      testGroup("should fail on the given exception", function() {
        test("check: raise('oops') raises-other-than 'op' end", checkFailed);
      });
      testGroup("should fail on no exception", function() {
        test("check: 'oops' raises-other-than 'op' end", checkFailed);
      });
      testGroup("should give good error messages", function() {
        test("check: raise('oops') raises-other-than 'op' end", checkMessage("expected it not to contain \"op\""));
        test("check: 'oops' raises-other-than 'op' end", checkMessage("No exception raised"));
      });
    });

    describe("does-not-raise", function() {
      testGroup("should succeed on no exception", function() {
        test("check: 3         does-not-raise end", checkPassed);
        test("check: 'raise'   does-not-raise end", checkPassed);
      });
      testGroup("should fail when an exception is raised", function() {
        test("check: raise('') does-not-raise end", checkFailed);
      });
      testGroup("should give good error messages", function() {
        test("check: raise('oops') does-not-raise end", checkMessage("unexpected exception"));
      });
    });

    describe("errors", function() {
      testGroup("should not report success when errors happen", function() {
        test("check: x :: String = 3\n  1 is 1 end", checkMessage("Ended in Error: 1"));
        test("fun f(x :: String): x end\n check: f(3) is 'oops' end", checkMessage("Ended in Error: 1"));
      });
    });
  }
  return { performTest: performTest };
});
