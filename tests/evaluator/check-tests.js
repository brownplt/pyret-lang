var r = require("requirejs")
define(["js/runtime-anf", "./eval-matchers", "../../src/js/base/ffi-helpers"], function(rtLib, e, ffiLib) {

  var _ = require('jasmine-node');
  var rt = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
  var P;
  var same;
  var err;
  var tests;
  var ffi;

  function performTest() {

    beforeEach(function() {
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
        test("check: raise('oops') raises-other-than 'op' end", checkMessage("expected it not to contain"));
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

    describe("raises-satisfies/raises-violates", function() {
      testGroup("should succeed/fail based on the predicate", function() {
        test("check: raise('oops') raises-satisfies _ == 'oops' end", checkPassed);
        test("check: raise('spoo') raises-satisfies _ == 'oops' end", checkFailed);
        test("check: raise('oops') raises-violates  _ == 'oops' end", checkFailed);
        test("check: raise('spoo') raises-violates  _ == 'oops' end", checkPassed);
      });
      testGroup("should fail on no exception", function() {
        test("check: 'oops' raises-satisfies _ == 'oops' end", checkFailed);
        test("check: 'oops' raises-violates  _ == 'oops' end", checkFailed);
      });
      testGroup("should give good error messages", function() {
        test("check: raise('spoo') raises-satisfies _ == 'oops' end",  checkMessage("Predicate failed for exception"));
        test("check: raise('oops') raises-violates  _ == 'oops' end",  checkMessage("Predicate succeeded for exception"));
        test("check: 'oops'        raises-satisfies _ == 'oops'  end", checkMessage("No exception raised"));
        test("check: 'oops'        raises-violates  _ == 'oops'  end", checkMessage("No exception raised"));
      });
    });

    describe("nested check blocks", function() {
      testGroup("should run nested blocks", function() {
        test(
"fun f(y):" +
"  fun g(x):\n" +
"    x\n" +
"  where:\n" +
"    g(y) is y\n" +
"  end\n" +
"  g(y)\n" +
"where:\n" +
"  f(4) is 5\n" +
"  f(4) is 5\n" +
"  f(5) is 6\n" +
"end\n", checkMessage("Passed: 3; Failed: 3; Ended in Error: 0; Total: 6"));
      });

      testGroup("should not fail if nested blocks error", function() {
        test(
"fun f(y):" +
"  fun g(x):\n" +
"    x\n" +
"  where:\n" +
"    g(y) is raise('a')\n" +
"  end\n" +
"  g(y)\n" +
"where:\n" +
"  f(4) is 5\n" +
"  f(4) is 5\n" +
"  f(5) is 6\n" +
"end\n", checkMessage("Passed: 0; Failed: 3; Ended in Error: 3; Total: 3"));
      });

      testGroup("should report failures in nested blocks", function() {
        test(
"fun f(y):" +
"  fun g(x):\n" +
"    x\n" +
"  where:\n" +
"    g(y) is y + 1\n" +
"  end\n" +
"  g(y)\n" +
"where:\n" +
"  f(4) is 4\n" +
"  f(4) is 4\n" +
"  f(5) is 4\n" +
"end\n", checkMessage("Passed: 2; Failed: 4; Ended in Error: 0; Total: 6"));
      });

      testGroup("should report failures in parallel nested blocks", function() {
        test(
"fun f(y):" +
"  fun h(x):\n" +
"    x\n" +
"  where:\n" +
"    h(y) is raise('a')\n" +
"  end\n" +
"  fun g(x):\n" +
"    x\n" +
"  where:\n" +
"    g(y) is y + 1\n" +
"  end\n" +
"  g(y)\n" +
"where:\n" +
"  f(4) is 4\n" +
"  f(4) is 4\n" +
"  f(5) is 4\n" +
"  f(4) is 4\n" +
"end\n", checkMessage("Passed: 3; Failed: 5; Ended in Error: 4; Total: 8"));
      });

      testGroup("should report failures in parallel nested blocks", function() {
        test(
"fun f(y):" +
"  fun g(x):\n" +
"    fun h(shadow x):\n" +
"      x\n" +
"    where:\n" +
"      h(y) is 4\n" +
"    end\n" +
"    x\n" +
"  where:\n" +
"    g(y) is y + 1\n" +
"  end\n" +
"  g(y)\n" +
"where:\n" +
"  f(4) is 4\n" +
"  f(5) is 4\n" +
"  f(4) is 4\n" +
"end\n", checkMessage("Passed: 6; Failed: 6; Ended in Error: 0; Total: 12"));
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
