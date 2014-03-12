var r = require("requirejs")
define(["js/runtime-anf", "./eval-matchers", "./eval"], function(rtLib, e, pyeval) {

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
    describe("Stack height", function() {
      it("should work for simple is", function(done) {
        var currentStackDepth = 0;
        try {
          function foo() { try { if(Math.random()) { currentStackDepth++; foo(); } else { currentStackDepth++; foo(); } } catch(e) { throw e; } }
          foo();
        } catch(e) {
          if(!(e instanceof RangeError)) { fail(); }
        }
        console.log("Calculated stack depth: ", currentStackDepth);
        currentStackDepth = 8000;
        console.log("Current stack depth: ", currentStackDepth);

        function repeat(s, n) { return new Array(n + 1).join(s); }
        var counter = "fun f(n): if n < 1: 0 else: f(n - 1);; \n f(" + (currentStackDepth) + ")";
        var cond = "(n < 1) or false";
        var condor = cond + " or ";
        var counterOr = "fun f(n): if " + cond + ": 0 else: f(n - 1);; \n f(" + (currentStackDepth) + ")";
        var counterOr5 = "fun f(n): if " + repeat(condor, 5) + cond + ": 0 else: f(n - 1);; \n f(" + (currentStackDepth) + ")";
        var counterOr15 = "fun f(n): if " + repeat(condor, 15) + cond + ": 0 else: f(n - 1);; \n f(" + (currentStackDepth) + ")";
        var counterOr30 = "fun f(n): if " + repeat(condor, 30) + cond + ": 0 else: f(n - 1);; \n f(" + (currentStackDepth) + ")";
        var counterOr45 = "fun f(n): if " + repeat(condor, 45) + cond + ": 0 else: f(n - 1);; \n f(" + (currentStackDepth) + ")";
        var counterOr100 = "fun f(n): if " + repeat(condor, 100) + cond + ": 0 else: f(n - 1);; \n f(" + (currentStackDepth) + ")";
        var counterOr200 = "fun f(n): if " + repeat(condor, 200) + cond + ": 0 else: f(n - 1);; \n f(" + (currentStackDepth) + ")";
        var counterOr500 = "fun f(n): if " + repeat(condor, 500) + cond + ": 0 else: f(n - 1);; \n f(" + (currentStackDepth) + ")";
        var mutual = "fun f(n): if (n < 1): 0 else: g(n - 1);;\n" +
                     "fun g(n): if (n < 1): 0 else: f(n - 1);;\n" + "f(" + (currentStackDepth) + ")";
        var tests = {
            counter: counter,
            counterOr: counterOr,
            counterOr5: counterOr5,
            counterOr15: counterOr15,
            counterOr30: counterOr30,
            counterOr45: counterOr45,
            counterOr100: counterOr100,
            counterOr200: counterOr200,
            counterOr500: counterOr500,
            mutual: mutual,
          };

        var donecount = 0;

        pyeval.evalPyret(rt, counter, { gas: currentStackDepth * 2 }, function(result) {
            expect(result).toBeFailure(rt);
            console.log(result);
            expect(result.exn).toBeInstanceOf(RangeError);
            donecount++;
          });
        function tryHeight(name, program, n, after) {
          pyeval.evalPyret(rt, program, { gas: n }, function(result) {
              if(rt.isSuccessResult(result)) {
                console.log("Succeeded at height: ", n);
                after(name, n, result);
              }
              else {
                setTimeout(function() { tryHeight(name, program, n - 100, after); }, 0);
              }
            });
        }
        function tryHeights(keys, results, after) {
          if (keys.length === 0) { after(results); return; }
          var k = keys.pop();
          tryHeight(k, tests[k], currentStackDepth, function(name, n, result) {
            results.push({name: name, n: n, result: result});
            setTimeout(function() {
              tryHeights(keys, results, after);
            }, 0);
          });
        }
        tryHeights(Object.keys(tests), [], function(results) {
            console.log(results);
            for(var i = 0; i < results.length - 1; i++) {
              expect(results[i].n).toEqual(results[i + 1].n);
            }
            done();
          });

      }, 30000);
    });

  }
  return {performTest: performTest};
});

