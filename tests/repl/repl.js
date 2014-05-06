var r = require("requirejs")
define(["q", "js/runtime-anf", "./../evaluator/eval-matchers", "../../src/js/base/repl-lib", "js/ffi-helpers", 
       "js/dialects-lib"], function(Q, rtLib, e, repl, ffiLib, dialectsLib) {

  var _ = require('jasmine-node');
  var rt;
  var P;
  var same;
  var err;
  var aRepl;
  var ffi;
  var replCount = 0;
  function getVal(result) {
    if(!rt.isSuccessResult(result)) {
      console.error("Tried to getVal of non-SuccessResult: ", result, result.exn ? result.exn.stack : "No stack");
      throw result.exn;
    }
    return rt.getField(result.result, "answer");
  }
  function getChecks(result) {
    if(!rt.isSuccessResult(result)) {
      console.error("Tried to getChecks of non-SuccessResult: ", result, result.exn);
      throw result.exn;
    }
    return ffi.toArray(rt.getField(result.result, "checks"));
  }

  function performTest() {

    beforeEach(function() {
      rt = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
      P =  e.makeEvalCheckers(this, rt);
      same = P.checkEvalsTo;
      err = P.checkError;
      ffi = ffiLib(rt, rt.namespace);
      dialects = dialectsLib(rt, rt.namespace);
      dialect = "Pyret";
      dialectConfig = dialects.dialects[dialect];
      aRepl = repl.create(rt, dialectConfig.makeNamespace(rt), dialectConfig.compileEnv, { name: "repl-test" + replCount++, dialect: dialect});
    });

    describe("repl", function() {
      it("should initialize and run simple programs", function(done) {
        aRepl.restartInteractions("x = 10\nx").then(function(replResult) {
          expect(getVal(replResult)).toBeSameAs(rt, rt.makeNumber(10));
          return aRepl.run("x");
        }).then(function(replResult) {
          expect(getVal(replResult)).toBeSameAs(rt, rt.makeNumber(10));
          return aRepl.run("y");
        }).then(function(replResult) {
          expect(replResult).toPassPredicate(rt.isFailureResult);
          return aRepl.run("y = fun(): x + 1 end");
        }).then(function(replResult) {
          expect(replResult).toPassPredicate(rt.isSuccessResult);
          return aRepl.run("y()");
        }).then(function(replResult) {
          expect(getVal(replResult)).toBeSameAs(rt, rt.makeNumber(11));
          done();
        }).catch(function(err) {
          console.error("Failure: ", err);
          fail();
        });
      }, 10000);

      it("should allow recursive references in the same block", function(done) {
        aRepl.restartInteractions("");
        aRepl.run("fun even(a): if a == 1: false else: odd(a - 1) end end\n" +
                  "fun odd(a): if a == 1: true else: even(a - 1) end end");
        aRepl.run("even(5)").then(function(replResult) {
          expect(getVal(replResult)).toBeSameAs(rt, rt.makeBoolean(false));
        }); 
        aRepl.run("odd(7)").then(function(replResult) {
          expect(getVal(replResult)).toBeSameAs(rt, rt.makeBoolean(true));
          done();
        });
      });

      it("should not allow unbound ids, even in functions", function(done) {
        aRepl.restartInteractions("");
        aRepl.run("fun even(a): if a == 1: false else: odd(a - 1) end end")
          .then(function(replResult) {
            expect(replResult).toPassPredicate(rt.isFailureResult);
          });
        aRepl.run("fun odd(a): if a == 1: true else: even(a - 1) end end")
          .then(function(replResult) {
            expect(replResult).toPassPredicate(rt.isFailureResult);
            done();
          });
      });

      it("should include check results", function(done) {
        aRepl.restartInteractions("check: 1 is 1; check: 2 is 2;")
          .then(function(replResult) {
            expect(getChecks(replResult).length).toBe(2);
            return aRepl.run("check: 4 is 5 end")
          }).then(function(replResult) {
            expect(getChecks(replResult).length).toBe(1);
            done();
          }).catch(function(err) {
            console.error("Failed in testing checks: ", err);
            fail();
          });
      });

      it("should allow for shadowing only with the 'shadow' keyword", function(done) {
        aRepl.restartInteractions("x = 5")
          .then(function(replResult) {
            expect(getVal(replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
            return aRepl.run("x = 10")
          }).then(function(replResult) {
            expect(replResult).toPassPredicate(rt.isFailureResult);
            return aRepl.run("shadow x = 10")
          }).then(function(replResult) {
            expect(getVal(replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
            return aRepl.run("x")
          }).then(function(replResult) {
            expect(getVal(replResult)).toBeSameAs(rt, rt.makeNumber(10));
            done();
          }).catch(function(err) {
            console.error("Failed in testing shadowing: ", err);
            fail();
          });
      });

      it("should not allow forward references", function(done) {
        aRepl.restartInteractions("")
          .then(function(replResult) {
            expect(getVal(replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
            return aRepl.run("fun f(): forward-reference end")
          }).then(function(replResult) {
            expect(replResult).toPassPredicate(rt.isFailureResult);
            done();
          }).catch(function(err) {
            console.error("Failed in testing shadowing: ", err);
            fail();
          });
      });

      it("should allow recursive references in the same block", function(done) {
        aRepl.restartInteractions("")
          .then(function(replResult) {
            expect(getVal(replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
            return aRepl.run("fun f(): g(4) end\nfun g(x): x end\nf()");
          }).then(function(replResult) {
            expect(getVal(replResult)).toBeSameAs(rt, rt.makeNumber(4));
            done();
          }).catch(function(err) {
            console.error("Failed in testing shadowing: ", err);
            fail();
          });
      });

      it("should allow restarting of interactions to clear state", function(done) {
        aRepl.restartInteractions("x = 5")
          .then(function(replResult) {
            expect(getVal(replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
            return aRepl.run("x = 10")
          }).then(function(replResult) {
            expect(replResult).toPassPredicate(rt.isFailureResult);
            return aRepl.restartInteractions("not-x = 5");
          }).then(function(replResult) {
            expect(getVal(replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
            return aRepl.run("x = 22");
          }).then(function(replResult) {
            expect(getVal(replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
            return aRepl.run("x");
          }).then(function(replResult) {
            expect(getVal(replResult)).toBeSameAs(rt, rt.makeNumber(22));
            done();
          }).catch(function(err) {
            console.error("Failed in testing shadowing: ", err);
            fail();
          });

      });

      it("should allow stopping", function(done) {
        aRepl.restartInteractions("fun fact(n): if n < 1: 1 else: n * fact(n - 1);;").then(function(replResult) {
          setTimeout(function() {
              aRepl.stop();
            }, 1000);
          return aRepl.run("fact(100000)");
        }).then(function(result) {
          expect(result).toPassPredicate(rt.isFailureResult)
          expect(result.exn.exn).toPassPredicate(rt.ffi.isUserBreak);
          done();
        }).fail(function(err) { fail(); });
      });

      it("should allow stopping nested inside tasks", function(done) {
        aRepl.restartInteractions("fun fact(n): if n < 1: 1 else: n * fact(n - 1);;").then(function(replResult) {
          setTimeout(function() {
              aRepl.stop();
            }, 1000);
          return aRepl.run("run-task(fun(): fact(100000) 'done' end)");
        }).then(function(result) {
          expect(result).toPassPredicate(rt.isFailureResult);
          expect(result.exn.exn).toPassPredicate(rt.ffi.isUserBreak);
          done();
        }).fail(function(err) { fail(); });
      }, 10000);
    });

  }

  return { performTest: performTest };
});
