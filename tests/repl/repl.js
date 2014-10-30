var r = require("requirejs")
define(["q", "js/runtime-anf", "./../evaluator/eval-matchers", "../../src/js/base/repl-lib", "js/ffi-helpers", 
       "js/dialects-lib"], function(Q, rtLib, e, repl, ffiLib, dialectsLib) {

  var J = require('jasmine-node');
  var rt;
  var P;
  var same;
  var err;
  var aRepl;
  var ffi;
  var replCount = 0;
  function getVal(rt, result) {
    if(!rt.isSuccessResult(result)) {
      console.error("Tried to getVal of non-SuccessResult: ", result, result.exn ? result.exn.stack : "No stack");
      throw result.exn;
    }
    return rt.getField(result.result, "answer");
  }
  function getChecks(rt, result) {
    if(!rt.isSuccessResult(result)) {
      console.error("Tried to getChecks of non-SuccessResult: ", result, result.exn);
      throw result.exn;
    }
    return ffi.toArray(rt.getField(result.result, "checks"));
  }

  function performTest() {
    var dialect;

    beforeEach(function(done) {
      rt = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
      P =  e.makeEvalCheckers(this, rt);
      same = P.checkEvalsTo;
      err = P.checkError;
      ffi = ffiLib(rt, rt.namespace);
      var dialectP = Q.defer();
      rt.runThunk(function() {
        return dialectsLib(rt, rt.namespace);
      }, function(dResult) {
        dialectP.resolve(dResult.result.dialects["Pyret"]);
      });
      aRepl = dialectP.promise.then(function(dialectConfig) {
        done();
        return repl.create(rt, dialectConfig.makeNamespace(rt), dialectConfig.compileEnv, { name: "repl-test" + replCount++, dialect: "Pyret"});
      });
      aRepl.fail(function(err) {
        console.error("Failed to create repl: ", err);
      });
    });

    describe("repl", function() {
      it("should initialize and run simple programs", function(done) {
        aRepl.then(function(aRepl) {
          var rt = aRepl.runtime;
          aRepl.restartInteractions("x = 10\nx").then(function(replResult) {
            expect(getVal(rt, replResult)).toBeSameAs(rt, rt.makeNumber(10));
            return aRepl.run("x");
          }).then(function(replResult) {
            expect(getVal(rt, replResult)).toBeSameAs(rt, rt.makeNumber(10));
            return aRepl.run("y");
          }).then(function(replResult) {
            expect(replResult).toPassPredicate(rt.isFailureResult);
            return aRepl.run("y = lam(): x + 1 end");
          }).then(function(replResult) {
            expect(replResult).toPassPredicate(rt.isSuccessResult);
            return aRepl.run("y()");
          }).then(function(replResult) {
            expect(getVal(rt, replResult)).toBeSameAs(rt, rt.makeNumber(11));
            done();
          }).catch(function(err) {
            console.error("Failure: ", err);
            fail();
          });
        });
      }, 10000);

      it("should not allow unbound ids, even in functions", function(done) {
        aRepl.then(function(aRepl) {
          var rt = aRepl.runtime;
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
      });

      it("should not allow unbound ids, even in functions", function(done) {
        aRepl.then(function(aRepl) {
          var rt = aRepl.runtime;
          aRepl.restartInteractions("");
          aRepl.run("import lists as L")
            .then(function(replResult) {
              expect(replResult).toPassPredicate(rt.isSuccessResult);
            });
          aRepl.run("is-object(L)")
            .then(function(replResult) {
              expect(replResult).toPassPredicate(rt.isSuccessResult);
              done();
            });
        });
      });

      it("should include check results", function(done) {
        aRepl.then(function(aRepl) {
          var rt = aRepl.runtime;
          aRepl.restartInteractions("check: 1 is 1;\ncheck: 2 is 2;")
            .then(function(replResult) {
              expect(getChecks(rt, replResult).length).toBe(2);
              return aRepl.run("check: 4 is 5 end")
            }).then(function(replResult) {
              expect(getChecks(rt, replResult).length).toBe(1);
              done();
            }).catch(function(err) {
              console.error("Failed in testing checks: ", err);
              fail();
            });
        });
      });

      it("should allow for shadowing only with the 'shadow' keyword", function(done) {
        aRepl.then(function(aRepl) {
          var rt = aRepl.runtime;
          aRepl.restartInteractions("x = 5")
            .then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
              return aRepl.run("x = 10")
            }).then(function(replResult) {
              expect(replResult).toPassPredicate(rt.isFailureResult);
              return aRepl.run("shadow x = 10")
            }).then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
              return aRepl.run("x")
            }).then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.makeNumber(10));
              done();
            }).catch(function(err) {
              console.error("Failed in testing shadowing: ", err);
              fail();
            });
        });
      }, 20000);

      it("should not allow forward references", function(done) {
        aRepl.then(function(aRepl) {
          var rt = aRepl.runtime;
          aRepl.restartInteractions("")
            .then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
              return aRepl.run("fun f(): forward-reference end")
            }).then(function(replResult) {
              expect(replResult).toPassPredicate(rt.isFailureResult);
              done();
            }).catch(function(err) {
              console.error("Failed in testing forward references ", err);
              fail();
            });
        });
      });

      it("should have builtin modules available at the repl", function(done) {
        aRepl.then(function(aRepl) {
          var rt = aRepl.runtime;
          aRepl.restartInteractions("")
            .then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
              return aRepl.run("x :: lists.List = empty")
            }).then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
              done();
            }).catch(function(err) {
              console.error("Failed in testing builtin-modules binding: ", err, err.exn.dict.reason);
              fail();
            });
        });
      });

      it("should bind type names", function(done) {
        aRepl.then(function(aRepl) {
          var rt = aRepl.runtime;
          aRepl.restartInteractions("data D: | var1 end")
            .then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
              return aRepl.run("x :: D = var1")
            }).then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
              done();
            }).catch(function(err) {
              console.error("Failed in testing typename binding: ", err); 
              fail();
            });
        });
      }, 15000);

      it("should allow recursive references in the same block", function(done) {
        aRepl.then(function(aRepl) {
          var rt = aRepl.runtime;
          aRepl.restartInteractions("")
            .then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
              return aRepl.run("fun f(): g(4) end\nfun g(x): x end\nf()");
            }).then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.makeNumber(4));
              done();
            }).catch(function(err) {
              console.error("Failed in testing recursive references ", err);
              fail();
            });
        });
      }, 30000);

      it("should allow restarting of interactions to clear state", function(done) {
        aRepl.then(function(aRepl) {
          var rt = aRepl.runtime;
          aRepl.restartInteractions("x = 5")
            .then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
              return aRepl.run("x = 10")
            }).then(function(replResult) {
              expect(replResult).toPassPredicate(rt.isFailureResult);
              return aRepl.restartInteractions("not-x = 5");
            }).then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
              return aRepl.run("x = 22");
            }).then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.namespace.get("nothing"));
              return aRepl.run("x");
            }).then(function(replResult) {
              expect(getVal(rt, replResult)).toBeSameAs(rt, rt.makeNumber(22));
              done();
            }).catch(function(err) {
              console.error("Failed in testing restarting interactions: ", err);
              fail();
            });
        });
      }, 30000);

      it("should allow stopping", function(done) {
        aRepl.then(function(aRepl) {
          var rt = aRepl.runtime;
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
      });

      it("should allow stopping nested inside tasks", function(done) {
        aRepl.then(function(aRepl) {
          var rt = aRepl.runtime;
          aRepl.restartInteractions("fun fact(n): if n < 1: 1 else: n * fact(n - 1);;").then(function(replResult) {
            setTimeout(function() {
                aRepl.stop();
              }, 1000);
            return aRepl.run("run-task(lam(): fact(100000)\n 'done' end)");
          }).then(function(result) {
            expect(result).toPassPredicate(rt.isFailureResult);
            expect(result.exn.exn).toPassPredicate(rt.ffi.isUserBreak);
            done();
          }).fail(function(err) { fail(); });
        });
      }, 10000);
    });

  }

  return { performTest: performTest };
});
