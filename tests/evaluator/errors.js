var r = require("requirejs")
define(["js/runtime-anf", "./eval-matchers"], function(rtLib, e) {

  var _ = require('jasmine-node');
  var rt;
  var P;
  var err;
  var same;

  function performTest() {

    beforeEach(function() {
      rt = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
      P =  e.makeEvalCheckers(this, rt);
      err = P.checkError;
      same = P.checkEvalsTo;
    });

    describe("run-task", function() {
      it("should work for normal computation", function(done) {
        var prog =
"cases(Either) run-task(fun(): 5 end):\n" +
"    | left(v) => v\n" +
"    | right(exn) => 'fail'\n" +
"end";
        same(prog, rt.makeNumber(5));

        P.wait(done);
        
      });
      it("should work for exceptional computation", function(done) {
        var prog =
"cases(Either) run-task(fun(): raise('ahoy') end):\n" +
"    | left(v) => 'fail'\n" +
"    | right(exn) => exn\n" +
"end";
        same(prog, rt.makeString('ahoy'));
        
        P.wait(done);
      });
    });

    xdescribe("lookup-number", function() {
      it("should signal an error for looking up fields on numbers", function(done) {
        err("5.x", function(e) {
          return rt.unwrap(e.exn).indexOf("looked up field x on 5, which does not have any fields") !== -1;
        });
        P.wait(done);
      });
    });

    describe("compiler", function() {
      it("should signal an error when the compile fails", function(done) {
        P.checkCompileError("fun(): x = 5 y = 10 end", function(e) {
            expect(e.length).toEqual(1);
            return true;
          });
        P.wait(done);
      });
    });

    describe("unbound ids", function() {
      it("should notice unbound ids", function(done) {
        P.checkCompileError("z", function(e) {
          expect(e.length).toEqual(1);
          return true;
        });
        P.wait(done);
      });
    });

    describe("shadowing", function() {
      it("should notice shadowed builtins", function(done) {
        P.checkCompileError("fun(x): x = 5 x end", function(e) {
          expect(e.length).toEqual(1);
          return true;
        });
        P.checkCompileError("string-contains = 5", function(e) {
          expect(e.length).toEqual(1);
          return true;
        });
        P.checkEvalsTo("shadow string-contains = 5\nstring-contains", rt.makeNumber(5));
        P.wait(done);
      });
    });

  }
  return { performTest: performTest };
});
