var r = require("requirejs")
define(["js/runtime-anf", "./eval-matchers"], function(rtLib, e) {

  var _ = require('jasmine-node');
  var rt;
  var P;
  var same;
  var err;

  function performTest() {

    beforeEach(function() {
      rt = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
      P =  e.makeEvalCheckers(this, rt);
      same = P.checkEvalsTo;
      err = P.checkError;
    });
    describe("if", function() {
      it("should dispatch on true and false", function(done) {

        same("if true: 5 else: 10 end", rt.makeNumber(5));
        same("if false: 5 else: 10 end", rt.makeNumber(10));

        P.wait(done);

      });

      it("should have any effects exactly once", function(done) {
        var xfg = function(fres, gres) {
return "var x = ''\n" +
"fun f(): x := x + 'f' " + fres + " end\n" +
"fun g(): x := x + 'g' " + gres + " end\n"

        };

        same(
xfg("true", "true") +
"if if f(): g() else: false end: x else: 4 end", rt.makeString("fg"))

        same(
xfg("false", "true") +
"if if f(): g() else: false end: x else: x + 'else' end\n", rt.makeString("felse"))

        same(
xfg("true", "false") +
"if if f(): g() else: false end: x else: x + 'else' end\n", rt.makeString("fgelse"))

        same(
xfg("false", "true") +
"if if f(): f() else: g() end: x else: x + 'else' end\n", rt.makeString("fg"))

        P.wait(done);
      });

    });

    describe("Booleans", function() {
      it("Correct and", function(done) {
        same("true and true", rt.pyretTrue);
        same("true and false", rt.pyretFalse);
        same("false and true", rt.pyretFalse);
        same("false and false", rt.pyretFalse);
        P.wait(done);
      });
      it("Correct or", function(done) {
        same("true or true", rt.pyretTrue);
        same("true or false", rt.pyretTrue);
        same("false or true", rt.pyretTrue);
        same("false or false", rt.pyretFalse);
        P.wait(done);
      });
      it("should error if given a non-boolean, but still short-circuit", function(done) {
        var pte = function(e) {
          return rt.unwrap(e.exn).indexOf("Pyret Type Error") !== -1;
        };
        err("false or 5", pte);
        err("5 or true", pte);
        err("false or 'foo'", pte);
        same("true or 'foo'", rt.pyretTrue);
        same("true or block: raise('do not get here') end", rt.pyretTrue)

        err("true and 5", pte);
        err("5 and true", pte);
        err("true and 'foo'", pte);
        same("false and 'foo'", rt.pyretFalse);
        same("false and block: raise('do not get here') end", rt.pyretFalse)

        P.wait(done);
      });
    });

    describe("when", function() {
      it("should only have its effect when true", function(done) {
        same("var x = 0 when true: x := 4 end x", rt.makeNumber(4));
        same("var x = 0 when false: x := 4 end x", rt.makeNumber(0));

        P.wait(done);
      });

      it("should evaluate to nothing no matter what", function(done) {

        same("when true: 5 end", rt.namespace.get("nothing"));
        same("when false: 5 end", rt.namespace.get("nothing"));

        P.wait(done);

      });
    });
  }
  return { performTest: performTest };
});
