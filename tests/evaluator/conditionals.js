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
"fun f(): x := x + 'f'\n" + fres + " end\n" +
"fun g(): x := x + 'g'\n" + gres + " end\n"

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

      it("should introduce new scopes", function(done) {
        var check = "x = 5\n" +
                    "f = lam(): x end\n" +
                    "when true:\n" +
                      "shadow x = 10\n" +
                      "x\n" +
                    "end\n" +
                    "f()\n";
        same(check, rt.makeNumber(5));
        P.wait(done);
      });

    });

    describe("and", function() {
      it("should have the right binary truth table", function(done) {
        same("false and false", rt.pyretFalse);
        same("false and true", rt.pyretFalse);
        same("true and false", rt.pyretFalse);
        same("true and true", rt.pyretTrue);
        P.wait(done);
      });

      it("should work for long sequences", function(done) {
        same("false and false and false and false and false and false and false and false", rt.pyretFalse);
        same("false and false and false and false and false and false and false and true", rt.pyretFalse);
        same("false and false and false and true and false and false and false and false", rt.pyretFalse);
        same("true and true and true and true and true and true and true and true", rt.pyretTrue);
        same("true and true and true and true and true and true and true and false", rt.pyretFalse);
        P.wait(done);
      });

      it("should short-circuit at the right place in a sequence", function(done) {
        var xf = "var x = 0\n" +
                 "fun f(): x := x + 1\n true end\n";

        same("true and true and false and raise('explosions')", rt.pyretFalse);
        same(xf +
             "f() and f() and false and f()\n" +
             "x", rt.makeNumber(2));
        same(xf +
             "false and f() and f() and f()\n" +
             "x", rt.makeNumber(0));
        P.wait(done);

      });

      xit("should error if given a non-boolean, but still short-circuit", function(done) {
        var pte = function(e) {
          return rt.unwrap(e.exn).indexOf("Pyret Type Error") !== -1;
        };
        err("true and 5", pte);
        err("true and 'foo'", pte);
        same("false and 'foo'", rt.pyretFalse);
        same("false and block: raise('do not get here') end", rt.pyretFalse);
        P.wait(done);
      });
    });


    describe("or", function() {
      it("should have the right binary truth table", function(done) {
        same("false or false", rt.pyretFalse);
        same("false or true", rt.pyretTrue);
        same("true or false", rt.pyretTrue);
        same("true or true", rt.pyretTrue);
        P.wait(done);
      });

      it("should work for long sequences", function(done) {
        same("false or false or false or false or false or false or false or false", rt.pyretFalse);
        same("false or false or false or false or false or false or false or true", rt.pyretTrue);
        same("false or false or false or true or false or false or false or false", rt.pyretTrue);
        same("true or true or true or true or true or true or true or true", rt.pyretTrue);
        same("true or true or true or true or true or true or true or false", rt.pyretTrue);
        P.wait(done);
      });

      it("should short-circuit at the right place in a sequence", function(done) {
        var xf = "var x = 0\n" +
                 "fun f(): x := x + 1\n false end\n";

        same("false or false or true or raise('explosions')", rt.pyretTrue);
        same(xf +
             "f() or f() or true or f()\n" +
             "x", rt.makeNumber(2));
        same(xf +
             "true or f() or f() or f()\n" +
             "x", rt.makeNumber(0));
        P.wait(done);

      });
    });

    describe("when", function() {
      it("should only have its effect when true", function(done) {
        same("var x = 0\nwhen true: x := 4 end\nx", rt.makeNumber(4));
        same("var x = 0\nwhen false: x := 4 end\nx", rt.makeNumber(0));

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
