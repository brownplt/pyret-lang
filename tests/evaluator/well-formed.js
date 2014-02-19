var r = require("requirejs")
define(["js/runtime-anf", "./eval-matchers"], function(rtLib, e) {

  var _ = require('jasmine-node');
  var rt;
  var P;

  function wf_check(s) { 
    return "where: blocks only allowed on named function declarations and data, not on " + s; 
  }
  function performTest() {

    beforeEach(function() {
      rt = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
      P =  e.makeEvalCheckers(this, rt);
    });
    describe("Well-formedness", function() {
      xit("should be well-formed", function(done) {
        P.checkEvalsTo("true and false and true", rt.makeBoolean(false));
        P.checkEvalsTo("1 + 2 + 3 + 4", rt.makeNumber(10));
        P.checkEvalsTo("fun foo():\n" + 
                       " var x = 10\n" + 
                       " x\n" + 
                       "end\n" + 
                       "10", 
                       rt.makeNumber(10));
        // returns a number because we are really just checking OK parse/wf,
        // and this is (void) otherwise
        P.checkEvalsTo("fun f(): nothing where: 5 + 2 is 7 end 42", rt.makeNumber(42));
        P.checkEvalsTo("fun f(): nothing where: 1 is 2 end 10", rt.makeNumber(10));

        P.wait(done);
      });
      xit("mixed operators should be malformed", function(done) {
        P.checkErrorMsg("true and not false", "Cannot have nested bare `not`");
        P.checkErrorMsg("true and false or true", "Cannot mix binary operators of different types");
        P.checkErrorMsg("true and false and not true and false", "Cannot have nested bare `not`");
        P.checkErrorMsg("1 + 2 - 3", "Cannot mix binary operators of different types");
        P.checkErrorMsg("1 + 2 + 3 * 4", "Cannot mix binary operators of different types");
        P.checkErrorMsg("1 / 2 + 3 * 4 - 5", "Cannot mix binary operators of different types");

        P.wait(done);
      });
      xit("nullary methods", function(done) {
        P.checkErrorMsg("method(): end", "Cannot have a method with zero arguments");
        P.checkErrorMsg("{foo(): end}", "Cannot have a method with zero arguments");

        P.wait(done);
      });
      it("malformed blocks", function(done) {
        // P.checkErrorMsg("fun foo():\n" + 
        //                 " x = 10\n" + 
        //                 "end\n" + 
        //                 "10", 
        //                 "Cannot end a block in a let-binding");
        // P.checkErrorMsg("fun foo():\n" + 
        //                 " var x = 10\n" + 
        //                 "end\n" + 
        //                 "10", 
        //                 "Cannot end a block in a var-binding");
        // P.checkErrorMsg("fun foo():\n" + 
        //                 " fun f(): end\n" + 
        //                 "end\n" + 
        //                 "10",
        //                 "Cannot end a block in a fun-binding");
        // P.checkErrorMsg("fun: x = 5 end", "Cannot end a block in a let-binding");
        // P.checkErrorMsg("fun: var x = 5 end", "Cannot end a block in a var-binding");
        // P.checkErrorMsg("fun: fun f(): end end", "Cannot end a block in a fun-binding");
        // P.checkErrorMsg("fun: x = 5 fun f(): end end", "Cannot end a block in a fun-binding");
        // P.checkErrorMsg("fun: var x = 5 y = 4 fun f(): end end", "Cannot end a block in a fun-binding");


        // // NOTE(dbp 2013-08-09): The more "obvious" occurence of these two get
        // // caught by typechecking / parsing.
        // P.checkErrorMsg("check = 1 check", "Cannot use `check` as an identifier.");
        // P.checkErrorMsg("where = 1 fun(): where end", "Cannot use `where` as an identifier.");
        // P.checkErrorMsg("fun: 1 is 2 end", "Cannot use `is` outside of a `check` or `where` block.");
        // P.checkErrorMsg("fun: 1 raises 2 end", "Cannot use a check-test form outside of a `check` or `where` block.");

        // P.checkErrorMsg("fun:\n" + 
        //                 "  data D:\n" + 
        //                 "    | var1()\n" + 
        //                 "  end\n" + 
        //                 "end",
        //                 "Cannot end a block with a data definition");
        // P.checkErrorMsg("fun:\n" + 
        //                 "  y = 10\n" + 
        //                 "  x = 5\n" + 
        //                 "  fun f(): end\n" + 
        //                 "  data D:\n" + 
        //                 "    | var1()\n" + 
        //                 "  end\n" + 
        //                 "end",
        //                 "Cannot end a block with a data definition");
        // P.checkErrorMsg("fun:\n" + 
        //                 "  y = 10\n" + 
        //                 "  x = 5\n" + 
        //                 "  fun f(): end\n" + 
        //                 "  graph:\n" + 
        //                 "  z = 5\n" + 
        //                 "  end\n" + 
        //                 "end",
        //                 "Cannot end a block with a graph definition");
        // P.checkErrorMsg("block:\n" + 
        //                 "  x = 5\n" + 
        //                 "  y = 10\n" + 
        //                 "end",
        //                 "Cannot end a block in a let-binding");
        // P.checkErrorMsg("block:\n" + 
        //                 "  x = 5\n" + 
        //                 "  graph: y = 10 end\n" + 
        //                 "end",
        //                 "Cannot end a block with a graph definition");
        // P.checkErrorMsg("if x < y:\n" + 
        //                 "  print('x less than y')\n" + 
        //                 "end",
        //                 "Cannot have an if with a single branch");

        // P.checkErrorMsg("fun(): where: 5 end", "Empty block");
        //P.checkErrorMsg("fun(): true where: 5 end", wf_check("anonymous functions"));
        // P.checkErrorMsg("method(self): where: 5 end", wf_check("methods"));
        // P.checkErrorMsg("{m(self): where: 5 end}", wf_check("methods"));
        P.checkErrorMsg("datatype Foo:\n" +
                        "  | foo() with constructor(self): self end\n" +
                        "  | foo with constructor(self): self end\n" +
                        "end",
                        "Constructor name foo appeared more than once.");

        P.checkErrorMsg("datatype Foo:\n" +
                        "  | foo() with constructor(self): self end\n" +
                        "  | bar() with constructor(self): self end\n" +
                        "  | baz() with constructor(self): self end\n" +
                        "  | foo(a) with constructor(self): self end\n" +
                        "end",
                        "Constructor name foo appeared more than once.");

        P.checkErrorMsg("datatype Foo:\n" +
                        "  | bang with constructor(self): self end\n" +
                        "  | bar() with constructor(self): self end\n" +
                        "  | bang() with constructor(self): self end\n" +
                        "  | foo() with constructor(self): self end\n" +
                        "  | foo(a) with constructor(self): self end\n" +
                        "end",
                        "Constructor name bang appeared more than once.");

        P.checkErrorMsg("cases(List) []:\n" +
                        "  | empty => 1\n" +
                        "  | empty => 2\n" +
                        "end",
                        "Duplicate case for empty");

        P.checkErrorMsg("cases(List) []:\n" +
                        "  | empty => 1\n" +
                        "  | link(f, r) => 2\n" +
                        "  | empty => 2\n" +
                        "end",
                        "Duplicate case for empty");

        P.checkErrorMsg("cases(List) []:\n" +
                        "  | empty => 1\n" +
                        "  | empty => 2\n" +
                        "  | else => 3\n" +
                        "end",
                        "Duplicate case for empty");

        P.checkErrorMsg("cases(List) []:\n" +
                        "  | link(f, r) => 2\n" +
                        "  | bogus => 'bogus'\n" +
                        "  | bogus2 => 'bogus'\n" +
                        "  | empty => 1\n" +
                        "  | bogus3 => 'bogus'\n" +
                        "  | empty => 2\n" +
                        "  | else => 3\n" +
                        "end",
                        "Duplicate case for empty");

        P.checkErrorMsg("cases(List) []:\n" +
                        "  | empty => 2\n" +
                        "  | bogus => 'bogus'\n" +
                        "  | bogus2 => 'bogus'\n" +
                        "  | link(f, r) => 1\n" +
                        "  | bogus3 => 'bogus'\n" +
                        "  | link(_, _) => 2\n" +
                        "end",
                        "Duplicate case for link");

        P.wait(done);
      });
    });
  }
  return { performTest: performTest };
});




