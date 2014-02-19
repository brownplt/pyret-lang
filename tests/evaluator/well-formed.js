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
      it("should be well-formed", function(done) {
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
      it("mixed operators should be malformed", function(done) {
        P.checkCompileErrorMsg("true and not false", "Cannot have nested bare `not`");
        P.checkCompileErrorMsg("true and false or true", "Cannot mix binary operators of different types");
        P.checkCompileErrorMsg("true and false and not true and false", "Cannot have nested bare `not`");
        P.checkCompileErrorMsg("1 + 2 - 3", "Cannot mix binary operators of different types");
        P.checkCompileErrorMsg("1 + 2 + 3 * 4", "Cannot mix binary operators of different types");
        P.checkCompileErrorMsg("1 / 2 + 3 * 4 - 5", "Cannot mix binary operators of different types");

        P.wait(done);
      });
      it("nullary methods", function(done) {
        P.checkCompileErrorMsg("method(): nothing end", "Cannot have a method with zero arguments");
        P.checkCompileErrorMsg("{foo(): nothing end}", "Cannot have a method with zero arguments");

        P.wait(done);
      });
      it("malformed blocks", function(done) {
        P.checkCompileErrorMsg("fun foo():\n" + 
                               " x = 10\n" + 
                               "end\n" + 
                               "10", 
                               "Cannot end a block in a let-binding");
        P.checkCompileErrorMsg("fun foo():\n" + 
                               " var x = 10\n" + 
                               "end\n" + 
                               "10", 
                               "Cannot end a block in a var-binding");
        P.checkCompileErrorMsg("fun foo():\n" + 
                               " fun f(): nothing end\n" + 
                               "end\n" + 
                               "10",
                               "Cannot end a block in a fun-binding");
        P.checkCompileErrorMsg("fun: x = 5 end", "Cannot end a block in a let-binding");
        P.checkCompileErrorMsg("fun: var x = 5 end", "Cannot end a block in a var-binding");
        P.checkCompileErrorMsg("fun: fun f(): nothing end end", "Cannot end a block in a fun-binding");
        P.checkCompileErrorMsg("fun: x = 5 fun f(): nothing end end", "Cannot end a block in a fun-binding");
        P.checkCompileErrorMsg("fun: var x = 5 y = 4 fun f(): nothing end end", "Cannot end a block in a fun-binding");


        // NOTE(dbp 2013-08-09): The more "obvious" occurence of these two get
        // caught by typechecking / parsing.
        P.checkCompileErrorMsg("check = 1 check", "Cannot use `check` as an identifier");
        P.checkCompileErrorMsg("where = 1 fun(): where end", "Cannot use `where` as an identifier");
        P.checkCompileErrorMsg("fun: 1 is 2 end", "Cannot use `is` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("fun: 1 raises 2 end", "Cannot use a check-test form outside of a `check` or `where` block");

        P.checkCompileErrorMsg("fun:\n" + 
                               "  data D:\n" + 
                               "    | var1()\n" + 
                               "  end\n" + 
                               "end",
                               "Cannot end a block with a data definition");
        P.checkCompileErrorMsg("fun:\n" + 
                               "  y = 10\n" + 
                               "  x = 5\n" + 
                               "  fun f(): nothing end\n" + 
                               "  data D:\n" + 
                               "    | var1()\n" + 
                               "  end\n" + 
                               "end",
                               "Cannot end a block with a data definition");
        P.checkCompileErrorMsg("fun:\n" + 
                               "  y = 10\n" + 
                               "  x = 5\n" + 
                               "  fun f(): nothing end\n" + 
                               "  graph:\n" + 
                               "  z = 5\n" + 
                               "  end\n" + 
                               "end",
                               "Cannot end a block with a graph definition");
        P.checkCompileErrorMsg("block:\n" + 
                               "  x = 5\n" + 
                               "  y = 10\n" + 
                               "end",
                               "Cannot end a block in a let-binding");
        P.checkCompileErrorMsg("block:\n" + 
                               "  x = 5\n" + 
                               "  graph: y = 10 end\n" + 
                               "end",
                               "Cannot end a block with a graph definition");
        P.checkCompileErrorMsg("if x < y:\n" + 
                               "  print('x less than y')\n" + 
                               "end",
                               "Cannot have an `if` with a single branch");

        P.checkCompileErrorMsg("fun(): true where: 5 end", wf_check("anonymous functions"));
        P.checkCompileErrorMsg("method(self): nothing where: 5 end", wf_check("methods"));
        P.checkCompileErrorMsg("{m(self): nothing where: 5 end}", wf_check("methods"));

        P.wait(done)
      });
      xit("empty blocks", function(done) {
        P.checkCompileError("fun(): where: 5 end", function(e){ 
          expect(rt.unwrap(e)).toEqual("Empty block"); return true; 
        });
        P.wait(done);
      });
      xit("malformed datatypes", function(done){
        P.checkCompileErrorMsg("datatype Foo:\n" +
                               "  | foo() with constructor(self): self end\n" +
                               "  | foo with constructor(self): self end\n" +
                               "end",
                               "Constructor name foo appeared more than once.");

        P.checkCompileErrorMsg("datatype Foo:\n" +
                               "  | foo() with constructor(self): self end\n" +
                               "  | bar() with constructor(self): self end\n" +
                               "  | baz() with constructor(self): self end\n" +
                               "  | foo(a) with constructor(self): self end\n" +
                               "end",
                               "Constructor name foo appeared more than once.");

        P.checkCompileErrorMsg("datatype Foo:\n" +
                               "  | bang with constructor(self): self end\n" +
                               "  | bar() with constructor(self): self end\n" +
                               "  | bang() with constructor(self): self end\n" +
                               "  | foo() with constructor(self): self end\n" +
                               "  | foo(a) with constructor(self): self end\n" +
                               "end",
                               "Constructor name bang appeared more than once.");

        P.wait(done);
      });
      it("malformed cases", function(done) {
        P.checkCompileErrorMsg("cases(List) []:\n" +
                               "  | empty => 1\n" +
                               "  | empty => 2\n" +
                               "end",
                               "Duplicate case for empty");

        P.checkCompileErrorMsg("cases(List) []:\n" +
                               "  | empty => 1\n" +
                               "  | link(f, r) => 2\n" +
                               "  | empty => 2\n" +
                               "end",
                               "Duplicate case for empty");

        P.checkCompileErrorMsg("cases(List) []:\n" +
                               "  | empty => 1\n" +
                               "  | empty => 2\n" +
                               "  | else => 3\n" +
                               "end",
                               "Duplicate case for empty");

        P.checkCompileErrorMsg("cases(List) []:\n" +
                               "  | link(f, r) => 2\n" +
                               "  | bogus => 'bogus'\n" +
                               "  | bogus2 => 'bogus'\n" +
                               "  | empty => 1\n" +
                               "  | bogus3 => 'bogus'\n" +
                               "  | empty => 2\n" +
                               "  | else => 3\n" +
                               "end",
                               "Duplicate case for empty");

        P.checkCompileErrorMsg("cases(List) []:\n" +
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




