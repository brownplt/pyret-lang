var r = require("requirejs")
define(["js/runtime-anf", "./eval-matchers"], function(rtLib, e) {

  var _ = require('jasmine-node');
  var rt = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
  var P;

  function wf_check(s) { 
    return "where: blocks only allowed on named function declarations and data, not on " + s; 
  }
  function performTest() {

    beforeEach(function() {
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
        P.checkEvalsTo("fun f(): nothing where: 5 + 2 is 7 end\n42", rt.makeNumber(42));
        P.checkEvalsTo("fun f(): nothing where: 1 is 2 end\n10", rt.makeNumber(10));

        P.wait(done);
      });
      it("mixed operators should be malformed", function(done) {
        P.checkCompileErrorMsg("true and false or true", "Cannot mix binary operators of different types");
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
      it("multiple statements on a line", function(done) {
        var msg =  "Found two expressions on the same line";
        P.checkCompileErrorMsg("5-2", msg);
        P.checkCompileErrorMsg("'ab''de'", msg);
        P.checkCompileErrorMsg("a\"abc\"", msg);
        P.checkCompileErrorMsg("a=3b=4", msg)
        P.checkCompileErrorMsg("fun f(x): f x end", msg);
        P.checkCompileErrorMsg("fun f(x): f (x) end",msg);
        P.checkEvalsTo("fun f(x): f\n (x) end\n10", rt.makeNumber(10));
        P.checkEvalsTo("fun f(x):\n  f\n  # a comment\n  (x)\nend\n10", rt.makeNumber(10));
        P.wait(done);
      });
      it("anonymous bindings", function(done) {
        P.checkCompileErrorMsg("var _ = 5", "anonymous mutable variable");
        P.checkCompileErrorMsg("shadow _ = 5", "can't actually shadow");
        //P.checkCompileErrorMsg("graph: _ = BOS\nBOS = 5\nend", "graph expressions");
        P.checkCompileErrorMsg("{a : 5, a(self): 'bad' end}", "a is declared twice");
        P.wait(done);
      });
      it("malformed check-tests", function(done) {
        // toplevel
        P.checkCompileErrorMsg("5 is 5", "Cannot use `is` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("5 is-not 5", "Cannot use `is-not` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("5 is== 5", "Cannot use `is==` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("5 is=~ 5", "Cannot use `is=~` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("5 is<=> 5", "Cannot use `is<=>` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("5 satisfies 5", "Cannot use `satisfies` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("5 violates 5", "Cannot use `violates` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("5 raises 5", "Cannot use `raises` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("5 does-not-raise", "Cannot use `does-not-raise` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("5 raises-other-than 5", "Cannot use `raises-other-than` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("5 raises-satisfies 5", "Cannot use `raises-satisfies` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("5 raises-violates 5", "Cannot use `raises-violates` outside of a `check` or `where` block");
        // nested but still not in check-blocks
        P.checkCompileErrorMsg("lam(): 5 is 5 end", "Cannot use `is` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("lam(): 5 is-not 5 end", "Cannot use `is-not` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("lam(): 5 is== 5 end", "Cannot use `is==` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("lam(): 5 is=~ 5 end", "Cannot use `is=~` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("lam(): 5 is<=> 5 end", "Cannot use `is<=>` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("lam(): 5 satisfies 5 end", "Cannot use `satisfies` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("lam(): 5 violates 5 end", "Cannot use `violates` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("lam(): 5 raises 5 end", "Cannot use `raises` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("lam(): 5 does-not-raise end", "Cannot use `does-not-raise` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("lam(): 5 raises-other-than 5 end", "Cannot use `raises-other-than` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("lam(): 5 raises-satisfies 5 end", "Cannot use `raises-satisfies` outside of a `check` or `where` block");
        P.checkCompileErrorMsg("lam(): 5 raises-violates 5 end", "Cannot use `raises-violates` outside of a `check` or `where` block");
        // bad refinements
        P.checkCompileErrorMsg("check: 5 satisfies%(5) 5 end", "Cannot use refinement syntax `%(...)` with `satisfies`");
        P.checkCompileErrorMsg("check: 5 violates%(5) 5 end", "Cannot use refinement syntax `%(...)` with `violates`");
        P.checkCompileErrorMsg("check: 5 is==%(5) 5 end", "Cannot use refinement syntax `%(...)` with `is==`");
        P.checkCompileErrorMsg("check: 5 is=~%(5) 5 end", "Cannot use refinement syntax `%(...)` with `is=~`");
        P.checkCompileErrorMsg("check: 5 is<=>%(5) 5 end", "Cannot use refinement syntax `%(...)` with `is<=>`");
        P.checkCompileErrorMsg("check: 5 raises%(5) 5 end", "Cannot use refinement syntax `%(...)` with `raises`");
        P.checkCompileErrorMsg("check: 5 raises-satisfies%(5) 5 end", "Cannot use refinement syntax `%(...)` with `raises-satisfies`");
        P.checkCompileErrorMsg("check: 5 raises-violates%(5) 5 end", "Cannot use refinement syntax `%(...)` with `raises-violates`");
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
        P.checkCompileErrorMsg("lam(): x = 5 end", "Cannot end a block in a let-binding");
        P.checkCompileErrorMsg("lam(): var x = 5 end", "Cannot end a block in a var-binding");
        P.checkCompileErrorMsg("lam(): fun f(): nothing end end", "Cannot end a block in a fun-binding");
        P.checkCompileErrorMsg("lam(): x = 5\n fun f(): nothing end end", "Cannot end a block in a fun-binding");
        P.checkCompileErrorMsg("lam(): var x = 5\n y = 4\n fun f(): nothing end end", "Cannot end a block in a fun-binding");


        P.checkCompileErrorMsg("lam():\n" + 
                               "  data D:\n" + 
                               "    | var1()\n" + 
                               "  end\n" + 
                               "  42\n" +
                               "end",
                               "top level");
        P.checkCompileErrorMsg("lam():\n" + 
                               "  y = 10\n" + 
                               "  x = 5\n" + 
                               "  fun f(): nothing end\n" + 
                               "  data D:\n" + 
                               "    | var1()\n" + 
                               "  end\n" + 
                               "  42\n" +
                               "end",
                               "top level");
        P.checkCompileErrorMsg("block:\n" + 
                               "  x = 5\n" + 
                               "  y = 10\n" + 
                               "end",
                               "Cannot end a block in a let-binding");
        P.checkCompileErrorMsg("if x < y:\n" + 
                               "  print('x less than y')\n" + 
                               "end",
                               "Cannot have an `if` with a single branch");

        P.checkCompileErrorMsg("lam(): true where: 5 end", wf_check("anonymous functions"));
        P.checkCompileErrorMsg("method(self): nothing where: 5 end", wf_check("methods"));
        P.checkCompileErrorMsg("{m(self): nothing where: 5 end}", wf_check("methods"));

        P.wait(done)
      });
      it("should notice empty blocks", function(done) {
        P.checkCompileError("lam(): end", function(e) {
          expect(e.length).toEqual(1);
          return true;
        });
        P.checkCompileError("for each(elt from [list: ]): end", function(e) {
          expect(e.length).toEqual(1);
          return true;
        });
        P.checkCompileError("letrec x = 10: end", function(e) {
          expect(e.length).toEqual(1);
          return true;
        });
        P.checkCompileError("let x = 10: end", function(e) {
          expect(e.length).toEqual(1);
          return true;
        });
        P.checkCompileError("when true: end", function(e) {
          expect(e.length).toEqual(1);
          return true;
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
        P.checkCompileErrorMsg("cases(List) [list: ]:\n" +
                               "  | empty => 1\n" +
                               "  | empty => 2\n" +
                               "end",
                               "Duplicate case for empty");

        P.checkCompileErrorMsg("cases(List) [list: ]:\n" +
                               "  | empty => 1\n" +
                               "  | link(f, r) => 2\n" +
                               "  | empty => 2\n" +
                               "end",
                               "Duplicate case for empty");

        P.checkCompileErrorMsg("cases(List) [list: ]:\n" +
                               "  | empty => 1\n" +
                               "  | empty => 2\n" +
                               "  | else => 3\n" +
                               "end",
                               "Duplicate case for empty");

        P.checkCompileErrorMsg("cases(List) [list: ]:\n" +
                               "  | link(f, r) => 2\n" +
                               "  | bogus => 'bogus'\n" +
                               "  | bogus2 => 'bogus'\n" +
                               "  | empty => 1\n" +
                               "  | bogus3 => 'bogus'\n" +
                               "  | empty => 2\n" +
                               "  | else => 3\n" +
                               "end",
                               "Duplicate case for empty");

        P.checkCompileErrorMsg("cases(List) [list: ]:\n" +
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
      it("reserved words", function(done) {
        var reservedNames = [
          "function",
          "break",
          "return",
          "do",
          "yield",
          "throw",
          "continue",
          "while",
          "class",
          "interface",
          "generator",
          "alias",
          "extends",
          "implements",
          "module",
          "package",
          "namespace",
          "use",
          "public",
          "private",
          "protected",
          "static",
          "const",
          "enum",
          "super",
          "export",
          "new",
          "try",
          "finally",
          "debug",
          "spy",
          "switch",
          "this",
          "match",
          "case",
          "with"
        ];
        for(var i = 0; i < reservedNames.length; i++) {
          var err = "cannot use " + reservedNames[i] + " as an identifier";
          P.checkCompileErrorMsg(reservedNames[i], err);
          P.checkCompileErrorMsg(reservedNames[i] + " = 5", err);
          P.checkCompileErrorMsg("fun f(" + reservedNames[i] + "): 5 end", err);
          P.checkCompileErrorMsg("fun " + reservedNames[i] + "(): 5 end", err);
          if (reservedNames[i] !== "type") {
            P.checkCompileErrorMsg("{ " + reservedNames[i] + " : 42 }", err);
            P.checkCompileErrorMsg("{ " + reservedNames[i] + "(self): 42 end }", err);
          }
        }

        P.wait(done);
      });
      it("fraction literals", function(done) {
        var err = "fraction literal with zero denominator"
        P.checkCompileErrorMsg("1/0", err);
        P.checkCompileErrorMsg("100/0", err);
        P.checkCompileErrorMsg("0/0", err);
        P.checkCompileErrorMsg("0/00000", err);
        P.wait(done);
      });
      it("special imports", function(done) {
        var err = "Unsupported import type";
        P.checkCompileErrorMsg("import mydrive('foo') as D", err);
        P.checkNoCompileError("import my-gdrive('foo') as F");
        P.checkCompileErrorMsg("import my-gdrive('a', 'b') as D", "one argument");
        P.checkCompileErrorMsg("import shared-gdrive('a') as D", "two arguments");
        P.wait(done);
      });
      it("underscores", function(done) {
        P.checkCompileErrorMsg("cases(List) _: | empty => 5 end", "Underscore used as");
        P.checkCompileErrorMsg("cases(List) _: | empty => 5 | else => 6 end", "Underscore used as");
        P.checkCompileErrorMsg("cases(List) empty: | empty => _ end", "Underscore used as");
        P.checkCompileErrorMsg("cases(List) empty: | _ => 5 end", "Found a cases branch using _");
        P.checkCompileErrorMsg("block:\n _ \n 5 \n end", "Underscore used as");
        P.checkCompileErrorMsg("{ foo(self): _ end }", "Underscore used as");
        P.checkCompileErrorMsg("{ fieldname: _ }", "Underscore used as");
        P.checkCompileErrorMsg("method(self): _ end", "Underscore used as");
        P.checkCompileErrorMsg("lam(self): _ end", "Underscore used as");
        P.checkCompileErrorMsg("fun foo(self): _ end", "Underscore used as");
        P.checkCompileErrorMsg("check: _ end", "Underscore used as");
        P.checkCompileErrorMsg("provide _ end", "Underscore used as");
        P.wait(done);
      });
    });
  }
  return { performTest: performTest };
});




