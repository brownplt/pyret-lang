import file("../../../src/arr/compiler/compile-structs.arr") as CS
import file("../test-compile-helper.arr") as C

sc = lam(test-str): string-contains(_, test-str) end
fun c(str) block:
  errs = C.get-compile-errs(str)
  when is-empty(errs):
    print-error("Expected at least one error for running \n\n " + str + "\n\n" + " but got none ")
  end
  errs.first
end
fun cwfs(str):
  err = c(str)
  err.msg
end
  
fun cok(str):
  C.get-compile-errs(str)
end

check "mixed ops":
  c("true and false or true") satisfies CS.is-mixed-binops
  c("1 + 2 - 3") satisfies CS.is-mixed-binops
  c("1 + 2 + 3 * 4") satisfies CS.is-mixed-binops
  c("1 / 2 + 3 * 4 - 5") satisfies CS.is-mixed-binops
end

check "nullary methods":
  c("method(): nothing end") satisfies CS.is-no-arguments
  c("{method foo(): nothing end}") satisfies CS.is-no-arguments
end

check "multiple statements on a line":
  msg =  "on the same line"
  c("(5) (-2)") satisfies CS.is-same-line
  c("'ab''de'") satisfies CS.is-same-line
  c("a\"abc\"") satisfies CS.is-same-line
  c("a=3b=4") satisfies CS.is-same-line
  c("fun f(x) block: f x end") satisfies CS.is-same-line
  c("fun f(x) block: f (x) end") satisfies CS.is-same-line
  cok("fun f(x) block: f\n (x) end\n10") is empty
  cok("fun f(x) block:\n  f\n  # a comment\n  (x)\nend\n10") is empty
end

check "pointless underscores":
  c("var _ = 5") satisfies CS.is-pointless-var
  c("shadow _ = 5") satisfies CS.is-pointless-shadow
  c("rec _ = 5") satisfies CS.is-pointless-rec
end

check "bad-checks":
  print("Checking bad checks")
  c("5 is 5") satisfies CS.is-unwelcome-test
  c("5 is-not 5") satisfies CS.is-unwelcome-test
  c("5 is== 5") satisfies CS.is-unwelcome-test
  c("5 is=~ 5") satisfies CS.is-unwelcome-test
  c("5 is<=> 5") satisfies CS.is-unwelcome-test
  c("5 satisfies 5") satisfies CS.is-unwelcome-test
  c("5 violates 5") satisfies CS.is-unwelcome-test
  c("5 raises 5") satisfies CS.is-unwelcome-test
  c("5 does-not-raise") satisfies CS.is-unwelcome-test
  c("5 raises-other-than 5") satisfies CS.is-unwelcome-test
  c("5 raises-satisfies 5") satisfies CS.is-unwelcome-test
  c("5 raises-violates 5") satisfies CS.is-unwelcome-test
  c("lam(): 5 is 5 end") satisfies CS.is-unwelcome-test
  c("lam(): 5 is-not 5 end") satisfies CS.is-unwelcome-test
  c("lam(): 5 is== 5 end") satisfies CS.is-unwelcome-test
  c("lam(): 5 is=~ 5 end") satisfies CS.is-unwelcome-test
  c("lam(): 5 is<=> 5 end") satisfies CS.is-unwelcome-test
  c("lam(): 5 satisfies 5 end") satisfies CS.is-unwelcome-test
  c("lam(): 5 violates 5 end") satisfies CS.is-unwelcome-test
  c("lam(): 5 raises 5 end") satisfies CS.is-unwelcome-test
  c("lam(): 5 does-not-raise end") satisfies CS.is-unwelcome-test
  c("lam(): 5 raises-other-than 5 end") satisfies CS.is-unwelcome-test
  c("lam(): 5 raises-satisfies 5 end") satisfies CS.is-unwelcome-test
  c("lam(): 5 raises-violates 5 end") satisfies CS.is-unwelcome-test
  c("check: 5 satisfies%(5) 5 end") satisfies CS.is-unwelcome-test-refinement
  c("check: 5 violates%(5) 5 end") satisfies CS.is-unwelcome-test-refinement
  c("check: 5 is==%(5) 5 end") satisfies CS.is-unwelcome-test-refinement
  c("check: 5 is=~%(5) 5 end") satisfies CS.is-unwelcome-test-refinement
  c("check: 5 is<=>%(5) 5 end") satisfies CS.is-unwelcome-test-refinement
  c("check: 5 raises%(5) 5 end") satisfies CS.is-unwelcome-test-refinement
  c("check: 5 raises-satisfies%(5) 5 end") satisfies CS.is-unwelcome-test-refinement
  c("check: 5 raises-violates%(5) 5 end") satisfies CS.is-unwelcome-test-refinement
end

check "malformed blocks":
  print("Checking malformed blocks")
  c("fun foo():\n" + 
       " x = 10\n" + 
       "end\n" + 
       "10")
    satisfies CS.is-block-ending

  c("fun foo():\n" + 
       " var x = 10\n" + 
       "end\n" + 
       "10")
    satisfies CS.is-block-ending

  c("fun foo():\n" + 
       " fun f(): nothing end\n" + 
       "end\n" + 
       "10")
    satisfies CS.is-block-ending

  c("fun foo():\n" +
       " 123\n" +
       " a :: Number\n" +
       "end\n" +
       "10")
    satisfies CS.is-block-needed

  c("lam(): x = 5 end") satisfies CS.is-block-ending
  c("lam(): var x = 5 end") satisfies CS.is-block-ending
  c("lam(): fun f(): nothing end end") satisfies CS.is-block-ending
  c("lam(): x = 5\n fun f(): nothing end end") satisfies CS.is-block-ending
  c("lam(): var x = 5\n y = 4\n fun f(): nothing end end") satisfies CS.is-block-ending


  c("lam():\n" + 
       "  data D:\n" + 
       "    | var1()\n" + 
       "  end\n" + 
       "  42\n" +
       "end")
    satisfies CS.is-block-needed
  c("lam():\n" + 
       "  y = 10\n" + 
       "  x = 5\n" + 
       "  fun f(): nothing end\n" + 
       "  data D:\n" + 
       "    | var1()\n" + 
       "  end\n" + 
       "  42\n" +
       "end")
    satisfies CS.is-block-needed
  c("block:\n" + 
       "  x = 5\n" + 
       "  y = 10\n" + 
       "end")
    satisfies CS.is-block-ending

  c("if x < y:\n" + 
       "  print('x less than y')\n" + 
       "end")
    satisfies CS.is-single-branch-if

  c("lam(): true where: 5 end") satisfies CS.is-unwelcome-where
  c("method(self): nothing where: 5 end") satisfies CS.is-unwelcome-where
  c("{method m(self): nothing where: 5 end}") satisfies CS.is-unwelcome-where
end

check "table loading checks":
  print("Checking table loading")
  c("load-table: h1 end") satisfies CS.is-load-table-no-body
  c("load-table: source: src end") satisfies CS.is-table-empty-header
  c("load-table: h1 "
      + "sanitize h1 using s1 "
      + "end") satisfies CS.is-load-table-bad-number-srcs
  c("load-table: h1 "
      + "source: src1 "
      + "sanitize h1 using s1 "
      + "source: src2 "
      + "end") satisfies CS.is-load-table-bad-number-srcs
  c("load-table: h1, h2 "
      + "source: src1 "
      + "sanitize h1 using s1 "
      + "sanitize h2 using s2 "
      + "sanitize h1 using s1 "
      + "end") satisfies CS.is-load-table-duplicate-sanitizer
  c("load-table: h1 "
      + "source: src1 "
      + "sanitize h1 using s1 "
      + "sanitize h2 using s2 "
      + "end") satisfies CS.is-table-sanitizer-bad-column
end

check "tuple bindings":
  print("Checking tuple bindings")
  cwfs("data D: d({x;y}) end") satisfies sc("Tuple binding not allowed")
  cwfs("{ {x;y}; z} = 5") satisfies sc("Tuple bindings cannot be nested")
  cwfs("let var {x;y} = 10: x end") satisfies sc("Variable bindings must be names")
  cwfs("var {x;y} = 10") satisfies sc("Variable bindings must be names")
  cwfs("rec {x;y} = 10") satisfies sc("Recursive bindings must be names")
  cwfs("letrec {x;y} = 10: x end") satisfies sc("Recursive bindings must be names")

  # Now nested in other scopes, since toplevel bindings are special
  cwfs("lam(): var {x;y} = 10\nx end") satisfies sc("Variable bindings must be names")
  cwfs("lam(): rec {x;y} = 10\nx end") satisfies sc("Recursive bindings must be names")
end

#|
check "reactors":
  cwfs("{reactor: }") satisfies sc("must have a field named init")
  cwfs("{reactor: init: 5, todraw: 67}") satisfies sc("but found one named todraw")
  cwfs("{reactor: method f(self): 5 end}") satisfies sc("cannot contain method fields")
end
|#

#|
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
          var err = "disallows the use of `" + reservedNames[i] + "` as an identifier";
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
      xit("special imports", function(done) {
        var err = "Unsupported import type";
        P.checkCompileErrorMsg("import mydrive('foo') as D", err);
        P.checkNoCompileError("import my-gdrive('foo') as F");
        P.checkCompileErrorMsg("import my-gdrive('a', 'b') as D", "one argument");
        P.checkCompileErrorMsg("import shared-gdrive('a') as D", "two arguments");
        P.wait(done);
      });
      it("examples restriction", function(done) {
        P.checkCompileErrorMsg("examples: f() end", "must contain only test");
        P.wait(done);
      });
      it("underscores", function(done) {
        P.checkCompileErrorMsg("cases(List) _: | empty => 5 end", "The underscore");
        P.checkCompileErrorMsg("cases(List) _: | empty => 5 | else => 6 end", "The underscore");
        P.checkCompileErrorMsg("cases(List) empty: | empty => _ end", "The underscore");
        P.checkCompileErrorMsg("cases(List) empty: | _ => 5 end", "Found a cases branch using _");
        P.checkCompileErrorMsg("block:\n _ \n 5 \n end", "The underscore");
        P.checkCompileErrorMsg("{ foo(self): _ end }", "The underscore");
        P.checkCompileErrorMsg("{ fieldname: _ }", "The underscore");
        P.checkCompileErrorMsg("method(self): _ end", "The underscore");
        P.checkCompileErrorMsg("lam(self): _ end", "The underscore");
        P.checkCompileErrorMsg("fun foo(self): _ end", "The underscore");
        P.checkCompileErrorMsg("check: _ end", "The underscore");
        P.checkCompileErrorMsg("provide _ end", "The underscore");
        P.checkCompileErrorMsg("x = {1; 2; 3}\n x.{-1}", "Index too small");
        P.wait(done);
      });
        it("tuples", function(done) {
        P.checkCompileErrorMsg("x = {1; 2; 3}\n x.{-1}", "Index too small");
        P.wait(done);
      });
    });
  }
  return { performTest: performTest };
});



|#
