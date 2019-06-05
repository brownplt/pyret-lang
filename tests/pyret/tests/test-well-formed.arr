import file("../../../src/arr/compiler/compile-structs.arr") as CS
import render-error-display as RED
import file("../test-compile-helper.arr") as C

run-str = C.run-str
compile-error = C.compile-error
output = C.output

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
  RED.display-to-string(err.render-reason(), torepr, empty)
end
  
fun cok(str):
  C.get-compile-errs(str)
end

check "mixed ops":
  run-str("true and false or true") is%(output) compile-error(CS.is-mixed-binops)
  run-str("1 + 2 - 3") is%(output) compile-error(CS.is-mixed-binops)
  run-str("1 + 2 + 3 * 4") is%(output) compile-error(CS.is-mixed-binops)
  run-str("1 / 2 + 3 * 4 - 5") is%(output) compile-error(CS.is-mixed-binops)
end

check "nullary methods":
  run-str("method(): nothing end") is%(output) compile-error(CS.is-no-arguments)
  run-str("{method foo(): nothing end}") is%(output) compile-error(CS.is-no-arguments)
end

check "multiple statements on a line":
  msg =  "on the same line"
  run-str("(5) (-2)") is%(output) compile-error(CS.is-same-line)
  run-str("'ab''de'") is%(output) compile-error(CS.is-same-line)
  run-str("a\"abc\"") is%(output) compile-error(CS.is-same-line)
  run-str("a=3b=4") is%(output) compile-error(CS.is-same-line)
  run-str("fun f(x) block: f x end") is%(output) compile-error(CS.is-same-line)
  run-str("fun f(x) block: f (x) end") is%(output) compile-error(CS.is-same-line)
  cok("fun f(x) block: num-sqr(f)\n (x) end\n10") is empty
  cok("fun f(x) block:\n  num-sqr(f)\n  # a comment\n  (x)\nend\n10") is empty
end

check "pointless underscores":
  run-str("var _ = 5") is%(output) compile-error(CS.is-pointless-var)
  run-str("shadow _ = 5") is%(output) compile-error(CS.is-pointless-shadow)
  run-str("rec _ = 5") is%(output) compile-error(CS.is-pointless-rec)
end

check "bad-checks":
  run-str("5 is 5") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("5 is-not 5") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("5 is== 5") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("5 is=~ 5") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("5 is<=> 5") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("5 satisfies 5") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("5 violates 5") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("5 raises 5") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("5 does-not-raise") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("5 raises-other-than 5") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("5 raises-satisfies 5") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("5 raises-violates 5") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("lam(): 5 is 5 end") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("lam(): 5 is-not 5 end") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("lam(): 5 is== 5 end") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("lam(): 5 is=~ 5 end") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("lam(): 5 is<=> 5 end") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("lam(): 5 satisfies 5 end") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("lam(): 5 violates 5 end") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("lam(): 5 raises 5 end") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("lam(): 5 does-not-raise end") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("lam(): 5 raises-other-than 5 end") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("lam(): 5 raises-satisfies 5 end") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("lam(): 5 raises-violates 5 end") is%(output) compile-error(CS.is-unwelcome-test)
  run-str("check: 5 satisfies%(5) 5 end") is%(output) compile-error(CS.is-unwelcome-test-refinement)
  run-str("check: 5 violates%(5) 5 end") is%(output) compile-error(CS.is-unwelcome-test-refinement)
  run-str("check: 5 is==%(5) 5 end") is%(output) compile-error(CS.is-unwelcome-test-refinement)
  run-str("check: 5 is=~%(5) 5 end") is%(output) compile-error(CS.is-unwelcome-test-refinement)
  run-str("check: 5 is<=>%(5) 5 end") is%(output) compile-error(CS.is-unwelcome-test-refinement)
  run-str("check: 5 raises%(5) 5 end") is%(output) compile-error(CS.is-unwelcome-test-refinement)
  run-str("check: 5 raises-satisfies%(5) 5 end") is%(output) compile-error(CS.is-unwelcome-test-refinement)
  run-str("check: 5 raises-violates%(5) 5 end") is%(output) compile-error(CS.is-unwelcome-test-refinement)
end

check "bad objects":
  run-str("{__proto__: 42}") is%(output) compile-error(CS.is-reserved-name)
end

check "malformed blocks":
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

  run-str("lam(): x = 5 end") is%(output) compile-error(CS.is-block-ending)
  run-str("lam(): var x = 5 end") is%(output) compile-error(CS.is-block-ending)
  run-str("lam(): fun f(): nothing end end") is%(output) compile-error(CS.is-block-ending)
  run-str("lam(): x = 5\n fun f(): nothing end end") is%(output) compile-error(CS.is-block-ending)
  run-str("lam(): var x = 5\n y = 4\n fun f(): nothing end end") is%(output) compile-error(CS.is-block-ending)


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

  run-str("lam(): true where: 5 end") is%(output) compile-error(CS.is-unwelcome-where)
  run-str("method(self): nothing where: 5 end") is%(output) compile-error(CS.is-unwelcome-where)
  run-str("{method m(self): nothing where: 5 end}") is%(output) compile-error(CS.is-unwelcome-where)
end

check "table row sizes, non-top-level":
  run-str("f(table: a, b row: end)") is%(output) compile-error(CS.is-table-empty-row)
  run-str("f(table: a, b row: 1 end)") is%(output) compile-error(CS.is-table-row-wrong-size)
  run-str("f(table: row: end)") is%(output) compile-error(CS.is-table-empty-header)
end

check "table loading checks":
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

check "table headers":
  run-str("table: a, b, a row: 1, 2, 3 end") is%(output) compile-error(CS.is-table-duplicate-column-name)
  run-str("load-table: a, b, a source: src end") is%(output) compile-error(CS.is-table-duplicate-column-name)
  run-str("load-table: timestamp, name, class source: src end") is%(output) compile-error(CS.is-reserved-name)
end

check "tuple bindings":
  cwfs("data D: d({x;y}) end") satisfies sc("Tuple binding not allowed")
  cwfs("let var {x;y} = 10: x end") satisfies sc("Variable bindings must be names")
  cwfs("var {x;y} = 10") satisfies sc("Variable bindings must be names")
  cwfs("rec {x;y} = 10") satisfies sc("Recursive bindings must be names")
  cwfs("letrec {x;y} = 10: x end") satisfies sc("Recursive bindings must be names")

  # Now nested in other scopes, since toplevel bindings are special
  cwfs("lam(): var {x;y} = 10\nx end") satisfies sc("Variable bindings must be names")
  cwfs("lam(): rec {x;y} = 10\nx end") satisfies sc("Recursive bindings must be names")
end

check "reactors":
  cwfs("reactor: todraw: 67 end") satisfies sc("must have a field named `init`")
  cwfs("reactor: init: 5, todraw: 67 end") satisfies sc("but found one named `todraw`")
  cwfs("reactor: method f(self): 5 end end") satisfies sc("cannot contain method fields")
  cwfs("reactor: init: 5, init: 10 end") satisfies sc("Duplicate")
end

check "malformed reactors":
  c("reactor:\n" +
    "  init: nothing,\n" +
    "  on-tick: lam(_):\n" +
    "      1 > true * 2\n" +
    "      1 + _\n" +
    "    end,\n" +
    "end") satisfies CS.is-block-needed
  c("reactor:\n" +
    "  init: nothing,\n" +
    "  on-tick: lam(_):\n" +
    "      1 > true * 2\n" +
    "    end,\n" +
    "end") satisfies CS.is-mixed-binops
end  

check "empty data definitions":
  cok("data NoVariants: end") is empty
  cok("data NoVariants:\nend") is empty
end

check "duplicated names in data defintiions":
  run-str("data Foo: is-Foo end") is%(output) compile-error(CS.is-duplicate-is-data)
  run-str("data Foo: Foo end") is%(output) compile-error(CS.is-data-variant-duplicate-name)
  run-str("data is-Foo3: Foo3 | baz end") is%(output) compile-error(CS.is-duplicate-is-data-variant)
  run-str("data Foo: bar | is-bar end") is%(output) compile-error(CS.is-duplicate-is-variant)
  run-str("data Foo: is-bar | bar end") is%(output) compile-error(CS.is-duplicate-is-variant)
  run-str("data Foo: bar | bar end") is%(output) compile-error(CS.is-duplicate-variant)
end

check "underscores":
  run-str("cases(List) _: | empty => 5 end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("cases(List) _: | empty => 5 | else => 6 end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("cases(List) empty: | empty => _ end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("cases(List) empty: | _ => 5 end") is%(output) compile-error(CS.is-underscore-as-pattern)
  run-str("block:\n _ \n 5 \n end") is%(output) compile-error(CS.is-wf-err)
  run-str("{ method foo(self): _ end }") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("{ fieldname: _ }") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("method(self): _ end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("lam(self): _ end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("fun foo(self): _ end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("provide _ end") is%(output) compile-error(CS.is-non-object-provide)

  run-str("table: _, a row: 1, 2 end") is%(output) compile-error(CS.is-underscore-as)

  run-str("{a: 1}.{_: 2}") is%(output) compile-error(CS.is-underscore-as)
  run-str("{a: 1}._") is%(output) compile-error(CS.is-underscore-as)

  run-str("2%<_>") is%(output) compile-error(CS.is-underscore-as-unit)
  run-str("n :: Number%<_ * m> = 1") is%(output) compile-error(CS.is-underscore-as-unit)
  run-str("n :: Number%<_ / m> = 1") is%(output) compile-error(CS.is-underscore-as-unit)
  run-str("n :: Number%<(_)> = 1") is%(output) compile-error(CS.is-underscore-as-unit)
  run-str("n :: Number%<_ ^ 2> = 1") is%(output) compile-error(CS.is-underscore-as-unit)
end

check "unit annotations":
  run-str("2%<m * n / o>") is%(output) compile-error(CS.is-mixed-binops)
  run-str("2%<m ^ 1 / o>") is%(output) compile-error(CS.is-mixed-binops)
  run-str("2%<m / n * o>") is%(output) compile-error(CS.is-mixed-binops)
  run-str("1/2%<m * n / o>") is%(output) compile-error(CS.is-mixed-binops)
  run-str("1/2%<m ^ 1 / o>") is%(output) compile-error(CS.is-mixed-binops)
  run-str("1/2%<m / n * o>") is%(output) compile-error(CS.is-mixed-binops)
  run-str("~1/2%<m * n / o>") is%(output) compile-error(CS.is-mixed-binops)
  run-str("~1/2%<m ^ 1 / o>") is%(output) compile-error(CS.is-mixed-binops)
  run-str("~1/2%<m / n * o>") is%(output) compile-error(CS.is-mixed-binops)
  run-str("var n :: Number%<m * n / o> = 0") is%(output) compile-error(CS.is-mixed-binops)
  run-str("var n :: Number%<m ^ 1 / o> = 0") is%(output) compile-error(CS.is-mixed-binops)
  run-str("var n :: Number%<m / n * o> = 0") is%(output) compile-error(CS.is-mixed-binops)

  run-str("2%<m ^ 0>") is%(output) compile-error(CS.is-invalid-unit-power)
  run-str("2%<m ^ 2.1>") is%(output) compile-error(CS.is-invalid-unit-power)
  run-str("1/2%<m ^ 0>") is%(output) compile-error(CS.is-invalid-unit-power)
  run-str("1/2%<m ^ 2.1>") is%(output) compile-error(CS.is-invalid-unit-power)
  run-str("~1/2%<m ^ 0>") is%(output) compile-error(CS.is-invalid-unit-power)
  run-str("~1/2%<m ^ 2.1>") is%(output) compile-error(CS.is-invalid-unit-power)
  run-str("var n :: Number%<m ^ 0> = 0") is%(output) compile-error(CS.is-invalid-unit-power)
  run-str("var n :: Number%<m ^ 2.1> = 0") is%(output) compile-error(CS.is-invalid-unit-power)
end

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
        P.checkCompileErrorMsg("cases(List) _: | empty => 5 end") satisfies CS.as-underscore
        P.checkCompileErrorMsg("cases(List) _: | empty => 5 | else => 6 end") satisfies CS.as-underscore
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
