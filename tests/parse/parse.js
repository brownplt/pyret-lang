const R = require("requirejs");
var build = process.env["PHASE"] || "build/phaseA";
R(["pyret-base/js/pyret-tokenizer", "pyret-base/js/pyret-parser", "fs"], function(T, G, fs) {
  _ = require("jasmine-node");
  function parse(str) {
    const toks = T.Tokenizer;
    toks.tokenizeFrom(str);
    var parsed = G.PyretGrammar.parse(toks);
    if (!parsed) {
      return false;
    }
    var countParses = G.PyretGrammar.countAllParses(parsed);
    if (countParses === 1) {
      var ast = G.PyretGrammar.constructUniqueParse(parsed);
      return ast;
    }
    else {
      throw "Non-unique parse";
    }
  }
  var toks = [];
  var numToks = 0;
  function lex(str) {
    const tokenizer = T.Tokenizer;
    tokenizer.tokenizeFrom(str);
    numToks = 0;
    while (tokenizer.hasNext())
      toks[numToks++] = tokenizer.next();
    return toks;
  }
  function test(actual, expected, testname, toks) {
    if (actual === expected) {
      return true;
    } else {
      var allToks = "Str was " + JSON.stringify(testname) + "\n";
      for (var t = 0; t < toks.length; t++) {
        if (t > 0) allToks += "\n";
        allToks += "Tok[" + t + "] = " + toks[t].toString(true)
          + " at pos " + toks[t].pos.toString(true);
      }
      allToks += "Expected " + JSON.stringify(expected) + ", but got " + JSON.stringify(actual)
        + " in " + JSON.stringify(testname);
      expect(allToks).toBe("");
      return false;
    }
  }
  function testPos(tok, expected, str, toks) {
    if (tok.pos.endChar - tok.pos.startChar == expected.length) {
      return true;
    } else {
      test(str.slice(tok.pos.startChar, tok.pos.endChar), expected, str, toks);
      return false;
    }
  }
  describe("lexing", function() {
    const tok_words = [
      "an-ident",
      "import", "provide-types", "provide", "as", "newtype", "type-let", "type", "lazy",
      "var", "letrec", "let", "fun", "lam", "true", "false", "method", "doc:", "check:", "check",
      "try:", "except", "cases", "when", "ask:", "otherwise:", "if", "then:", "else:", "else if", "else",
      "data", "with:", "sharing:", "shadow", "mutable", "cyclic", "block:", "for", "from", "end",
      "4/5", "-123/456", "01823.1225426", "-1.2", "1", "-2", "and", "or", "is", "satisfies", "raises",
      "```a one line string```", "```a two\nline string```", "\"a string\"", "'a string'",
    ];
    const tok_opers = [
      ".", "!", "%", ",", "->", "=>", ":=", "::", ":", "|",
      " ^ ", " + ", " - ", " * ", " / ", " <= ", " >= ", " == ", " <> ", " < ", " > ", "<", ">",
      "[", "]", "{", "}", "(", ")", "=", ";", "\\"
    ];
    const compound_toks = {
      "else if": true, // "else" + " " + "if"
      "else:": true, // "else" + "" + ":"
      "else:=": true, // "else:" + "" + "="
      "else::": true, // "else:" + "" + ":"
      "check:": true, // "check" + "" + ":"
      "check:=": true, // "check:" + "" + ":"
      "check::": true, // "check:" + "" + "::"
      "::": true, // ":" + "" + ":"
      "1.4/5": true, // "1" + "." + "4/5"
      "-2.4/5": true, // "-2" + "." + "4/5"
      "1.01823.1125426": true, // "1" + "." + "01823.1125426"
      "1.1": true, // "1" + "." + "1"
      "-2.01823.1125426": true, // "-2" + "." + "01823.1125426"
      "-2.1": true, // "-2" + "." + "1"
    };
    const toks_needing_ws = {
      "::": true,
      "=>": true,
      "^": true,
      "+": true,
      "-": true,
      "*": true,
      "/": true,
      "<=": true,
      ">=": true,
      "<>": true,
      "<": true,
      ">": true
    };
    const trimmed_tok_words = tok_words.map(function(s) { return s.trim(); });
    const trimmed_tok_opers = tok_opers.map(function(s) { return s.trim(); });
    const all_toks = trimmed_tok_words.slice(0).push.apply(trimmed_tok_opers);
    const ws = [ "", "   ", "\n", "# comment\n\n", "   \n", "  \n\n   ", "  \n  # comment  \n   \n   ", "#\n" ];
    it("should have tight lexical extents for all tokens", function() {
      lex("");
      expect(numToks).toBe(1);
      expect(toks[0].name).toBe("EOF");
      for (var ws1 = 0; ws1 < ws.length; ws1++) {
        for (var i = 0; i < all_toks.length; i++) {
          for (var ws2 = 0; ws2 < ws.length; ws2++) {
            var str = "" + ws[ws1] + all_toks[i] + ws[ws2]
            lex(str);
            test(numToks, 2, str, toks) &&
              testPos(toks[0], all_toks[i], str, toks) &&
              test(toks[1].name, "EOF", str, toks);
          }
        }
      }
      console.log("Finished single token tests");
    });
    xit("should have tight lexical extents for all pairs of tokens", function() {
      for (var ws1 = 0; ws1 < ws.length; ws1++) {
        for (var i = 0; i < tok_words.length; i++) {
          for (var ws2 = 0; ws2 < ws.length; ws2++) {
            for (var j = 0; j < tok_opers.length; j++) {
              if (compound_toks[trimmed_tok_words[i] + trimmed_tok_opers[j]] === true) continue;
              for (var ws3 = 0; ws3 < ws.length; ws3++) {
                var str = "" + ws[ws1] + tok_words[i] + ws[ws2] + tok_opers[j] + ws[ws3];
                lex(str);
                test(numToks, 3, str, toks) &&
                  testPos(toks[0], trimmed_tok_words[i], str, toks) &&
                  testPos(toks[1], trimmed_tok_opers[j], str, toks) &&
                  test(toks[2].name, "EOF", str, toks);
              }
            }
            if (ws[ws2] === "") continue;
            for (var j = 0; j < tok_words.length; j++) {
              for (var ws3 = 0; ws3 < ws.length; ws3++) {
                var str = "" + ws[ws1] + tok_words[i] + ws[ws2] + tok_words[j] + ws[ws3];
                lex(str);
                test(numToks, 3, str, toks) &&
                  testPos(toks[0], trimmed_tok_words[i], str, toks) &&
                  testPos(toks[1], trimmed_tok_words[j], str, toks) &&
                  test(toks[2].name, "EOF", str, toks);
              }
            }
          }
        }
        for (var i = 0; i < tok_opers.length; i++) {
          for (var ws2 = 0; ws2 < ws.length; ws2++) {
            if (!(toks_needing_ws[tok_opers[i]] && (ws[ws2] === ""))) {
              for (var j = 0; j < tok_words.length; j++) {
                for (var ws3 = 0; ws3 < ws.length; ws3++) {
                  var str = "" + ws[ws1] + tok_opers[i] + ws[ws2] + tok_words[j] + ws[ws3];
                  lex(str);
                  test(numToks, 3, str, toks) &&
                    testPos(toks[0], trimmed_tok_opers[i], str, toks) &&
                    testPos(toks[1], trimmed_tok_words[j], str, toks) &&
                    test(toks[2].name, "EOF", str, toks);
                }
              }
            }
            if (ws[ws2] !== "") {
              for (var j = 0; j < tok_opers.length; j++) {
                for (var ws3 = 0; ws3 < ws.length; ws3++) {
                  var str = "" + ws[ws1] + tok_opers[i] + ws[ws2] + tok_opers[j] + ws[ws3];
                  lex(str);
                  test(numToks, 3, str, toks) &&
                    testPos(toks[0], trimmed_tok_opers[i], str, toks) &&
                    testPos(toks[1], trimmed_tok_opers[j], str, toks) &&
                    test(toks[2].name, "EOF", str, toks);
                }
              }
            }
          }
        }
      }
      console.log("Finished token-pair tests");
    });
    xit("should have tight lexical extents for all triples of tokens", function() {
      for (var ws1 = 0; ws1 < 4; ws1++) { // deliberately skipping a lot of whitespaces
        for (var i = 0; i < tok_words.length; i++) {
          console.log(ws1 + "." + i + " out of " + ws.length + "." + tok_words.length);
          for (var ws2 = 0; ws2 < 4; ws2++) { // deliberately skipping a lot of whitespaces
            for (var j = 0; j < tok_opers.length; j++) {
              if (compound_toks[trimmed_tok_words[i] + trimmed_tok_opers[j]] === true) continue;
              for (var ws3 = 0; ws3 < 4; ws3++) { // deliberately skipping a lot of whitespaces
                if (!(toks_needing_ws[tok_opers[j]] && (ws[ws3] === ""))) {
                  for (var k = 0; k < tok_words.length; k++) {
                    for (var ws4 = 0; ws4 < 4; ws4++) { // deliberately skipping a lot of whitespaces
                      var str = ws[ws1] + tok_words[i] + ws[ws2] + tok_opers[j] + ws[ws3] + tok_words[k] + ws[ws4];
                      lex(str);
                      test(numToks, 4, str, toks) &&
                        testPos(toks[0], trimmed_tok_words[i], str, toks) &&
                        testPos(toks[1], trimmed_tok_opers[j], str, toks) &&
                        testPos(toks[2], trimmed_tok_words[k], str, toks) &&
                        test(toks[3].name, "EOF", str, toks);
                    }
                  }
                }
              }
            }
          }
        }
      }
      console.log("Finished token-triple tests");
    });
  });
  describe("lexing", function() {
    it("should lex triple-quoted strings", function() {
      expect(parse("```asd`asd```")).not.toBe(false);
      expect(parse("```asd\`asd```")).not.toBe(false);
      expect(parse("```asd``asd```")).not.toBe(false);
      expect(parse("```asd``\\`asd```")).not.toBe(false);
      expect(parse("```asd``\\````")).not.toBe(false);
      expect(parse("```asd```asd```")).toBe(false);
    });
  });
  describe("lexing", function() {
    it("shouldn't lex bad numbers", function() {
      expect(parse("2")).not.toBe(false);
      expect(parse("2.")).toBe(false);
      expect(parse("2..")).toBe(false);
      expect(parse("2...")).toBe(false);
      expect(parse("2....")).toBe(false);
      expect(parse("2.....")).toBe(false);

      expect(parse("2e4")).not.toBe(false);
      expect(parse("2.e4")).toBe(false);
      expect(parse("2..e4")).toBe(false);
      expect(parse("2...e4")).toBe(false);
      expect(parse("2....e4")).toBe(false);
      expect(parse("2.....e4")).toBe(false);

      expect(parse(".2")).toBe(false);
      expect(parse(".2.")).toBe(false);
      expect(parse(".2..")).toBe(false);
      expect(parse(".2...")).toBe(false);
      expect(parse(".2....")).toBe(false);
      expect(parse(".2.....")).toBe(false);

      expect(parse(".2e4")).toBe(false);
      expect(parse(".2.e4")).toBe(false);
      expect(parse(".2..e4")).toBe(false);
      expect(parse(".2...e4")).toBe(false);
      expect(parse(".2....e4")).toBe(false);
      expect(parse(".2.....e4")).toBe(false);

      expect(parse("2.3")).not.toBe(false);
      expect(parse("2.3.")).toBe(false);
      expect(parse("2.3..")).toBe(false);
      expect(parse("2.3...")).toBe(false);
      expect(parse("2.3....")).toBe(false);
      expect(parse("2.3.....")).toBe(false);

      expect(parse("2.3e4")).not.toBe(false);
      expect(parse("2.3.e4")).toBe(false);
      expect(parse("2.3..e4")).toBe(false);
      expect(parse("2.3...e4")).toBe(false);
      expect(parse("2.3....e4")).toBe(false);
      expect(parse("2.3.....e4")).toBe(false);

      expect(parse("~2")).not.toBe(false);
      expect(parse("~2.")).toBe(false);
      expect(parse("~2..")).toBe(false);
      expect(parse("~2...")).not.toBe(false);
      expect(parse("~2....")).toBe(false);
      expect(parse("~2.....")).toBe(false);

      expect(parse("~2e4")).not.toBe(false);
      expect(parse("~2.e4")).toBe(false);
      expect(parse("~2..e4")).toBe(false);
      expect(parse("~2...e4")).not.toBe(false);
      expect(parse("~2e4...")).toBe(false);
      expect(parse("~2....e4")).toBe(false);
      expect(parse("~2.....e4")).toBe(false);

      expect(parse("~.2")).toBe(false);
      expect(parse("~.2.")).toBe(false);
      expect(parse("~.2..")).toBe(false);
      expect(parse("~.2...")).toBe(false);
      expect(parse("~.2....")).toBe(false);
      expect(parse("~.2.....")).toBe(false);

      expect(parse("~.2e4")).toBe(false);
      expect(parse("~.2.e4")).toBe(false);
      expect(parse("~.2..e4")).toBe(false);
      expect(parse("~.2...e4")).toBe(false);
      expect(parse("~.2....e4")).toBe(false);
      expect(parse("~.2.....e4")).toBe(false);

      expect(parse("~2.3")).not.toBe(false);
      expect(parse("~2.3.")).toBe(false);
      expect(parse("~2.3..")).toBe(false);
      expect(parse("~2.3...")).not.toBe(false);
      expect(parse("~2.3....")).toBe(false);
      expect(parse("~2.3.....")).toBe(false);

      expect(parse("~2.3e4")).not.toBe(false);
      expect(parse("~2.3.e4")).toBe(false);
      expect(parse("~2.3..e4")).toBe(false);
      expect(parse("~2.3...e4")).not.toBe(false);
      expect(parse("~2.3e4...")).toBe(false);
      expect(parse("~2.3....e4")).toBe(false);
      expect(parse("~2.3.....e4")).toBe(false);

    });
  });
  describe("parsing", function() {
    it("should parse lets and letrecs", function() {
      expect(parse("let: 10 end")).toBe(false);
      expect(parse("letrec: 10 end")).toBe(false);
      expect(parse("let x = 10, y = 12: x + y end")).not.toBe(false);
      expect(parse("let x = 10, y = 12, z = 13: BAMBOOZLE end")).not.toBe(false);
      expect(parse("letrec x = 10, y = 12: x + y end")).not.toBe(false);
      expect(parse("letrec z = 62, x = 10, y = 12: x + y end")).not.toBe(false);
    });

    it("should parse type-lets", function() {
      expect(parse("type-let t1 = Number, t2 = String: 5 end")).not.toBe(false);
      expect(parse("type-let t1 = Number: 10 end")).not.toBe(false);
      expect(parse("type-let: 10 end")).toBe(false);
      expect(parse("type-let newtype List as ListT: {} end")).not.toBe(false);
      expect(parse("type-let newtype List as ListT, thing = foo: {} end")).not.toBe(false)
    });

    it("should parse standalone types", function() {
      expect(parse("type foo = { x :: Number }")).not.toBe(false);
      expect(parse("type foo = Number -> String")).toBe(false);
    });

    it("should parse standalone newtypes", function() {
      expect(parse("newtype Foo as FooT")).not.toBe(false);
      expect(parse("newtype (Number -> String)")).toBe(false);
    });

    it("should parse provide-types", function() {
      expect(parse("provide-types { List :: List }")).not.toBe(false);
      expect(parse("provide-types { List :: List, }")).not.toBe(false);
      expect(parse("provide-types { List :: List, x :: (Number -> String) }")).not.toBe(false);
    });

    it("shouldn't parse expressions in provide-types", function() {
      expect(parse("provide-types { List :: 5 + 5 }")).toBe(false);
      expect(parse("provide-types { List :: List, x :: lam(x): x end }")).toBe(false);
    });

    it("shouldn't allow English ops as identifiers, no matter the whitespace", function() {
      // NOTE(joe): See John Ericson's comment about changing the tokenizer
      // at https://github.com/brownplt/pyret-lang/pull/220#issuecomment-48685416
      // if this turns into a regression

      const wss = [" ", " \n", "\n ", " \n", " \n "];
      const en_ops = ["or", "and", "is", "satisfies", "raises"];

      for (var i = 0; i < en_ops.length; ++i) {
        const op = en_ops[i];

        expect(parse(op + "=" + "false")).toBe(false);

        for (var j = 0; j < wss.length; ++j) {
          const ws = wss[j];

          expect(parse("(" + op + ws + op + ws + op + ")")).toBe(false);
          expect(parse(op + ws + "="      + "false")).toBe(false);
          expect(parse(op +      "=" + ws + "false")).toBe(false);
          expect(parse(op + ws + "=" + ws + "false")).toBe(false);
        }
      }
    });

    it("shouldn't allow hyphens at the beginning or end of identifiers", function() {
      // issue #222
      expect(parse("-")).toBe(false);
      expect(parse("(- -)")).toBe(false);
      expect(parse("--")).toBe(false);
      expect(parse("(-- --)")).toBe(false);
      expect(parse("a- b")).toBe(false);
      expect(parse("a -b")).toBe(false);
      expect(parse("a- = b")).toBe(false);
      expect(parse("-a = b")).toBe(false);

      expect(parse("a-a")).not.toBe(false);
      expect(parse("a-a-a")).not.toBe(false);
      expect(parse("a--aa")).not.toBe(false);
      expect(parse("aa--a")).not.toBe(false);
    });

    it("should allow English ops with all manner of surrounding whitespace and parens", function() {

      const wss = [" ", " \n", "\n ", " \n", " \n "];
      const en_ops = ["or", "and", "is", "satisfies", "raises"];

      for (var i = 0; i < en_ops.length; ++i) {
        const op = en_ops[i];

        expect(parse("(false)" + op            )).toBe(false);
        expect(parse(            op + "(false)")).toBe(false);

        expect(parse("(false)" + op + "(false)")).not.toBe(false);

        for (var j = 0; j < wss.length; ++j) {
          const ws = wss[j];

          expect(parse("(false)" + ws + op                 )).toBe(false);
          expect(parse(                 op + ws + "(false)")).toBe(false);

          expect(parse("(false)" + ws + op      + "(false)")).not.toBe(false);
          expect(parse("(false)" +      op + ws + "(false)")).not.toBe(false);
          expect(parse("(false)" + ws + op + ws + "(false)")).not.toBe(false);
        }
      }
    });

    it("should notice parse errors", function() {
      expect(parse("bad end")).toBe(false);
      expect(parse("provide-types { List :: List } end")).toBe(false);
    });

    it ("shouldn't parse empty comma-separated things with trailing comma", function() {
      expect(parse("{,}")).toBe(false);
      expect(parse('x :: {,} = {}')).toBe(false);
    });

    it("should parse all comma-separated things", function() {
      expect(parse('import foo from quuz')).not.toBe(false);
      expect(parse('import foo, bar, baz from quuz')).not.toBe(false);
      expect(parse('import foo("bar") as floo')).not.toBe(false);
      expect(parse('import foo("bar", "baz", "quuz") as floo')).not.toBe(false);
      expect(parse('let x = 5: true end')).not.toBe(false);
      expect(parse('let x = 5, y = 4,z=2: true end')).not.toBe(false);
      expect(parse('letrec x = 5: true end')).not.toBe(false);
      expect(parse('letrec x = 5, y = 4,z=2: true end')).not.toBe(false);
      expect(parse('type-let foo=Any: true end')).not.toBe(false);
      expect(parse('type-let Foo=Any, Bar = Foo<Number>: true end')).not.toBe(false);
      expect(parse('fun foo<A>(): 5 end')).not.toBe(false);
      expect(parse('fun foo<A, B, C>(): 5 end')).not.toBe(false);
      expect(parse('fun foo<A>(a): 5 end')).not.toBe(false);
      expect(parse('fun foo<A, B, C>(a, b, c): 5 end')).not.toBe(false);
      expect(parse('data Foo: var1() end')).not.toBe(false);
      expect(parse('data Foo: var1(a) end')).not.toBe(false);
      expect(parse('data Foo: var1(a, b, c) end')).not.toBe(false);
      expect(parse('foo()')).not.toBe(false);
      expect(parse('foo(1)')).not.toBe(false);
      expect(parse('foo(1, 2, 3)')).not.toBe(false);
      expect(parse('foo<>')).toBe(false);
      expect(parse('foo<A>')).not.toBe(false);
      expect(parse('foo<A, B, C>')).not.toBe(false);
      expect(parse('{}')).not.toBe(false);
      expect(parse('{foo: 5}')).not.toBe(false);
      expect(parse('{foo: 5, bar: 6, baz: 7}')).not.toBe(false);
      expect(parse('{foo: 5, bar: 6, baz: 7,}')).not.toBe(false);
      expect(parse('a.{}')).toBe(false);
      expect(parse('a.{foo: 5}')).not.toBe(false);
      expect(parse('a.{foo: 5, bar: 6, baz: 7}')).not.toBe(false);
      expect(parse('a.{foo: 5, bar: 6, baz: 7,}')).not.toBe(false);
      expect(parse('[foo: ]')).not.toBe(false);
      expect(parse('[foo: 5]')).not.toBe(false);
      expect(parse('[foo: 5, 6, 7]')).not.toBe(false);
      expect(parse('cases(Foo) bar: | foo() => true end')).not.toBe(false);
      expect(parse('cases(Foo) bar: | foo(a) => true end')).not.toBe(false);
      expect(parse('cases(Foo) bar: | foo(a, ref b, c) => true end')).not.toBe(false);
      expect(parse('for map(): 5 end')).not.toBe(false);
      expect(parse('for map(a from b): 5 end')).not.toBe(false);
      expect(parse('for map(a from b, c from d, e from f): 5 end')).not.toBe(false);
      expect(parse('x :: {} = 5')).not.toBe(false);
      expect(parse('x :: {foo:: A} = 5')).not.toBe(false);
      expect(parse('x :: {foo:: A,} = 5')).not.toBe(false);
      expect(parse('x :: {foo:: A, bar :: B, baz:: C} = 5')).not.toBe(false);
      expect(parse('x :: -> A')).not.toBe(false);
      expect(parse('x :: A -> A')).not.toBe(false);
      expect(parse('x :: A, A -> A')).not.toBe(false);
      expect(parse('x :: A<( -> A)>')).not.toBe(false);
      expect(parse('x :: A<( -> A), B>')).not.toBe(false);
    });

    it("should parse angle brackets without whitespace only as type instantiations", function() {
      expect(parse("map<A>")).not.toBe(false);
      expect(parse("(map<A>)")).not.toBe(false);
      expect(parse("(map<A, B>)")).not.toBe(false);
      expect(parse("map<A, B>(id)")).not.toBe(false);
      expect(parse("(map < A, B > (id))")).toBe(false);
      expect(parse("(map\n<\nA, B\n>\n(id))")).toBe(false);
      expect(parse("map<A,\nB>(id)")).not.toBe(false);
    });

    it("should parse angle brackets without whitespace in annotations only as type function application", function() {
      expect(parse("a :: List < A > = a")).toBe(false);
      expect(parse("a :: List < A, B > = a")).toBe(false);
      expect(parse("a :: List<A> = a")).not.toBe(false);
      expect(parse("a :: List<A, B> = a")).not.toBe(false);
    });

    it("should parse angle brackets with whitespace as gt/lt", function() {
      expect(parse("1\n<\n2 or false\n B > (id)")).not.toBe(false);
      expect(parse("1<\n2 or false, B > (id)")).toBe(false);
    });

    it("should not care about whitespace and angle brackets in declarations", function() {
      expect(parse("fun print<A>(): end")).not.toBe(false);
      expect(parse("fun print< A>(): end")).not.toBe(false);
      expect(parse("fun print <A>(): end")).not.toBe(false);
      expect(parse("fun print < A>(): end")).not.toBe(false);
      expect(parse("fun print<A >(): end")).not.toBe(false);
      expect(parse("fun print< A >(): end")).not.toBe(false);
      expect(parse("fun print <A >(): end")).not.toBe(false);
    });

    it("should not treat (...) after operators as application", function() {
      expect(parse("(true) or (false)")).not.toBe(false);
      expect(parse("(true) < (false)")).not.toBe(false);
      expect(parse("(true) > (false)")).not.toBe(false);
    });

    it("should not mind end at EOF", function() {
      expect(parse("lam<T>(x :: T) -> T: x end")).not.toBe(false);
    });

    it("should not mind end at EOL, and then another statement", function() {
      var a = "  fun x<T>(x :: T) -> T: x end";
      expect(parse("block:\n" + a + "\n" + a + " end")).not.toBe(false);
      expect(parse("block:\n" + a + " \n" + a + " end")).not.toBe(false);
    });

    it("should require whitespace after :: and =>", function() {
      expect(parse("cases (T) x: | Foo() =>(true) end")).toBe(false);
      expect(parse("cases (T) x: | Foo() => (true) end")).not.toBe(false);
      expect(parse("cases (T) x: | Foo() =>\n(true) end")).not.toBe(false);
      expect(parse("block: dog ::Cat = really-huh end")).toBe(false);
      expect(parse("block: dog :: Cat = really-huh end")).not.toBe(false);
      expect(parse("block: dog :: Cat =\nreally-huh end")).not.toBe(false);
    });

    it("should treat (...) as grouping after ,", function() {
      expect(parse("[list: x,(x)]")).not.toBe(false);
      expect(parse("[list: x , (x)]")).not.toBe(false);
      expect(parse("[list: x ,\n(x)]")).not.toBe(false);
    });
    it("should treat (...) as grouping after :", function() {
      expect(parse("{ asdf:(asdf) }")).not.toBe(false);
      expect(parse("{ asdf : (asdf) }")).not.toBe(false);
      expect(parse("{ asdf :\n(asdf) }")).not.toBe(false);
      expect(parse("fun f(x):\nx\nwhere:(f(5)) is 5\nend")).not.toBe(false);
      expect(parse("check:(5) is 5 end")).not.toBe(false);
      expect(parse("examples:(5) is 5 end")).not.toBe(false);
      expect(parse("ask:\n  | false then: 1\n  | otherwise:(true)\nend")).not.toBe(false);
      expect(parse("ask:\n  | true then:(1)\nend")).not.toBe(false);
      expect(parse("if true: 1 else:(1) end")).not.toBe(false);
      expect(parse("block:(5) end")).not.toBe(false);
      expect(parse("ask:\n  |(true) then: 1\nend")).not.toBe(false);
    });

    it("should treat (...) as grouping after =", function() {
      expect(parse("block: x=(x) end")).not.toBe(false);
      expect(parse("block: x = (x) end")).not.toBe(false);
      expect(parse("block: x =\n(x) end")).not.toBe(false);
    });

    it("should treat (...) as grouping after :=", function() {
      expect(parse("block: x:=(x) end")).not.toBe(false);
      expect(parse("block: x := (x) end")).not.toBe(false);
      expect(parse("block: x :=\n(x) end")).not.toBe(false);
    });

    it("should treat (...) as grouping after ;", function() {
      expect(parse("block: lam(x): x end(x)end")).not.toBe(false);
      expect(parse("block: lam(x): x end (x)end")).not.toBe(false);
      expect(parse("block: lam(x): x end\n(x)end")).not.toBe(false);
    });

    it("should treat (...) as grouping or as args within {...}", function() {
      expect(parse("{(): true}")).not.toBe(false);
      expect(parse("{(a): true}")).not.toBe(false);
      expect(parse("{(a, b): true}")).not.toBe(false);
      expect(parse("{ (): true}")).toBe(false);
      expect(parse("{ (a): true}")).toBe(false);
      expect(parse("{ (a, b): true}")).toBe(false);
      expect(parse("{(1 + 2); (3 + 4)}")).not.toBe(false);
      expect(parse("{ (1 + 2); (3 + 4) }")).not.toBe(false);
    });

    it("should parse get-bang", function() {
      expect(parse("o!x")).not.toBe(false);
      expect(parse("y.x!x")).not.toBe(false);
    });

    it("should parse update", function() {
      expect(parse("o!{x:5}")).not.toBe(false);
      expect(parse("y!{x:5, y:10}")).not.toBe(false);
    });

    it("should parse ref fields in data definitions", function() {
      expect(parse("data D: d(ref x) end")).not.toBe(false);
      expect(parse("data D: d(ref x :: Number % (is-odd)) end")).not.toBe(false);
      expect(parse("data D: d(ref x, ref y :: Number) end")).not.toBe(false);
      expect(parse("data D: | d(ref x :: Boolean, ref y) end")).not.toBe(false);
    });

    it("should parse ref fields in object literals", function() {
      expect(parse("{ref x :: Number: 22}")).not.toBe(false);
      expect(parse("{ref x: 22}")).not.toBe(false);
      expect(parse("{ref x: 22, y: \"a\"}")).not.toBe(false);
      expect(parse("{ref x: 22, ref y: \"a\"}")).not.toBe(false);
      expect(parse("{ref x: 22, ref y :: String: \"a\"}")).not.toBe(false);
      expect(parse("{ref x :: { z :: Number}: 22, ref y :: String: \"a\"}")).not.toBe(false);

      expect(parse("{x :: Number: 5}")).toBe(false);
      expect(parse("{ ref ref y :: String: 5 }")).toBe(false);
      expect(parse("{ ref ref: 5 }")).toBe(false);
    });

    it("should parse imports", function() {
      expect(parse('import modname as G')).not.toBe(false);
      expect(parse('import file("modname.arr") as G')).not.toBe(false);
      expect(parse('import file-is-not-special("modname.arr") as G')).not.toBe(false);
      expect(parse('import gdrive(a) as G')).toBe(false);
      expect(parse('import gdrive("a") as G')).not.toBe(false);
      expect(parse('import gdrive("a", "b") as G')).not.toBe(false);
      expect(parse('import gdrive() as G')).toBe(false);
    });

    it("should parse includes", function() {
      expect(parse('include modname')).not.toBe(false);
      expect(parse('include file("modname.arr")')).not.toBe(false);
      expect(parse('include file-is-not-special("modname.arr")')).not.toBe(false);
      expect(parse('include gdrive(a)')).toBe(false);
      expect(parse('include gdrive("a")')).not.toBe(false);
      expect(parse('include gdrive("a", "b")')).not.toBe(false);
      expect(parse('include gdrive()')).toBe(false);
    });

    it("should parse new equality operators", function() {
      expect(parse('o <=> o2')).not.toBe(false);
      expect(parse('o <= > o2')).toBe(false);
      expect(parse('o < = > o2')).toBe(false);
      expect(parse('o < => o2')).toBe(false);
      expect(parse('o =~ o2')).not.toBe(false);
      expect(parse('o == o2')).not.toBe(false);

      expect(parse('check: o is== o2 end')).not.toBe(false);
      expect(parse('check: o is == o2 end')).toBe(false);
      expect(parse('check: o is=~ o2 end')).not.toBe(false);
      expect(parse('check: o is =~ o2 end')).toBe(false);
      expect(parse('check: o is<=> o2 end')).not.toBe(false);
      expect(parse('check: o is <=> o2 end')).toBe(false);

      expect(parse('check: o is-not== o2 end')).not.toBe(false);
      expect(parse('check: o is-not == o2 end')).toBe(false);
      expect(parse('check: o is-not=~ o2 end')).not.toBe(false);
      expect(parse('check: o is-not =~ o2 end')).toBe(false);
      expect(parse('check: o is-not<=> o2 end')).not.toBe(false);
      expect(parse('check: o is-not <=> o2 end')).toBe(false);
    });

    it("should parse examples", function() {
      expect(parse('examples: 5 is 5 end')).not.toBe(false);
    });

    it("should parse ref cases bindings", function() {
      expect(parse('cases(List) l: | link(ref first, rest) => 5 end')).not.toBe(false);
      expect(parse('cases(List) l: | link(ref first, ref rest) => 5 end')).not.toBe(false);
      expect(parse('cases(List) l: | link(first, ref rest) => 5 end')).not.toBe(false);
      expect(parse('cases(List) l: | link(ref first :: Number, rest) => 5 end')).not.toBe(false);
      expect(parse('cases(List) l: | link(ref first :: Number, rest :: Number) => 5 end')).not.toBe(false);
      expect(parse('cases(List) l: | link(first :: Number, ref rest :: Number) => 5 end')).not.toBe(false);
      expect(parse('cases(List) l: | link(ref first :: Number, ref rest :: Number) => 5 end')).not.toBe(false);

      expect(parse('cases(List) l: link(ref ref) => 5 end')).toBe(false);
    });

    it("should parse type parameters on methods", function() {
      expect(parse('method<a>(self): self end')).not.toBe(false);
      expect(parse('method<a,b>(self): self end')).not.toBe(false);
      expect(parse('method<a,b,c>(self): self end')).not.toBe(false);
      expect(parse('{ method m<a>(self): self end }')).not.toBe(false);
      expect(parse('{ method m<a,b>(self): self end }')).not.toBe(false);
      expect(parse('{ method m<a,b,c>(self): self end }')).not.toBe(false);
      expect(parse('data D: | var1 with: method m<a>(self): 5 end sharing: method m2<a>(self): 5 end end')).not.toBe(false);
      expect(parse('data D: | var1 with: method m<a,b>(self): 5 end sharing: method m2<a,b>(self): 5 end end')).not.toBe(false);
      expect(parse('data D: | var1 with: method m<a,b,c>(self): 5 end sharing: method m2<a,b,c>(self): 5 end end')).not.toBe(false);
    });

    it("should parse rec statements", function() {
      expect(parse('rec a = 10')).not.toBe(false);
      expect(parse('rec ohn = lz(1, lam(): ohn end)')).not.toBe(false);
      expect(parse('rec = 5')).toBe(false);
    });

    it("should not parse bracket exprs", function() {
      expect(parse('o.[x]')).toBe(false);
    });

    it("should not parse string keys", function() {
      expect(parse('{"x x": true}')).toBe(false);
      expect(parse("{'x x': true}")).toBe(false);
    });

    it("should parse block comments", function() {
      expect(parse('#| anything |#')).not.toBe(false);
      expect(parse('#| even with  | pipes |#')).not.toBe(false);
      expect(parse('#|||#')).not.toBe(false);
      expect(parse('#||||||#')).not.toBe(false);
      expect(parse('#| | # | # | # | # |#')).not.toBe(false);
      expect(parse('#| back to |##| back |#')).not.toBe(false);
      expect(parse('#||##||#')).not.toBe(false);
      expect(parse('#|\n|#')).not.toBe(false);
      expect(parse('#||#')).not.toBe(false);
      expect(parse(' #||#')).not.toBe(false);
      expect(parse('\n#||#')).not.toBe(false);
      expect(parse('\r\n#||#')).not.toBe(false);
      // Unterminated comments
      expect(parse('#| #| |#')).toBe(false);
      expect(parse('#|#||#')).toBe(false);
      expect(parse('#|#|#')).toBe(false);
      // Nested comments
      expect(parse('x = #| not #| parsed |# here either |# 5')).not.toBe(false);

      expect(parse('#| |# # extra hash for line comment')).not.toBe(false);
      expect(parse('#| |# closing hash doesn\'t count as line comment')).toBe(false);

      expect(parse('#| |#\nfun f():\n  5\nend\n#| |#')).not.toBe(false);

      // mid-expression
      expect(parse('#| |#\nfun f():\n  5 + #| |#\n    5\nend\n#| |#')).not.toBe(false);
      expect(parse('lam(x #| stuff |#, y): x + y end')).not.toBe(false);
      expect(parse('lam(x #| two |##| comments|#, y): x + y end')).not.toBe(false);

      // PyretDoc style?
      expect(parse('#|\n' +
                   '# Things\n' +
                   '# about \n' +
                   '# the \n' +
                   '# program \n' +
                   '|#')).not.toBe(false);

      // notices the _first_ close comment
      expect(parse('#| |# |#')).toBe(false);


    });

    it("should not ignore the line after an empty hash comment", function(){
      expect(parse('#\n1')).not.toBe(false);
      expect(parse('#\n{1:2}')).toBe(false);
    });

    it("should parse tables", function() {
      expect(parse("table 3: row 3 end")).toBe(false);

      expect(parse("table: h1 row: end")).not.toBe(false);
      expect(parse("table: row: 3 end")).not.toBe(false);

      expect(parse("table: h1 h2 row: 3 row: 4 end")).toBe(false);
      expect(parse("table: h1 row: 3 3 row: 4 end")).toBe(false);
      expect(parse("table: h1 row: 3 row: 4 4 end")).toBe(false);
      expect(parse("table: h1 h2 row 3, 4 end")).toBe(false);

      expect(parse("table: h1 row: 3 end")).not.toBe(false);
      expect(parse("table: h1 row: 3 row: 4 end")).not.toBe(false);
      expect(parse("table: h1, h2 row: 3, 3 row: 4, 4 end")).not.toBe(false);
      expect(parse("table: h1, h2 row: 3 + 3, 3 * 3 end")).not.toBe(false);
    });

    it("should parse table loaders", function() {
      expect(parse("load-table: h1, h2, h3 source: my-src end")).not.toBe(false);
      expect(parse("load-table: h1 sanitize h1 using s1 source: s sanitize h2 using s2 end")).not.toBe(false);

      // Should be well-formedness errors
      expect(parse("load-table: end")).not.toBe(false);
      expect(parse("load-table: source: s end")).not.toBe(false);
      expect(parse("load-table: h1 sanitize h1 using s1 end")).not.toBe(false);

      expect(parse("load-table: h1 using s1 source: src end")).toBe(false);
      expect(parse("load-table: h1 h2 source: s end")).toBe(false);
    });

    it("should parse tuples", function() {
      expect(parse("{1; 2; 3}")).not.toBe(false);
      expect(parse("{4 * 3; 3; 10 - 2}")).not.toBe(false);
      expect(parse("{12; 2 + 3; 14;}")).not.toBe(false);
      expect(parse("{{124; 124; 12}")).toBe(false);
      expect(parse("{word; hello; there; pyret")).toBe(false);
      expect(parse("234; hi; bad}")).toBe(false);
      //expect(parse("{one}")).not.toBe(false);
    });

    it("should parse tuple-get", function() {
      expect(parse("tup.{2}")).not.toBe(false);
      expect(parse("one.{3 + 4}")).toBe(false);
      expect(parse("two.{4")).toBe(false);
      expect(parse("two.{two}")).toBe(false);
      expect(parse("hello.5}")).toBe(false);
      expect(parse("tup. {2}")).not.toBe(false);
      expect(parse("tup\n\n\n\n\n    \n\n\n.{2}")).not.toBe(false);
      expect(parse("{1;2;3}.{2}")).not.toBe(false);
      expect(parse("(5 + 6).{2}")).not.toBe(false);
      expect(parse("f().{2}")).not.toBe(false);
    });

    it("should parse tuple binding", function() {
     expect(parse("{x;y} = {1;2}")).not.toBe(false);
     expect(parse("{x;y;z = } {1;2}")).toBe(false);
     expect(parse("{1 + 3; hello} = t")).toBe(false);
     expect(parse("{v;v;b;u} = t")).not.toBe(false);
    });

    it("should parse tuple annotations", function(){
     expect(parse("fun f(tup:: {Number; String; Number}): tup.{0} end")).not.toBe(false);
     expect(parse("fun f(tup:: {Number; String; Number): tup.{0} end")).toBe(false);
     expect(parse("fun f(tup:: {hello; there}): hello end")).not.toBe(false);
     expect(parse("fun f(tup:: {Number}): tup.{1} end")).not.toBe(false);
     expect(parse("fun f(tup:: {Number; {String; Number; {hello; there}}; {hi; what}}): tup.{1} end")).not.toBe(false);
    });

    it("should parse tuple binding", function() {
      expect(parse("for each({k;v;} from elts): k end")).not.toBe(false);
    });

    it("should parse reactors", function() {
      expect(parse("reactor: start-with: 5, on-tick: lam(x): x + 1 end end")).not.toBe(false);
      expect(parse("reactor: error-later-than-parsing: error end")).not.toBe(false);
      expect(parse("reactor: end")).toBe(false);
      expect(parse("reactor end")).toBe(false);
    });

    it("should parse spy", function() {
      expect(parse("spy: x end")).not.toBe(false);
      expect(parse("spy: 5 end")).toBe(false);
      expect(parse("spy \"five\": x end")).not.toBe(false);
      expect(parse("spy \"five\": x: 5 end")).not.toBe(false);
    });
  });


});
