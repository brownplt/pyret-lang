const R = require("requirejs");
R(["../../../build/phase1/js/pyret-tokenizer", "../../../build/phase1/js/pyret-parser", "fs"], function(T, G, fs) {
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
  describe("parsing", function() {
    it("should parse new-style imports with types", function() {
//      expect(parse("import ast as A, AT")).not.toBe(false);
    });
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
      expect(parse("provide-types { List :: List, x :: (Number -> String) }")).not.toBe(false);
    });

    it("shouldn't parse expressions in provide-types", function() {
      expect(parse("provide-types { List :: 5 + 5 }")).toBe(false);
      expect(parse("provide-types { List :: List, x :: lam(x): x end }")).toBe(false);
    });

    it("shouldn't allow english ops as identifiers, no matter the whitespace", function() {
      // NOTE(joe): See John Ericson's comment about changing the tokenizer
      // at https://github.com/brownplt/pyret-lang/pull/220#issuecomment-48685416
      // if this turns into a regression
      expect(parse("or=false")).toBe(false);
      expect(parse("and=false")).toBe(false);
      expect(parse("or =false")).toBe(false);
      expect(parse("and =false")).toBe(false);
      expect(parse("or= false")).toBe(false);
      expect(parse("and= false")).toBe(false);
      expect(parse("or = false")).toBe(false);
      expect(parse("and = false")).toBe(false);
      expect(parse("or \n = \n  false")).toBe(false);
      expect(parse("and \n = \n false")).toBe(false);
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

    it("should notice parse errors", function() {
      expect(parse("bad end")).toBe(false);
      expect(parse("provide-types { List :: List } end")).toBe(false);
    })

  });
});

