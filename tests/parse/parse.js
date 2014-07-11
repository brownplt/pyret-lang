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

    it("should notice parse errors", function() {
      expect(parse("bad end")).toBe(false);
      expect(parse("provide-types { List :: List } end")).toBe(false);
    });

    it("should parse angle brackets without whitespace as type instantiations", function() {
      expect(parse("(map<A, B>)")).not.toBe(false);
      expect(parse("map<A, B>(id)")).not.toBe(false);
      expect(parse("(map < A, B > (id))")).toBe(false);
      expect(parse("(map\n<\nA, B\n>\n(id))")).toBe(false);
      expect(parse("map<A,\nB>(id)")).not.toBe(false);
    });

    it("should parse angle brackets with whitespace as gt/lt", function() {
      expect(parse("1\n<\n2 or false\n B > (id)")).not.toBe(false);
      expect(parse("1<\n2 or false, B > (id)")).toBe(false);
    });

    it("should not care about whitespace and angle brackets in annotations", function() {
      expect(parse("fun<A>print(): end")).not.toBe(false);
      expect(parse("fun< A>print(): end")).not.toBe(false);
      expect(parse("fun <A>print(): end")).not.toBe(false);
      expect(parse("fun < A>print(): end")).not.toBe(false);
      expect(parse("fun<A >print(): end")).not.toBe(false);
      expect(parse("fun<A> print(): end")).not.toBe(false);
      expect(parse("fun<A > print(): end")).not.toBe(false);
    });

    it("should not treat (...) after operators as application", function() {
      expect(parse("(true) or (false)")).not.toBe(false);
      expect(parse("(true) < (false)")).not.toBe(false);
      expect(parse("(true) > (false)")).not.toBe(false);
    });

    it("should not mind ; at EOF", function() {
      expect(parse("lam<T>(x :: T) -> T: x;")).not.toBe(false);
    });

    it("should not mind ; at EOL, and then another statement", function() {
      var a = "  fun<T> x(x :: T) -> T: x;";
      expect(parse("block:\n" + a + "\n" + a + "end")).not.toBe(false);
      expect(parse("block:\n" + a + " \n" + a + "end")).not.toBe(false);
    });

    it("should require whitespace after :: and =>", function() {
      expect(parse("cases (T) x: | Foo() =>(true) end")).toBe(false);
      expect(parse("cases (T) x: | Foo() => (true) end")).not.toBe(false);
      expect(parse("cases (T) x: | Foo() =>\n(true) end")).not.toBe(false);
      expect(parse("block: dog ::Cat = really-huh end")).toBe(false);
      expect(parse("block: dog :: Cat = really-huh end")).not.toBe(false);
      expect(parse("block: dog :: Cat =\nreally-huh end")).not.toBe(false);
    });

    it("should trait (...) as grouping after ,", function() {
      expect(parse("[list: x,(x)]")).not.toBe(false);
      expect(parse("[list: x , (x)]")).not.toBe(false);
      expect(parse("[list: x ,\n(x)]")).not.toBe(false);
    });
    it("should trait (...) as grouping after :", function() {
      expect(parse("{ asdf:(asdf) }")).not.toBe(false);
      expect(parse("{ asdf : (asdf) }")).not.toBe(false);
      expect(parse("{ asdf :\n(asdf) }")).not.toBe(false);
    });

    it("should trait (...) as grouping after =", function() {
      expect(parse("block: x=(x) end")).not.toBe(false);
      expect(parse("block: x = (x) end")).not.toBe(false);
      expect(parse("block: x =\n(x) end")).not.toBe(false);
    });

    it("should trait (...) as grouping after :=", function() {
      expect(parse("block: x:=(x) end")).not.toBe(false);
      expect(parse("block: x := (x) end")).not.toBe(false);
      expect(parse("block: x :=\n(x) end")).not.toBe(false);
    });

    it("should trait (...) as grouping after ;", function() {
      expect(parse("block: lam(x): x;(x);")).not.toBe(false);
      expect(parse("block: lam(x): x ; (x);")).not.toBe(false);
      expect(parse("block: lam(x): x ;\n(x);")).not.toBe(false);
    });

  });
});
