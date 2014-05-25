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
      expect(parse("import ast as A, AT")).not.toBe(false);
    });

    it("should notice parse errors", function() {
      expect(parse("bad end")).toBe(false);
    })
  });
});

