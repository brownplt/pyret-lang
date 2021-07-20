const nearley = require("nearley");
const grammar = require("./nearley-grammar.js");

const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));

parser.feed("2 + 3");

console.log(JSON.stringify(parser.results, null, 2));

