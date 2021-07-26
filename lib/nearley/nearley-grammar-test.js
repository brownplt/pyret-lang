const nearley = require("nearley");
const grammar = require("./grammar.js");

const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar), { keepHistory: true });

parser.feed("- 2 + \n3");

//console.log(JSON.stringify(parser.table, null, 2));
console.log(parser.results[0].toString(true));

