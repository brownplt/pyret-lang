// usage: node nearley-grammar-test.js ../../tests-new/ts-simple-output/assign.arr

const nearley = require("nearley");
const grammar = require("./pyret-grammar.js");
const fs = require("fs");

const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
const data = fs.readFileSync(process.argv[2], {encoding: "utf8"});
parser.feed(data);

//console.log(JSON.stringify(parser.results[0], null, 2));
console.log(parser.results[0].toString(true));

