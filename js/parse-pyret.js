console.log("Loading tokenizer");
const T = require('./pyret-tokenizer.js');
console.log("Loading parser");
const G = require('./pyret-parser.js');
console.log("Done");

const fs = require('fs');

//var data = fs.readFileSync("../filomino.arr", {encoding: "utf-8"});
var data = "#lang pyret\n\nimport \"foo\" as bar\na";

const toks = new T.Tokenizer(data, true);
// while (toks.hasNext())
//   console.log(toks.next().toString(true));
var parsed = G.PyretGrammar.parse(toks);
console.log(G.PyretGrammar.printSPPFasDot());
console.log(G.PyretGrammar.printGSSasDot());
// console.log("Result:");
// console.log(parsed.toString(true));

