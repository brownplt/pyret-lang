console.log("Loading tokenizer");
const T = require('./pyret-tokenizer.js');
console.log("Loading parser");
const G = require('./pyret-parser.js');
console.log("Done");

const fs = require('fs');

const file = process.argv[process.argv.length - 1];
var data = fs.readFileSync(file, {encoding: "utf-8"});
//var data = "#lang pyret\n\nif (f(x) and g(y) and h(z) and i(w) and j(u)): true else: false end";

const toks = T.Tokenizer;
toks.tokenizeFrom(data);
// while (toks.hasNext())
//   console.log(toks.next().toString(true));
var parsed = G.PyretGrammar.parse(toks);
// console.log(G.PyretGrammar.printSPPFasDot());
// console.log(G.PyretGrammar.printGSSasDot());
console.log("Result:");
var countParses = G.PyretGrammar.countAllParses(parsed);
console.log("There are " + countParses + " potential parses");
// var out = fs.createWriteStream("out.txt");
// out.write(JSON.stringify(JSON.decycle(parsed)));
// out.end();
// console.log("Done JSONing");
var posViolations = G.PyretGrammar.checkPositionContainment(parsed);
if (posViolations) {
  console.log("Not all nodes conain their children!");
} else {
  if (countParses === 1) {
    var ast = G.PyretGrammar.constructUniqueParse(parsed);
    console.log(ast.toString());
  } else {
    var asts = G.PyretGrammar.constructAllParses(parsed)
    for (var i = 0; i < asts.length; i++) {
      console.log("Parse " + i + ": " + asts[i].toString());
    }
  }
}
// out.write(ast.toString(true));
console.log("Done");
// out.end();

