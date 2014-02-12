const R = require("requirejs");

R(["../../build/phase1/js/pyret-tokenizer", "../../build/phase1/js/pyret-parser-comp", "fs"], function(T, G, fs) {
  const data = fs.readFileSync(process.argv[2], {encoding: "utf-8"});
  const toks = T.Tokenizer;
  toks.tokenizeFrom(data);
  // while (toks.hasNext())
  //   console.log(toks.next().toString(true));
  var parsed = G.PyretGrammar.parse(toks);
  if (parsed) {
    console.log("Result:");
    var countParses = G.PyretGrammar.countAllParses(parsed);
    console.log("There are " + countParses + " potential parses");
    var posViolations = G.PyretGrammar.checkPositionContainment(parsed);
    if (posViolations) {
      console.log("Not all nodes contain their children!");
    } else {
      if (countParses === 1) {
        var ast = G.PyretGrammar.constructUniqueParse(parsed);
        console.log("AST constructed");
        var astStr = ast.toString();
        console.log("ast.toString().length = " + astStr.length)
        console.log(astStr);
        return ast;
      } else {
        var asts = G.PyretGrammar.constructAllParses(parsed);
        for (var i = 0; i < asts.length; i++) {
          console.log("Parse " + i + ": " + asts[i].toString());
        }
        // return asts;
      }
    }
  } else {
    console.log("Invalid parse: you screwed up.");
    console.log("Next token is " + JSON.stringify(toks.next()));
  }
});
