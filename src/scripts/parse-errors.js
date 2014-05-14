const R = require("requirejs");

R(["../../build/phase1/js/bootstrap-un-tokenizer", "../../build/phase1/js/bootstrap-un-parser", "fs"], function(T, G, fs) {
  const dialect = process.argv[2];
  const data = fs.readFileSync(process.argv[2], {encoding: "utf-8"});
  const toks = T.Tokenizer;
  toks.tokenizeFrom(data);
  // while (toks.hasNext())
  //   console.log(toks.next().toString(true));
  var parsed = G.BootstrapGrammar.parse(toks);
  if (parsed) {
    console.log("Result:");
    var countParses = G.BootstrapGrammar.countAllParses(parsed);
    console.log("There are " + countParses + " potential parses");
    if (countParses === 1) {
      var ast = G.BootstrapGrammar.constructUniqueParse(parsed);
      console.log("AST constructed");
      var astStr = ast.toString(true);
      console.log("ast.toString().length = " + astStr.length)
      console.log(astStr);
      return ast;
    } else {
      console.log("There were " + countParses + " parses total.");
      var asts = G.BootstrapGrammar.constructAllParses(parsed);
      for (var i = 0; i < asts.length; i++) {
        console.log("Parse " + i + ": " + asts[i].toString());
      }
      console.log("There were " + countParses + " parses total.");
      // return asts;
    }
  } else {
    console.log("Invalid parse: you screwed up.");
    console.log("Next token is " + toks.curTok.toString(true) + " at " + toks.curTok.pos.toString(true));
  }
});
