const R = require("requirejs");

R.config({
  paths: {
    'jglr': "../../lib/jglr/",
    'pyret-base': "../../build/phaseA"
  }
});
R(["pyret-base/js/pyret-tokenizer", "pyret-base/js/pyret-parser", "fs"], function(T, G, fs) {
  var start = process.hrtime();
  const data = fs.readFileSync(process.argv[2], {encoding: "utf-8"});
  var readingTime = process.hrtime(start);
  const toks = T.Tokenizer;
  toks.tokenizeFrom(data);
  // while (toks.hasNext())
  //   console.log(toks.next().toString(true));
  start = process.hrtime();
  var parsed = G.PyretGrammar.parse(toks);
  var parsedTime = process.hrtime(start);
  if (parsed) {
    console.log("Result:");
    start = process.hrtime();
    var countParses = G.PyretGrammar.countAllParses(parsed);
    var countTime = process.hrtime(start);
    console.log("There are " + countParses + " potential parses");
    if (countParses === 1) {
      start = process.hrtime();
      var ast = G.PyretGrammar.constructUniqueParse(parsed);
      var astTime = process.hrtime(start);
      console.log("AST constructed");
      start = process.hrtime()
      var astStr = ast.toString(true);
      var tostrTime = process.hrtime(start);

      const NS_PER_SEC = 1e9;
      [{"reading  ": readingTime},
       {"parsing  ": parsedTime},
       {"counting ": countTime},
       {"ast      ": astTime},
       {"toString ": tostrTime}].forEach(function(entry) {
         for (var k in entry) {
           entry[k] = ((entry[k][0] * NS_PER_SEC + entry[k][1]) / NS_PER_SEC).toFixed(10);
         }
         console.log(entry);
       });

      console.log("ast.toString().length = " + astStr.length)
      // console.log(astStr);
      return ast;
    } else {
      var asts = G.PyretGrammar.constructAllParses(parsed);
      for (var i = 0; i < asts.length; i++) {
        console.log("Parse " + i + ": " + asts[i].toString());
      }
      // return asts;
    }
  } else {
    console.log("Invalid parse: you screwed up.");
    console.log("Next token is " + toks.curTok.toString(true) + " at " + toks.curTok.pos.toString(true));
  }
});
