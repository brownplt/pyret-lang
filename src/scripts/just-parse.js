const R = require("requirejs");

R.config({
  paths: {
    'jglr': "../../lib/jglr/",
    'pyret-base': "../../build/phaseA",
    'src-base/js': "../../src/js/base"
  }
});
R(["pyret-base/js/pyret-tokenizer", "src-base/js/pyret-tokenizer-old", "pyret-base/js/pyret-parser", "fs"], function(T, Told, G, fs) {
  function toTime(hrtime) {
    const NS_PER_SEC = 1e9;
    return (hrtime[0] * NS_PER_SEC + hrtime[1]) / NS_PER_SEC;
  }
  var start = process.hrtime();
  console.log(process.argv[2]);
  const data = fs.readFileSync(process.argv[2], {encoding: "utf-8"});
  var readingTime = process.hrtime(start);
  const toks2 = T.Tokenizer;
  const toks1 = Told.Tokenizer;
  toks1.tokenizeFrom(data);
  toks2.tokenizeFrom(data);
  // while (toks.hasNext())
  //   console.log(toks.next().toString(true));
  start = process.hrtime();
  var parsed = G.PyretGrammar.parse(toks2);
  var parsedTime2 = process.hrtime(start);
  start = process.hrtime();
  var parsed = G.PyretGrammar.parse(toks1);
  var parsedTime1 = process.hrtime(start);
  toks1.tokenizeFrom(data);
  toks2.tokenizeFrom(data);
  start = process.hrtime();
  var parsed = G.PyretGrammar.parse(toks1);
  var parsedTime1 = process.hrtime(start);
  start = process.hrtime();
  var parsed = G.PyretGrammar.parse(toks2);
  var parsedTime2 = process.hrtime(start);
  if (parsed) {
    //console.log("Result:");
    start = process.hrtime();
    var countParses = G.PyretGrammar.countAllParses(parsed);
    var countTime = process.hrtime(start);
    //console.log("There are " + countParses + " potential parses");
    if (countParses === 1) {
      start = process.hrtime();
      var ast = G.PyretGrammar.constructUniqueParse(parsed);
      var astTime = process.hrtime(start);
      //console.log("AST constructed");
      start = process.hrtime()
      var astStr = ast.toString(true);
      var tostrTime = process.hrtime(start);

      [ //{"reading  ": toTime(readingTime).toFixed(10)},
        //{"parsing1 ": toTime(parsedTime1).toFixed(10)},
        //{"parsing2 ": toTime(parsedTime2).toFixed(10)},
        {"RATIO    ": (toTime(parsedTime1) / toTime(parsedTime2)).toFixed(10)},
        //{"counting ": toTime(countTime).toFixed(10)},
        //{"ast      ": toTime(astTime).toFixed(10)},
        //{"toString ": toTime(tostrTime).toFixed(10)}
      ].forEach((e) => console.log(e));

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
  } else {
    console.log("Invalid parse: you screwed up.");
    console.log("Next token is " + toks2.curTok.toRepr(true) + " at " + toks2.curTok.pos.toString(true));
  }
});
