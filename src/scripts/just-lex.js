const R = require("requirejs");

R.config({
  paths: {
    'jglr': "../../lib/jglr/",
    'pyret-base': "../../build/phaseA",
  }
});
R(["pyret-base/js/pyret-tokenizer", "pyret-base/js/pyret-parser", "fs", "tokenize"], function(T, G, fs, Tok) {
  const data = fs.readFileSync(process.argv[2], {encoding: "utf-8"});
  console.log(process.argv[2]);


  var t = new Tok.Tokenizer(data, Tok.spec);
  var startNew = process.hrtime();
  var newAns = [];
  while (t.hasNext()) {
    var tok = t.next();
    newAns.push(tok);
  }
  var endNew = process.hrtime(startNew);


  const toks = T.Tokenizer;
  toks.tokenizeFrom(data);
  var trueAns = [];
  var startTrue = process.hrtime();
  while (toks.hasNext()) {
    var tok = toks.next();
    trueAns.push(tok);
  }
  var endTrue = process.hrtime(startTrue);
  console.log("=========================================================");

  const NS_PER_SEC = 1e9;
  [{"original  ": endTrue},
   {"new lexer ": endNew}].forEach(function(entry) {
     for (var k in entry) {
       entry[k] = ((entry[k][0] * NS_PER_SEC + entry[k][1]) / NS_PER_SEC).toFixed(10);
     }
     console.log(entry);
   });


  for (var i = 0; i < Math.max(trueAns.length, newAns.length); i++) {
    if (trueAns[i] === undefined && newAns[i] !== undefined) {
      var sTrue = "<undefined>";
      var sNew = newAns[i].toRepr(true) + " " + newAns[i].pos.toString(true);
      console.log((""+i).padEnd(5), ": ", sTrue.padEnd(50), sNew);
    } else if (trueAns[i] !== undefined && newAns[i] === undefined) {
      var sNew = "<undefined>";
      var sTrue = trueAns[i].toRepr(true) + " " + trueAns[i].pos.toString(true);
      console.log((""+i).padEnd(5), ": ", sTrue.padEnd(50), sNew);
    } else if (trueAns[i].toRepr(true) !== newAns[i].toRepr(true) &&
               (""+trueAns[i].value).trim() !== (""+newAns[i].value).trim()) {
      var sNew = newAns[i].toRepr(true) + " " + newAns[i].pos.toString(true);
      var sTrue = trueAns[i].toRepr(true) + " " + trueAns[i].pos.toString(true);
      console.log((""+i).padEnd(5), ": ", sTrue.padEnd(50), sNew);
    } else {
      var sTrue = "";
      if (trueAns)
        sTrue = trueAns[i].toRepr(true) + " " + trueAns[i].pos.toString(true);
      //console.log((""+i).padEnd(5), ": ", sTrue.padEnd(50));
    }
  }
  
  // var longestName = 0;
  // for (var t in toks.Tokens) {
  //   var tok = toks.Tokens[t];
  //   if (tok.name.length > longestName) longestName = tok.name.length;
  // }
  // for (var t in toks.Tokens) {
  //   var tok = toks.Tokens[t];
  //   console.log(tok.name.padEnd(longestName) + " ========> " + tok.val);
  // }
});
