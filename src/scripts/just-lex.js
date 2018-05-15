const R = require("requirejs");

R.config({
  paths: {
    'jglr': "../../lib/jglr",
    'pyret-base': "../../build/phaseA",
    'src-base': "../../src/js/base"
  }
});
R(["pyret-base/js/pyret-tokenizer", "pyret-base/js/pyret-parser", "fs", "pyret-base/js/pyret-tokenizer2"], function(T, G, fs, Tok) {
  const data = fs.readFileSync(process.argv[2], {encoding: "utf-8"});
  console.log(process.argv[2]);
  
  var t = new Tok.Tokenizer(Tok.spec);
  t.tokenizeFrom(data);
  var startNew = process.hrtime();
  var newAns = [];
  var tokHist = {};
  while (t.hasNext()) {
    var tok = t.next();
    newAns.push(tok);
    if (tokHist[tok.name] === undefined) tokHist[tok.name] = 0;
    tokHist[tok.name]++;
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
  var stats =
      [{"original  ": endTrue},
       {"new lexer ": endNew}];
  stats.forEach(function(entry) {
    for (var k in entry) {
      entry[k] = ((entry[k][0] * NS_PER_SEC + entry[k][1]) / NS_PER_SEC).toFixed(10);
    }
  });
  console.log({"RATIO     ": (stats[0]["original  "] / stats[1]["new lexer "]).toFixed(10)});
  console.log({"num NAMES ": tokHist["NAME"]});
  console.log({"total toks": newAns.length});
  var stats = [];
  for (var tok in t.times) {
    var sumNew = t.times[tok].reduce((a, b) => a + b);
    var avgNew = sumNew / t.times[tok].length;
    var sumTrue = toks.times[tok].reduce((a, b) => a + b);
    var avgTrue = sumTrue / toks.times[tok].length;
    stats.push({name: tok,
                sumNew, numNew: t.times[tok].length, avgNew,
                sumTrue, numTrue: toks.times[tok].length, avgTrue,
                ratio: (avgTrue / avgNew)});
  }
  stats.sort((a, b) => (a.ratio - b.ratio));
  var maxTokLength = 0;
  stats.forEach(function(stat) { maxTokLength = Math.max(maxTokLength, stat.name.length); });
  stats.forEach((stat) => 
                console.log(stat.name.padEnd(maxTokLength), ":",
                            "sumNew:", stat.sumNew.toFixed(10),
                            "numNew:", stat.numNew.toString().padEnd(5),
                            "avgNew:", stat.avgNew.toFixed(10),
                            "     ",
                            "sumTrue:", stat.sumTrue.toFixed(10),
                            "numTrue:", stat.numTrue.toString().padEnd(5),
                            "avgTrue:", stat.avgTrue.toFixed(10),
                            "      ratio:", (stat.avgTrue / stat.avgNew).toFixed(10)
                           )
               );
  
  // stats.forEach(function(entry) {
  //   for (var k in entry) {
  //     console.log(entry);
  //   }
  // });
  // console.log(tokHist);


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
