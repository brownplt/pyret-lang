const R = require("requirejs");

R.config({
  paths: {
    'jglr': "../../lib/jglr",
    'pyret-base': "../../build/phaseA",
    'src-base/js': "../../src/js/base"
  }
});
R(["pyret-base/js/pyret-tokenizer", "pyret-base/js/pyret-parser", "fs", "src-base/js/pyret-tokenizer-old"], function(T, G, fs, Told) {
  function toTime(hrtime) {
    const NS_PER_SEC = 1e9;
    return (hrtime[0] * NS_PER_SEC + hrtime[1]) / NS_PER_SEC;
  }
  const data = fs.readFileSync(process.argv[2], {encoding: "utf-8"});
  console.log(process.argv[2]);
  
  var t = T.Tokenizer;
  t.tokenizeFrom(data);
  var startNew = process.hrtime();
  var newAns = [];
  var tokHist = {};
  while (t.hasNext()) {
    var tok = t.next();
    newAns.push(tok);
    // console.log(tok.toRepr(true) + tok.pos.toString(true));
    if (tokHist[tok.name] === undefined) tokHist[tok.name] = 0;
    tokHist[tok.name]++;
  }
  var endNew = process.hrtime(startNew);


  const toks = Told.Tokenizer;
  toks.tokenizeFrom(data);
  var oldAns = [];
  var startOld = process.hrtime();
  while (toks.hasNext()) {
    var tok = toks.next();
    oldAns.push(tok);
  }
  var endOld = process.hrtime(startOld);
  console.log("=========================================================");

  var stats =
      [{"original  ": endOld},
       {"new lexer ": endNew}];
  stats.forEach(function(entry) {
    for (var k in entry) {
      entry[k] = toTime(entry[k]).toFixed(10);
    }
  });
  console.log({"RATIO     ": (stats[0]["original  "] / stats[1]["new lexer "]).toFixed(10)});
  console.log({"num NAMES ": tokHist["NAME"]});
  console.log({"total toks": newAns.length});
  var stats = [];
  for (var tok in t.times) {
    var sumNew = t.times[tok].reduce((a, b) => a + b);
    var avgNew = sumNew / t.times[tok].length;
    if (toks.times[tok]) {
      var sumOld = toks.times[tok].reduce((a, b) => a + b);
      var avgOld = sumOld / toks.times[tok].length;
      stats.push({name: tok,
                  sumNew, numNew: t.times[tok].length, avgNew,
                  sumOld, numOld: toks.times[tok].length, avgOld,
                  ratio: (avgOld / avgNew)});
    } else {
      stats.push({name: tok,
                  sumNew, numNew: t.times[tok].length, avgNew,
                  sumOld: 0, numOld: 0, avgOld: 0,
                  ratio: (avgOld / 0)});
    }
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
                            "sumOld:", stat.sumOld.toFixed(10),
                            "numOld:", stat.numOld.toString().padEnd(5),
                            "avgOld:", stat.avgOld.toFixed(10),
                            "      ratio (old/new):", (stat.avgOld / stat.avgNew).toFixed(10)
                           )
               );
  
  // stats.forEach(function(entry) {
  //   for (var k in entry) {
  //     console.log(entry);
  //   }
  // });
  // console.log(tokHist);



  var differences = false;
  for (var i = 0; i < Math.max(oldAns.length, newAns.length); i++) {
    if (oldAns[i] === undefined && newAns[i] !== undefined) {
      differences = true;
    } else if (oldAns[i] !== undefined && newAns[i] === undefined) {
      differences = true;
    } else if (oldAns[i].name !== newAns[i].name ||
               oldAns[i].pos.toString(true) !== newAns[i].pos.toString(true) ||
               (""+oldAns[i].value).trim() !== (""+newAns[i].value).trim()) {
      differences = true;
    }
  }
  if (differences) {
    console.log("\nDifferences");
    console.log("Tok# ", ": ", "OLD TOKEN".padEnd(50), "NEW TOKEN");
  }
  for (var i = 0; i < Math.max(oldAns.length, newAns.length); i++) {
    if (oldAns[i] === undefined && newAns[i] !== undefined) {
      var sOld = "<undefined>";
      var sNew = newAns[i].toRepr(true) + " " + newAns[i].pos.toString(true);
      console.log((""+i).padEnd(5), ": ", sOld.padEnd(50), sNew);
    } else if (oldAns[i] !== undefined && newAns[i] === undefined) {
      var sNew = "<undefined>";
      var sOld = oldAns[i].toRepr(true) + " " + oldAns[i].pos.toString(true);
      console.log((""+i).padEnd(5), ": ", sOld.padEnd(50), sNew);
    } else if (oldAns[i].name !== newAns[i].name ||
               oldAns[i].pos.toString(true) !== newAns[i].pos.toString(true) ||
               (""+oldAns[i].value).trim() !== (""+newAns[i].value).trim()) {
      var sNew = newAns[i].toRepr(true) + " " + newAns[i].pos.toString(true);
      var sOld = oldAns[i].toRepr(true) + " " + oldAns[i].pos.toString(true);
      console.log((""+i).padEnd(5), ": ", sOld.padEnd(50), sNew);
    } else {
      var sNew = "";
      if (newAns)
        sNew = newAns[i].toRepr(true) + " " + newAns[i].pos.toString(true);
      //console.log((""+i).padEnd(5), ": ", sNew.padEnd(50));
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
