const R = require("requirejs");

R.config({
  paths: {
    'jglr': "../../lib/jglr",
    'pyret-base': "../../src/arr/compiler",
    'src-base/js': "../../src/js/base"
  }
});
R(["pyret-base/pyret-tokenizer", "pyret-base/pyret-parser", "fs"], function(T, G, fs) {
  function toTime(hrtime) {
    const NS_PER_SEC = 1e9;
    return (hrtime[0] * NS_PER_SEC + hrtime[1]) / NS_PER_SEC;
  }
  const data = fs.readFileSync(process.argv[2], {encoding: "utf-8"});
  console.log(process.argv[2]);
  
  console.log(T);
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


});
