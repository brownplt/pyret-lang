const R = require("requirejs");

R.config({
  paths: {
    'jglr': "../../lib/jglr/",
    'pyret-base': "../../build/phaseA"
  }
});
R(["pyret-base/js/pyret-tokenizer", "pyret-base/js/pyret-parser", "fs"], function(T, G, fs) {
  const data = fs.readFileSync(process.argv[2], {encoding: "utf-8"});
  const toks = T.Tokenizer;
  toks.tokenizeFrom(data);
  while (toks.hasNext()) {
    var tok = toks.next();
    console.log(tok.toString(true), tok.pos.toString(true));
  }
});
