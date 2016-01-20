var r = require("requirejs");

var build = process.env.PHASE || "build/phase1";

var pyretLang = "../../../../";

r.config({
  waitSeconds: 15000,
  paths: {
    trove: pyretLang + build + "/trove",
    js: pyretLang + build + "/js",
    compiler: pyretLang + build + "/arr/compiler"
  }
});

console.log("Starting benchmark tests");

r([
    "../benchmark",
    ], function (
      benchmark
    ) {
  benchmark.performTest();
}, function(err) {
  console.log("Require failed! ", err);
});
