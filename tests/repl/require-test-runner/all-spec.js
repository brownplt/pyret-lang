var r = require("requirejs");

r.config({
  waitSeconds: 15000,
  baseUrl: "../..",
  paths: {
    trove: "build/phase1/trove",
    js: "build/phase1/js",
    compiler: "build/phase1/arr/compiler"
  }
});

console.log("Starting repl tests");

r([
    "./tests/repl/repl",
    ], function (
      repl
    ) {
  repl.performTest();
}, function(err) {
  console.log("Require failed! ", err);
});
