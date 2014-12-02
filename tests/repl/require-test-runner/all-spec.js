var r = require("requirejs");

r.config({
  waitSeconds: 15000,
  paths: {
    trove: "../../../build/phase1/trove",
    js: "../../../build/phase1/js",
    compiler: "../../../build/phase1/arr/compiler"
  }
});

console.log("Starting repl tests");

r([
    "../repl",
    "../locator-repl",
    ], function (
      repl,
      locRepl
    ) {
//  repl.performTest();
  locRepl.performTest();
}, function(err) {
  console.log("Require failed! ", err);
});
