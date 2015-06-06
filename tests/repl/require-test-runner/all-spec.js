var r = require("requirejs");

var build = process.env["PHASE"] || "build/phase1";

r.config({
  waitSeconds: 15000,
  paths: {
    trove: "../../../" + build + "/trove",
    js: "../../../" + build + "/js",
    compiler: "../../../" + build + "/arr/compiler"
  }
});

console.log("Starting repl tests");

r([
    "../repl",
    ], function (
      repl
    ) {
  repl.performTest();
}, function(err) {
  console.log("Require failed! ", err);
});
