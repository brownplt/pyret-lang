var r = require("requirejs");

r.config({
  waitSeconds: 15000,
  baseUrl: "../../build/phase1"
});

console.log("Starting repl tests");

r([
    "../../tests/repl/repl",
    ], function (
      repl
    ) {
  repl.performTest();
}, function(err) {
  console.log("Require failed! ", err);
});
