var r = require("requirejs");

r.config({
  waitSeconds: 15000,
  baseUrl: "../..",
  paths: {
    trove: "build/phase2/trove",
    js: "build/phase2/js"
  }
});

console.log("Starting all-spec");

//r(["trove/string-dict"], function(sd) {});
//r(["build/phase2/js/runtime-anf"], function(sd) {});

r([
    "./tests/evaluator/constants",
    "./tests/evaluator/conditionals"
    ], function (
      constants,
      conditionals
    ) {
  constants.performTest();
  conditionals.performTest();
}, function(err) {
  console.log("Require failed! ", err);

});
