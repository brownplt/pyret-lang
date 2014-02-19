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
    "./tests/evaluator/conditionals",
    "./tests/evaluator/data",
    "./tests/evaluator/errors",
    "./tests/evaluator/well-formed",
    ], function (
      constants,
      conditionals,
      data,
      errors,
      well_formed
    ) {
  constants.performTest();
  conditionals.performTest();
  data.performTest();
  errors.performTest();
  well_formed.performTest();
}, function(err) {
  console.log("Require failed! ", err);

});
