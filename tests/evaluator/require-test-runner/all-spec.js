var r = require("requirejs");

r.config({
  waitSeconds: 15000,
  paths: {
    trove: "../../../build/phase1/trove",
    js: "../../../build/phase1/js",
    compiler: "../../../build/phase1/arr/compiler"
  }
});

console.log("Starting evaluator tests");

r([
    "../constants",
    "../conditionals",
    "../check-tests",
    "../data",
    "../errors",
    "../well-formed",
    "../stack",
    "../managed-execution"
    ], function (
      constants,
      conditionals,
      check_tests,
      data,
      errors,
      well_formed,
      stack,
      managed
    ) {
  constants.performTest();
  conditionals.performTest();
  check_tests.performTest();
  data.performTest();
  errors.performTest();
  well_formed.performTest();
//  stack.performTest();
  managed.performTest();
}, function(err) {
  console.log("Require failed! ", err);

});
