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
