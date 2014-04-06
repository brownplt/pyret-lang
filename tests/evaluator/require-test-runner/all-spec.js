var r = require("requirejs");

r.config({
  waitSeconds: 15000,
  paths: {
    trove: "../../../build/phase1/trove",
    js: "../../../build/phase1/js",
    compiler: "../../../build/phase1/arr/compiler"
  }
});

console.log("Starting all-spec");

r([
    "../constants",
    "../conditionals",
    "../data",
    "../errors",
    "../well-formed",
    "../check",
    "../stack"
    ], function (
      constants,
      conditionals,
      data,
      errors,
      well_formed,
      stack
    ) {
  constants.performTest();
  conditionals.performTest();
  data.performTest();
  errors.performTest();
  well_formed.performTest();
//  stack.performTest();
}, function(err) {
  console.log("Require failed! ", err);

});
