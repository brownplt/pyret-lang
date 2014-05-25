var r = require("requirejs");

r.config({
  waitSeconds: 15000,
  paths: {
    trove: "../../../build/phase1/trove",
    js: "../../../build/phase1/js",
    compiler: "../../../build/phase1/arr/compiler"
  }
});

r(["../parse"], function () { }, function(err) {
  console.log("Require failed! ", err);
});
