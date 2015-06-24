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

r(["../parse"], function () { }, function(err) {
  console.log("Require failed! ", err);
});
