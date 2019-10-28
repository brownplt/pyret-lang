var r = require("requirejs");

var build = process.env["PHASE"] || "build/phaseA";

r.config({
  waitSeconds: 15000,
  paths: {
    "trove": "../../../" + build + "/trove",
    "js": "../../../" + build + "/js",
    "compiler": "../../../" + build + "/arr/compiler",
    "jglr": "../../../lib/jglr",
    "pyret-base": "../../../" + build
  }
});

r(["../parse"], function () { }, function(err) {
  console.log("Require failed! ", err);
});
