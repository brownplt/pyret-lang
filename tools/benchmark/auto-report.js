//run from ./tools/benchmarks
var r = require("requirejs");

r.config({
  paths: {
    trove: "../../build/phase1/trove",
    js: "../../build/phase1/js",
    compiler: "../../build/phase1/arr/compiler"
  }
});

r(["auto-report-code"], function (p) { });
