r = require("requirejs");

r.config({
  paths: {
    trove: "./trove",
    js: "./js",
    compiler: "./arr/compiler"
  }
});

r(["pyret-start"], function(p) { });
