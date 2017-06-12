sexpr = require("s-expression");
define("s-expression", [], function() {return sexpr;});

q = require("q");
define("q", [], function() {return q;});

seedrandom = require("seedrandom");
define("seedrandom", [], function() {return seedrandom;});

sourcemap = require("source-map");
define("source-map", [], function () { return sourcemap; });

jssha256 = require("js-sha256");
define("js-sha256", [], function () { return jssha256; });

// This one is different since we need to ask for browserify's copy of fs
fs = require("fs");
define("fs", [], function () { return fs; });

path = require("path");
define("path", [], function () { return path; });
