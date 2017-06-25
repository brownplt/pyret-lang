//
// When building a web-standalone, browserify will parse this file
// and produce a version which include each dependency that is required()
//
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

fs = require("fs");
define("fs", [], function () { return fs; });

path = require("path");
define("path", [], function () { return path; });

http = require("http");
define("http", [], function () {return http;});

lockfile = require("lockfile");
define("lockfile", [], function () { return lockfile; });

websocket = require("websocket");
define("websocket", [], function () { return websocket });

