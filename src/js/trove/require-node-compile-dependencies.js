//
// When building a standalone, browserify will parse this file
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

fs = nodeRequire("fs");
define("fs", [], function () { return fs; });

path = nodeRequire("path");
define("path", [], function () { return path; });

http = nodeRequire("http");
define("http", [], function () {return http;});

ws = nodeRequire("ws");
define("ws", [], function () { return ws });

deasync = nodeRequire("deasync");
define("deasync", [], function () { return deasync });

request = nodeRequire("request");
define("request", [], function () { return request });
