//
// When building a standalone, browserify will parse this file
// and produce a version which include each dependency that is required()
//
sexpr = require("s-expression");
define("s-expression", [], function() {return sexpr;});

q = require("q");
define("q", [], function() {return q;});

jsmd5 = require("js-md5");
define("js-md5", [], function() {return jsmd5;});

canvas = require("canvas");
define("canvas", [], function() {return canvas;});

seedrandom = require("seedrandom");
define("seedrandom", [], function() {return seedrandom;});

csv = require("fast-csv");
define("fast-csv", [], function() {return csv;});

crossFetch = require("cross-fetch");
define("cross-fetch", [], function() {return crossFetch;});

sourcemap = require("source-map");
define("source-map", [], function () { return sourcemap; });

jssha256 = require("js-sha256");
define("js-sha256", [], function () { return jssha256; });

fs = nodeRequire("fs");
define("fs", [], function () { return fs; });

readline = nodeRequire("readline");
define("readline", [], function () { return readline; });

path = nodeRequire("path");
define("path", [], function () { return path; });

http = nodeRequire("http");
define("http", [], function () {return http;});

vegaMin = nodeRequire(nodeRequire('node:path').dirname(nodeRequire.resolve('vega')) + '/vega.js');
define("vegaMin", [], function () {return global.vega;});
