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

fs = nodeRequire("fs");
define("fs", [], function () { return fs; });

path = nodeRequire("path");
define("path", [], function () { return path; });

http = nodeRequire("http");
define("http", [], function () {return http;});

lockfile = nodeRequire("lockfile");
define("lockfile", [], function () { return lockfile; });

websocket = nodeRequire("websocket");
define("websocket", [], function () { return websocket });

process = nodeRequire("process");
define("process", [], function () { return process });

stopify = require("stopify/dist/src/stopify/compileFunction")
define("stopify", [], function () { return stopify })

const defaultOpts = {
  filename: "",
  estimator: "reservoir",
  yieldInterval: 100,
  resampleInterval: 100,
  timePerElapsed: 1,
  stop: undefined,
  variance: false,
  env: "node",
  stackSize: 1000
}

$__T = require("stopify-continuations/dist/src/runtime/runtime")
$__R = $__T.newRTS("lazyDeep")
$S = require("stopify/dist/src/runtime/node").init($__R, defaultOpts);

