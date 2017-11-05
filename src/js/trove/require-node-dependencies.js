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

if (typeof document !== 'undefined') {
  // TODO(rachit): Hack for stopify benchmarking harness.
  fs = path = http = lockfile = websocket = {}

  // process sham for the browser
  process = {
    argv: [],
    exit(code) {
      console.log(`exit called with ${code}`)
    },
    stdout: {
      write(msg) {
        console.log(msg)
      }
    }
  }
}
else {
  fs = nodeRequire("fs");
  path = nodeRequire("path");
  http = nodeRequire("http");
  lockfile = nodeRequire("lockfile");
  websocket = nodeRequire("websocket");
}
define("fs", [], function () { return fs; });
define("path", [], function () { return path; });
define("http", [], function () {return http;});
define("lockfile", [], function () { return lockfile; });
define("websocket", [], function () { return websocket });

Stopify = require("Stopify")
define("Stopify", [], function () { return Stopify })

$__T = require("Stopify/built/src/rts")

$__T.makeRTS({transform: "lazy",
  estimator: "reservoir",
  env: "node",
  yieldInterval: 100})
