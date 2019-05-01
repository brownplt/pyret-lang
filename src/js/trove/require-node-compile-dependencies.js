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

// How WorkerFS works: https://github.com/jvilk/BrowserFS/issues/210
BrowserFS = require("browserfs");
BrowserFS.install({});

BrowserFS.configure({
    fs: "WorkerFS",
    // TODO(alex): Web Workers do not have access to LocalStorage
    // Source: https://stackoverflow.com/questions/6179159/accessing-localstorage-from-a-webworker 
    //fs: "LocalStorage"
    options: {
      worker: self,
    }
  }, function(e) {
    // Exception handler
    if (e) {
      throw e;
    }
  });

// Source: https://jvilk.com/browserfs/1.3.0/interfaces/browserfs.html#bfsrequire
fs = BrowserFS.BFSRequire("fs");
define("fs", [], function () { return fs; });

path = require("path");
define("path", [], function () { return path; });

define("http", [], function () {return {};});

define("ws", [], function () { return {} });

self.util = { format: function(object) { return object; } };

self.process = {
  argv: [],
  exit: function(exit_code) {
    console.error("The program tried to exit: ", exit_code); 
  },
  stdout: {
    write: function(str) { console.log(str); }
  },
  stderr: {
    write: function(str) { console.error(str); }
  },
  hrtime: function(time) {
    // TODO(alex): hrtime impl
    if (time == undefined) {
      return [0, 0];
    } else {
      return [1, 1];
    }
  }
};

