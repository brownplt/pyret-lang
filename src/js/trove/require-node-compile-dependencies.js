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

// NOTE(alex): Configure is async. Placeholder object so fs module will be defined
self.fsPlaceholder = {};

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
    // NOTE(alex): configure() is async

    // Source: https://jvilk.com/browserfs/1.3.0/interfaces/browserfs.html#bfsrequire
    // self.fs = BrowserFS.BFSRequire("fs");
    BrowserFS.FileSystem.WorkerFS.Create({worker: self}, function(error, fs) {
      self.fsPlaceholder = fs;
      console.log("RESULT:", self.fsPlaceholder);
      console.log("create result:", fs.mkdir("/", function(err, content) { 
        console.log("create:", err, ",", content);
      } ));
      console.log("exists result:", fs.exists("/", function(err, content) { 
        console.log("exists:", err, ",", content);
      } ));

      console.log("Done setting up");
      if (e) {
        console.error(e);
      }
      
      });
    
  });

define("fs", [], function () { return fsPlaceholder; });
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

