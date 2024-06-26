//
// When building a standalone, browserify will parse this file
// and produce a version which include each dependency that is required()
//
const process = require('process');
process.versions = { node: "20.11.0" };

stream = require("stream");
define("stream", [], function() {return stream;});

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

escodegen = require("escodegen");
define("escodegen", [], function () { return escodegen; });


// NOTE(alex): Configure is async. Placeholder object so fs module will be defined
self.fsPlaceholder = {};

function polyfillCreateReadStream(fs) {
  function createReadStream(path) {
    let hasRead = false;
    let s = new stream.Readable({
      read: function(n) {
        if(!hasRead) {
          hasRead = true;
          fs.readFile(path, function(err, contents) {
            if(err) { process.nextTick(s.emit("error", err)); }
            else { s.push(contents); }
          });
        }
        else {
          s.push(null);
        }
      },
      destroy: function() { /* intentional no-op */ }
    });
    return s;
  }
  fs.createReadStream = createReadStream;
}

// How WorkerFS works: https://github.com/jvilk/BrowserFS/issues/210
BrowserFS = require("browserfs");
BrowserFS.install({});

const {promise, resolve, reject} = Promise.withResolvers();


console.log("Configuring browserfs");
try {
  BrowserFS.configure({
    fs: "WorkerFS",
    // Web Workers do not have access to LocalStorage.
    // Source: https://stackoverflow.com/questions/6179159/accessing-localstorage-from-a-webworker 
    // Must use BrowserFS to talk to the main thread and gain access to LocalStorage
    options: {
      worker: self,
    }
  }, function(e) {
    console.log("Configuring browserfs error arg was ", e);

    // NOTE(alex): configure() is async

    if(e) { console.error("Browserfs config failed with" , e); reject(e); }

    // Source: https://jvilk.com/browserfs/1.3.0/interfaces/browserfs.html#bfsrequire
    self.fs = BrowserFS.BFSRequire("fs");
    fs.createReadStream = function() {

    };
    let requiredfs = require("fs");
    Object.assign(requiredfs, self.fs);
    Object.assign(requiredfs, self.fs.__proto__);
    Object.assign(self.fsPlaceholder, self.fs);
    Object.assign(self.fsPlaceholder, self.fs.__proto__);
    polyfillCreateReadStream(requiredfs);
    polyfillCreateReadStream(self.fsPlaceholder);
    console.log("Loaded browserfs and triggering GLOBAL_DEPS_READY");
    resolve('GLOBAL_DEPS_READY');
  });
}
catch(e) {
  console.error("BrowserFS threw an error: ", e);
}
console.log("After call to configure browserfs");


self.GLOBAL_DEPS_READY = promise;

define("fs", [], function () {
  return fsPlaceholder;
});
path = require("path");
define("path", [], function () { return path; });

define("http", [], function () {return {};});

define("ws", [], function () { return {} });

// Needed by module-deps
self.Buffer = require('buffer').Buffer;
moduleDeps = require("module-deps");
define("module-deps", [], function() {return moduleDeps;});

JSONStream = require("JSONStream");
define("JSONStream", [], function() {return JSONStream;});


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
  },
  versions: { node: "20.11.0" }
};
