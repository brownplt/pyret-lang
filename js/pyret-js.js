var PYRET = require('./runtime.js').pyret
var LIB = require('./htdocs/moorings.js').lib;
var Namespace = require('./htdocs/namespace.js').Namespace;
var fs = require('fs');
var http = require('http');
var $ = require('jquery');

var compile = require('./jslib/compile.js').lib;

var DEBUG = false;
function debug() {
  if (DEBUG) {
    console.log.apply(console.log, Array.prototype.slice.apply(arguments));
  }
}

var base = "http://localhost:8080";
var runtime = PYRET.makeRuntime();
debug(runtime.namespace.__proto__);
var extraIds = ["prim-read-sexpr", "equiv", "data-to-repr", "data-equals", "test-print"];
var mooringsResult = LIB(runtime.runtime, runtime.namespace);
var ids = mooringsResult.namespace.getNames();

var builtinsContext = {
  compilerURL: base + "/compile",
  debug: debug,
  mooringsNamespace: mooringsResult.namespace
};

var builtins = [compile];

builtins.forEach(function(b) {
  runtime.builtinModules[b.name] = b.lib(builtinsContext, runtime);
});

debug(mooringsResult);
fs.readFile(process.argv[2], {encoding: "utf8"}, function(err, src) {
  debug("File is: ", src);
  debug("Err is: ", err);
  debug(ids);
  var compiled = $.ajax(base + "/compile", {
        type: "POST",
        datatype: "json",
        data: {
            src: src,
            options: JSON.stringify({ids: extraIds.concat(ids), check: true})
        }
      });
  debug("request sent");
  debug(compiled);
  compiled.error(function(err) {
    debug("Failed: ", err);
  });
  var runResult = compiled.then(function(jsCode) {
      debug("Got code: ", jsCode);
      jsCode = JSON.parse(jsCode);
      var builtinsToLoad = jsCode["imports"];
      debug("Builtins: ", builtinsToLoad);
      var baseNamespace = runtime.namespace.merge(mooringsResult.namespace);
      builtinsToLoad.forEach(function(b) {
        baseNamespace = baseNamespace.set(b["imported-as"], runtime.builtinModules[b["module-name"]]);
      });
      return (1,eval)(jsCode["js-src"])(runtime.runtime, baseNamespace);
    });
  runResult.then(function(r) {
      debug(r);
      runtime.runtime.applyFunc(runtime.runtime.getField(r.val, "format"), []);
    });
});

