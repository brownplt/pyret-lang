({
  requires: [],
  nativeRequires: ["path", "fs", "requirejs"],
  provides: {
    values: {
      "make-standalone": "tany"
    },
    aliases: {},
    datatypes: {}
  },
  theModule: function(runtime, namespace, uri, path, fs, requirejs) {
    /*

      standaloneStr: A single string containing all the JS-compiled modules,
      dependency map, and load order.  The syntax is a JS object.

      deps: A Pyret list of strings of the requirejs dependencies of the program

      configJSON: A JSON string to parse and use as a configuration option to
      requirejs

      returns: The string produced by resolving dependencies with requirejs

    */
    function makeStandalone(deps, body, configJSON, standaloneFile) {
      runtime.checkArity(4, arguments, ["make-standalone"]);
      runtime.checkList(deps);
      runtime.checkString(configJSON);

      // TODO(joe): make sure this gets embedded correctly in the built version; can't
      // necessarily rely on this path
      console.log(process.cwd());
      var config = JSON.parse(configJSON);
      var storeDir = config["baseUrl"];
      var handalone = fs.readFileSync(standaloneFile, {encoding: 'utf8'});
      var depsArr = runtime.ffi.toArray(deps);
      depsArr.push("pyret-base/js/runtime");
      var depsStrs = depsArr.map(function(d) { return '"' + d + '"'; });
      var depsLine = "[" + depsStrs.join(",") + "]";

      var programRequires = "requirejs(" + depsLine + ")"
      fs.writeFileSync(path.join(storeDir, "program-require.js"), programRequires);

      if(!("out" in config)) {
        runtime.ffi.throwMessageException("make-standalone config must have an 'out' field");
      }
      var realOut = config.out;
      config.out = path.join(storeDir, "program-deps.js");
      config.name = "program-require";
      runtime.pauseStack(function(restarter) {
        requirejs.optimize(config, function(result) {
          var programWithDeps = fs.readFileSync(config.out, {encoding: 'utf8'});
          // Browser/node check based on window below
          fs.open(realOut, "w", function(err, outFile) {
            if (err) throw err;
            fs.writeSync(outFile, "if(typeof window === 'undefined') {\n");
            fs.writeSync(outFile, "var requirejs = require(\"requirejs\");\n");
            fs.writeSync(outFile, "var define = requirejs.define;\n}\n");
            fs.writeSync(outFile, programWithDeps);
            fs.writeSync(outFile, "define(\"program\", " + depsLine + ", function() {\nreturn ");
            var writeRealOut = function(str) { 
              fs.writeSync(outFile, str, {encoding: 'utf8'}); 
              return runtime.nothing; 
            };
            runtime.runThunk(function() { 
              return runtime.getField(body, "print-ugly-source").app(runtime.makeFunction(writeRealOut, "write-real-out"));
            }, function(_) {
              fs.writeSync(outFile, "\n});\n");
              fs.writeSync(outFile, handalone);
              fs.fsyncSync(outFile);
              fs.closeSync(outFile);
              restarter.resume(true);
            });
          });
        }, function(err) {
          console.error("Error while using requirejs optimizer: ", err); 
          restarter.error(runtime.ffi.makeMessageException("Error while using requirejs optimizer: ", String(err)));
        });
      });
    }
    return runtime.makeModuleReturn({
      "make-standalone": runtime.makeFunction(makeStandalone, "make-standalone")
    }, {}, {})
  }
})
