({
  requires: [],
  nativeRequires: ["path", "fs", "requirejs"],
  provides: {},
  theModule: function(runtime, namespace, uri, path, fs, requirejs) {
    /*

      standaloneStr: A single string containing all the JS-compiled modules,
      dependency map, and load order.  The syntax is a JS object.

      deps: A Pyret list of strings of the requirejs dependencies of the program

      storeDir: A path to a directory to use for storing intermediate files

      configPath: The path to a file containing configuration options in JSON
      according to the requireJS spec (as a string)

      returns: The string produced by resolving dependencies with requirejs

    */
    function makeStandalone(deps, body, storeDir, configPath) {
      runtime.checkArity(4, arguments, ["make-standalone"]);
      runtime.checkList(deps);
      runtime.checkString(body);
      runtime.checkString(storeDir);
      runtime.checkString(configPath);

      // TODO(joe): make sure this gets embedded correctly in the built version; can't
      // necessarily rely on this path
      console.log(process.cwd());
      var handalone = fs.readFileSync("src/js/handalone.js");
      var depsArr = runtime.ffi.toArray(deps);
      depsArr.push("js/runtime");
      var depsStrs = depsArr.map(function(d) { return '"' + d + '"'; });
      var depsLine = "[" + depsStrs.join(",") + "]";

      var programRequires = "requirejs(" + depsLine + ")"
      fs.writeFileSync(path.join(storeDir, "program-require"));

      var config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
      if(!("out" in config)) {
        runtime.ffi.throwMessageException("make-standalone config must have an 'out' field");
      }
      var realOut = config.out;
      config.out = config.out + ".deps";
      runtime.pauseStack(function(restarter) {
        requirejs.optimize(config, function(result) {
          var programWithDeps = fs.readFileSync(config.out, 'utf8');
          var fullProgram = "requirejs = require(\"requirejs\");\ndefine = requirejs.define;\n"
          fullProgram += programWithDeps;
          fullProgram += "define(\"program\", " + depsLine + ", function() {\nreturn " +
            body +
          "\n});\n";
          fullProgram += handalone;
          fs.writeFileSync(realOut, fullProgram);
          restarter.resume(true);
        }, function(err) {
          console.error("Error while using requirejs optimizer: ", err); 
          restarter.error(runtime.ffi.makeMessageException("Error while using requirejs optimizer: ", String(err)));
        });
      });
    }
    return runtime.makeModuleReturn({
      "make-standalone": runtime.makeFunction(makeStandalone)
    }, {})
  }
})
