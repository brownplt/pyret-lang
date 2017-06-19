({
  requires: [],
  nativeRequires: ["path", "fs"],
  provides: {},
  theModule: function(runtime, namespace, uri, path, fs, requirejs) {
    var NODE_BUNDLED_DEPS_FILE = "build/bundled-node-deps.js";
    var NODE_REQUIRE_DEPS_FILE = "src/js/trove/require-node-dependencies.js";
    var AMD_LOADER = "src/js/base/amd_loader.js";

    /*

      standaloneStr: A single string containing all the JS-compiled modules,
      dependency map, and load order.  The syntax is a JS object.

      deps: A Pyret list of strings of the requirejs dependencies of the program

      configJSON: A JSON string to parse and use as a configuration option to
      requirejs

      standaloneFile: File template for the standalone (usually src/js/base/handalone.js)

      bundleDependencies: whether or not to include node js dependencies in the standalone

      returns: The string produced by resolving dependencies with requirejs

    */
    function makeStandalone(deps, body, configJSON, standaloneFile, bundleDependencies) {
      runtime.checkArity(5, arguments, ["make-standalone"]);
      runtime.checkList(deps);
      runtime.checkString(configJSON);
      var READ_OPTIONS = {encoding: 'utf8'};

      // TODO(joe): make sure this gets embedded correctly in the built version; can't
      // necessarily rely on this path
      console.log(process.cwd());
      var config = JSON.parse(configJSON);
      var storeDir = config["baseUrl"];
      var handalone = fs.readFileSync(standaloneFile, READ_OPTIONS);
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
      if(!config["use-raw-files"]) {
        throw new Error("Cannot not use raw-files! RequireJS is gone");
      }
      var outFile = fs.openSync(realOut, "w");

      // Write the amd loader first
      var loaderContents = fs.readFileSync(AMD_LOADER, READ_OPTIONS);
      fs.writeSync(outFile, loaderContents);

      // Now either write the file containing all dependencies or the file which
      // just defines() the dependencies.

      var depsFile;
      if (runtime.isPyretTrue(bundleDependencies)) {
        depsFile = NODE_BUNDLED_DEPS_FILE;
      } else {
        depsFile = NODE_REQUIRE_DEPS_FILE;
      }
      var dependencyCode = fs.readFileSync(depsFile, READ_OPTIONS);
      fs.writeSync(outFile, dependencyCode);

      var filesToFetch = config["raw-js"];
      //fs.writeSync(outFile, "if(typeof window === 'undefined') {\n");
      //fs.writeSync(outFile, "var requirejs = require(\"requirejs\");\n");
      //fs.writeSync(outFile, "var define = requirejs.define;\n}\n");
      Object.keys(filesToFetch).forEach(function(f) {
        var contents = fs.readFileSync(filesToFetch[f], {encoding: 'utf8'});
        fs.writeSync(outFile, contents);
      });
      fs.writeSync(outFile, "define(\"program\", " + depsLine + ", function() {\nreturn ");
      var writeRealOut = function(str) {
        fs.writeSync(outFile, str, {encoding: 'utf8'});
        return runtime.nothing;
      };
      return runtime.safeCall(function() {
        return runtime.getField(body, "print-ugly-source").app(runtime.makeFunction(writeRealOut, "write-real-out"));
      }, function(_) {
        fs.writeSync(outFile, "\n});\n");
        fs.writeSync(outFile, handalone);
        fs.fsyncSync(outFile);
        fs.closeSync(outFile);
        return true;
      });
    }
    return runtime.makeModuleReturn({
      "make-standalone": runtime.makeFunction(makeStandalone, "make-standalone")
    }, {})
  }
})
