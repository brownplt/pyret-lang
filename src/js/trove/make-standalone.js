({
  requires: [],
  nativeRequires: ["path", "fs"],
  provides: {
    values: {
      "make-standalone": "tany",
      "make-html-file": "tany"
    }
  },
  theModule: function(runtime, namespace, uri, path, fs) {

    var READ_OPTIONS = {encoding: 'utf8'};

    function makeHtmlFile(bundledJSFile, outfile) {
      runtime.checkArity(2, arguments, ["make-html-file"]);
      runtime.checkString(bundledJSFile);
      runtime.checkString(outfile);

      var template = fs.readFileSync(HTML_TEMPLATE, READ_OPTIONS);

      var relativePath = path.relative(path.dirname(outfile), bundledJSFile);
      var filtered = template.replace("{{{compiled-jarr-file}}}", relativePath);


      fs.writeFileSync(outfile, filtered);
      return true;
    }

    /*

      standaloneStr: A single string containing all the JS-compiled modules,
      dependency map, and load order.  The syntax is a JS object.

      deps: A Pyret list of strings of the requirejs dependencies of the program

      configJSON: A JSON string to parse and use as a configuration option to
      requirejs

      standaloneFile: File template for the standalone (usually src/js/base/handalone.js)

      depsFile: File that contains the builtin npm/node dependencies, either as uses of "require" (i.e.
                dynamically linked) or as the output of `browserify` (i.e. statically linked)
                NOTE: This path is specified relative to the directory you are building from.

      returns: The string produced by resolving dependencies with requirejs

    */
    function makeStandalone(deps, body, configJSON, options) {
      runtime.checkArity(4, arguments, ["make-standalone"], false);
      runtime.checkList(deps);
      runtime.checkPyretVal(body);
      runtime.checkString(configJSON);
      var standaloneFile = runtime.getField(options, "standalone-file");
      var depsFile = runtime.getField( options, "deps-file");
      var thisPyretDir = runtime.getField( options, "this-pyret-dir" );
      var baseDir = runtime.getField( options, "base-dir" );

      var AMD_LOADER = path.join(thisPyretDir, "js/amd_loader.js");

      // TODO(joe): figure our where web-standalone-template should go
      var HTML_TEMPLATE = "src/scripts/web-standalone-template.html";

      // TODO(joe): make sure this gets embedded correctly in the built version; can't
      // necessarily rely on this path
      var config = JSON.parse(configJSON);
      var storeDir = config["baseUrl"];
      var handalone = fs.readFileSync(standaloneFile, READ_OPTIONS);
      var depsArr = runtime.ffi.toArray(deps);
      depsArr.push("pyret-base/js/runtime");
      var depsStrs = depsArr.map(function(d) { return '"' + d + '"'; });
      var depsLine = "[" + depsStrs.join(",") + "]";

      if(!("out" in config)) {
        runtime.ffi.throwMessageException("make-standalone config must have an 'out' field");
      }
      var realOut = config.out;
      if(!config["use-raw-files"]) {
        throw new Error("Cannot not use raw-files! RequireJS is gone");
      }
      var outFile = fs.openSync(realOut, "w");

      // Write the amd loader first
      var loaderContents = fs.readFileSync(AMD_LOADER, READ_OPTIONS);
      fs.writeSync(outFile, loaderContents);

      // Now either write the file containing all dependencies or the file which
      // just defines() the dependencies.

      var dependencyCode = fs.readFileSync(depsFile, READ_OPTIONS);
      fs.writeSync(outFile, dependencyCode);

      var filesToFetch = config["raw-js"];
      //fs.writeSync(outFile, "if(typeof window === 'undefined') {\n");
      //fs.writeSync(outFile, "var requirejs = require(\"requirejs\");\n");
      //fs.writeSync(outFile, "var define = requirejs.define;\n}\n");
      Object.keys(filesToFetch).forEach(function(f) {
        if (filesToFetch[f].indexOf( "$PYRET" ) !== -1) {
          var filename = filesToFetch[f].replace("$PYRET", thisPyretDir);
        } else if (!path.isAbsolute( filesToFetch[f] )) {
          var filename = path.resolve( path.join( baseDir, filesToFetch[f] ));
        } else {
          var filename = filesToFetch[f];
        }

        var contents = fs.readFileSync(filename, {encoding: 'utf8'});
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
      }, "makeStandalone:print-ugly-source");
    }
    return runtime.makeModuleReturn({
      "make-standalone": runtime.makeFunction(makeStandalone, "make-standalone"),
      "make-html-file": runtime.makeFunction(makeHtmlFile, "make-html-file")
    }, {})
  }
})
