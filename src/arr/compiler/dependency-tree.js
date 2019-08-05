({
  requires: [],
  provides: {
    values: {
      "get-dependencies": "tany"
    }
  },
  nativeRequires: ["module-deps", "JSONStream", "path"],
  theModule: function(runtime, _, _, moduleDeps, JSONStream, path) {
    function getDeps(path) {
      return runtime.pauseStack(function(restarter) {

        /*
         * Ignore requires on node modules (only available through builtins).
         * Delay resolution to runtime (where it may throw a runtime error).
         *
         * Needed so that required node modules are ignored in the browser when
         *  compiling a user's program.
         * Required node modules are provided by the webpage at runtime.
         */
        // TODO(alex): Make this a flag or something
        let nativeRequiresToIgnore = ["assert", "immutable"];

        let opts = {
          filter: function(id) {
            // Return true to include as a dependency
            // Return false to exclude
            return !nativeRequiresToIgnore.includes(id);
          }
        };

        var tree = moduleDeps(opts);
        
        var dataCollector = tree.pipe(JSONStream.stringify());

        // https://stackoverflow.com/questions/10623798/read-contents-of-node-js-stream-into-a-string-variable
        const chunks = [];

        dataCollector.on("data", function(chunk) {
          chunks.push(Buffer.from(chunk, 'utf8'));
        });
        dataCollector.on("end", function () {
          var result = JSON.parse(String(Buffer.concat(chunks)));
          var paths = result.map(x => x["file"]);
          restarter.resume(paths);
        });
        tree.end({ file: path });

      });
    }
    return runtime.makeModuleReturn({ "get-dependencies": runtime.makeFunction(getDeps) }, {});
  }
})
