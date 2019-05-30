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
        var tree = moduleDeps();
        
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
