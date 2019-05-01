({
  requires: [],
  provides: {
    values: {
      "open-input-file": "tany",
      "open-output-file": "tany",
      "read-file": "tany",
      "display": "tany",
      "flush-output-file": "tany",
      "file-times": "tany",
      "real-path": "tany",
      "exists": "tany",
      "close-output-file": "tany",
      "close-input-file": "tany",
      "create-dir": "tany",
      "list-files": "tany"
    }
  },
  nativeRequires: [],
  theModule: function(RUNTIME, NAMESPACE, uri) {
    function respond(jsonData) {
      postMessage(jsonData);
      return RUNTIME.nothing;
    }
    function respondJSON(json) { return respond(JSON.stringify(json)); }
    const respondForPy = RUNTIME.makeFunction(respond, "respond");

    function setupHandlers(onCompile) {
      onmessage = function(e) {
        // TODO(alex): May need to due complex message handling here
        onCompile.app(e.data, respondForPy);
      };
    }

    return RUNTIME.makeModuleReturn({
      "setupHandlers": RUNTIME.makeFunction(setupHandlers, "setupHandlers")
    }, {});
  }
})
