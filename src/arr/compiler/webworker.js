({
  requires: [],
  provides: {
    values: {
      "setupHandlers": "tany"
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
        if (!e.data.request) {
          return;
        }

        var message = JSON.stringify(e.data);

        RUNTIME.runThunk(function() {
          return onCompile.app(message, respondForPy);
        }, function(result) {
          if(RUNTIME.isFailureResult(result)) {
            console.error("Error from compile:", result);
          }
          else {
            console.log("Success:", result);
          }
        });
      };
    }

    return RUNTIME.makeModuleReturn({
      "setupHandlers": RUNTIME.makeFunction(setupHandlers, "setupHandlers")
    }, {});
  }
})
