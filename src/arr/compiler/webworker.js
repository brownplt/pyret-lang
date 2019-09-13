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
        // Data pre-processing
        console.log(e.data);
        if(!e.data || !e.data._parley) {
          return;
        }
        var parsed = e.data;
        var options = JSON.stringify(parsed.options);
        // TODO(alex): May need to due complex message handling here
        
        RUNTIME.runThunk(function() {
          return onCompile.app(options, respondForPy);
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
