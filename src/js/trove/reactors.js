({
  requires: [
    { "import-type": "builtin", "name": "reactor-events" },
    { "import-type": "builtin", "name": "valueskeleton" }
  ],
  nativeRequires: [],
  provides: {
    shorthands: {
      "RofA": ["tyapp", ["local", "Reactor"], [["tid", "a"]]],
      "ReactorEvent": { tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://reactor-events" },
                 name: "Event" },
      "Image": { tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://image" },
                 name: "Image" },
      "ValueSkeleton": { tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://valueskeleton" },
                 name: "ValueSkeleton" }
    },
    values: {
      "keypress": ["arrow", ["String"], ["local", "Event"]],
      "mouse": ["arrow", ["Number", "Number", "String"], ["local", "Event"]],
      "tick": ["local", "Event"]
    },
    aliases: {
      "Event": "ReactorEvent"
    },
    datatypes: {
      "Reactor": ["data", "Reactor", ["a"], [], {
        "get-value": ["arrow", [], ["tid", "a"]],
        "draw": ["arrow", [], "Image"],
        "interact": ["arrow", [], "RofA"],
        "start-trace": ["arrow", [], "RofA"],
        "stop-trace": ["arrow", [], "RofA"],
        "get-trace": ["arrow", [], ["List", ["tid", "a"]]],
        "react": ["arrow", [["local", "Event"]], "RofA"],
        "output": ["arrow", [], "ValueSkeleton"]
      }]
    },
  },
  theModule: function(runtime, _, uri, reactorEvents, VSlib) {
    var gmf = function(m, f) { return runtime.getField(runtime.getField(m, "values"), f); }
    var gtf = function(m, f) { return runtime.getField(m, "types")[f]; }
    var VS = runtime.getField(VSlib, "values");

    var isEvent = gmf(reactorEvents, "is-Event");
    var interact = null;
    var setInteract = function(newInteract) {
      interact = newInteract;
    }
    var makeReactor = function(init, fields) {
      runtime.ffi.checkArity(2, arguments, "reactor");
      runtime.checkObject(fields);
      return makeReactorRaw(init, fields.dict, false, []);
    }
    var makeReactorRaw = function(init, handlers, tracing, trace) {
      return runtime.makeObject({
        "get-value": runtime.makeMethod0(function(self) {
          return init;
        }),
        "draw": runtime.makeMethod0(function(self) {
          if(!handlers.hasOwnProperty("to-draw")) {
            runtime.ffi.throwMessageException("Cannot draw() because no to-draw was specified on this reactor.");
          }
          var drawer = handlers["to-draw"];
          return drawer.app(init);
        }),
        interact: runtime.makeMethod0(function(self) {
          if(interact === null) {
            runtime.ffi.throwMessageException("No interaction set up for this context (please report a bug if you are using code.pyret.org and see this message)");
          }
          var thisInteractTrace = [];
          var tracer = null;
          if(tracing) {
            tracer = function(newVal, oldVal, k) {
              thisInteractTrace.push(newVal);
              k();
            };
          }
          runtime.safeCall(function() {
            return interact(init, handlers, tracer);
          }, function(newVal) {
            return makeReactorRaw(newVal, handlers, tracing, trace.concat(thisInteractTrace));
          });
        }),
        "start-trace": runtime.makeMethod0(function(self) {
          return makeReactorRaw(init, handlers, true, [init]);
        }),
        "stop-trace": runtime.makeMethod0(function(self) {
          return makeReactorRaw(init, handlers, false, []);
        }),
        "get-trace": runtime.makeMethod0(function(self) {
          if(tracing) {
            return runtime.ffi.makeList(trace);
          }
          else {
            runtime.ffi.throwMessageException("Tried to get trace of a reactor that isn't tracing; try calling get-trace() first");
          }
        }),
        react: runtime.makeMethod1(function(self, event) {
          function callOrError(handlerName, args) {
            if(handlers.hasOwnProperty(handlerName)) {
              var funObj = handlers[handlerName].app;
              return runtime.safeCall(function() {
                return funObj.apply(funObj, args);
              }, function(newVal) {
                if(tracing) {
                  var newTrace = trace.concat([newVal]);
                }
                else {
                  var newTrace = trace;
                }
                return makeReactorRaw(newVal, handlers, tracing, newTrace);
              });
            }
            else {
              runtime.ffi.throwMessageException("No " + handlerName + " handler defined");
            }
          }
          return runtime.ffi.cases(isEvent, "Event", event, {
            keypress: function(key) {
              return callOrError("on-key", [init, key]);
            },
            tick: function() {
              return callOrError("on-tick", [init]);
            },
            mouse: function(x, y, kind) {
              return callOrError("on-mouse", [init, x, y, kind]);
            }
          });
        }),
        _output: runtime.makeMethod0(function(self) {
          if(tracing) {
            var traceVal = runtime.ffi.makeSome(runtime.ffi.makeList(trace));
          }
          else {
            var traceVal = runtime.ffi.makeNone();
          }
          return runtime.getField(VS, "vs-constr").app(
            "reactor",
            runtime.ffi.makeList([
              runtime.getField(VS, "vs-value").app(init),
              runtime.getField(VS, "vs-value").app(traceVal)]));
        })
      });
    }
    return runtime.makeObject({
      "provide-plus-types": runtime.makeObject({
        values: runtime.makeObject({
          mouse: gmf(reactorEvents, "mouse"),
          keypress: gmf(reactorEvents, "keypress"),
          tick: gmf(reactorEvents, "tick"),
          "make-reactor": runtime.makeFunction(makeReactor)
        }),
        types: {
          Event: gtf(reactorEvents, "Event")
        },
        internal: {
          setInteract: setInteract          
        }
      })
    });
  }
})
