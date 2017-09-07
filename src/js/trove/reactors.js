({
  requires: [
    { "import-type": "builtin", "name": "reactor-events" },
    { "import-type": "builtin", "name": "valueskeleton" },
    { "import-type": "builtin", "name": "table" }
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
      "time-tick": ["local", "Event"],

      "get-value": ["forall", ["a"], ["arrow", ["RofA"], ["tid", "a"]]],
      "get-instance": ["forall", ["a"], ["arrow", ["RofA"], ["tid", "a"]]],
      "draw": ["forall", ["a"], ["arrow", ["RofA"], "Image"]],
      "interact": ["forall", ["a"], ["arrow", ["RofA"], "RofA"]],
      "interact-trace": ["forall", ["a"], ["arrow", ["RofA"], "Any"]],
      "simulate-trace": ["forall", ["a"], ["arrow", ["RofA", "Number"], "Any"]],
      "start-trace": ["forall", ["a"], ["arrow", ["RofA"], "RofA"]],
      "stop-trace": ["forall", ["a"], ["arrow", ["RofA"], "RofA"]],
      "get-trace": ["forall", ["a"], ["arrow", ["RofA"], ["List", ["tid", "a"]]]],
      "get-trace-as-table": ["forall", ["a"], ["arrow", ["RofA"], "Any"]],
      "react": ["forall", ["a"], ["arrow", ["RofA", ["local", "Event"]], "RofA"]],
    },
    aliases: {
      "Event": "ReactorEvent",
      "Reactor": ["local", "Reactor"]
    },
    datatypes: {
      "Reactor": ["data", "Reactor", ["a"], [], {
        "get-value": ["arrow", [], ["tid", "a"]],
        "draw": ["arrow", [], "Image"],
        "interact": ["arrow", [], "RofA"],
        "interact-trace": ["forall", ["a"], ["arrow", [], "Any"]],
        "simulate-trace": ["forall", ["a"], ["arrow", ["Number"], "Any"]],
        "start-trace": ["arrow", [], "RofA"],
        "stop-trace": ["arrow", [], "RofA"],
        "get-trace": ["arrow", [], ["List", ["tid", "a"]]],
        "get-trace-as-table": ["arrow", [], "Any"],
        "react": ["arrow", [["local", "Event"]], "RofA"],
        "is-stopped": ["arrow", [], "Boolean"],
        "_output": ["arrow", [], "ValueSkeleton"]
      }]
    },
  },

  theModule: function(runtime, _, uri, reactorEvents, VSlib, tables, reactorLib) {
    var gf = runtime.getField;
    var gmf = function(m, f) { return gf(runtime.getField(m, "values"), f); }
    var gtf = function(m, f) { return gf(m, "types")[f]; }
    var VS = runtime.getField(VSlib, "values");

    var brandReactor = runtime.namedBrander("reactors", ["reactors"]);
    var annReactor = runtime.makeBranderAnn(brandReactor, "Reactor");

    var checkArity = runtime.ffi.checkArity;

    var annEvent = gtf(reactorEvents, "Event");

    function applyBrand(brand, val) {
      return gf(brand, "brand").app(val);
    }
    function hasBrand(brand, val) {
      return gf(brand, "test").app(val);
    }

    var isEvent = gmf(reactorEvents, "is-Event");
    var externalInteractionHandler = null;
    var setInteract = function(newInteract) {
      externalInteractionHandler = newInteract;
    }
    var makeReactor = function(init, fields) {
      runtime.ffi.checkArity(2, arguments, "reactor", false);
      runtime.checkObject(fields);
      var handlerDict = {};
      Object.keys(fields.dict).forEach(function(f) {
        if(runtime.ffi.isSome(gf(fields, f))) {
          handlerDict[f] = gf(gf(fields, f), "value");
        }
      });
      return makeReactorRaw(init, handlerDict, false, []);
    }
    var makeReactorRaw = function(init, handlers, tracing, trace) {
      var o = runtime.makeObject({
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
        "interact-trace": runtime.makeMethod0(function(self) {
          return runtime.safeThen(function() {
            return gf(self, "start-trace").app();
          }).then(function(val) {
            return gf(val, "interact").app();
          }).then(function(val) {
            return gf(val, "get-trace-as-table").app(); 
          }).start();
        }),
        "simulate-trace": runtime.makeMethod1(function(self, limit) {
          function help(r, i) {
            return r.then(function(rval) {
              if(i <= 0) {
                return gf(rval, "get-trace-as-table").app()
              }
              else {
                return runtime.safeThen(function() {
                  return gf(rval, "is-stopped").app();
                }).then(function(isStopped) {
                  if(isStopped) {
                    return gf(rval, "get-trace-as-table").app()
                  }
                  else {
                    return help(runtime.safeThen(function() {
                      return gf(rval, "react").app(gmf(reactorEvents, "time-tick"));
                    }), i - 1).start();
                  }
                }).start()
              }
            });
          }
          var withTracing = runtime.safeThen(function() {
            return gf(self, "start-trace").app();
          });
          return help(withTracing, limit).start();
        }),
        interact: runtime.makeMethod0(function(self) {
          if(externalInteractionHandler === null) {
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
          return runtime.safeCall(function() {
            return externalInteractionHandler(init, handlers, tracer);
          }, function(newVal) {
            // This unshift prevents duplicate first elements
            thisInteractTrace.shift();
            return makeReactorRaw(newVal, handlers, tracing, trace.concat(thisInteractTrace));
          }, "interact");
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
            runtime.ffi.throwMessageException("Tried to get trace of a reactor that isn't tracing; try calling start-trace() first");
          }
        }),
        "get-trace-as-table": runtime.makeMethod0(function(self) {
          if(tracing) {
            var i = 0;
            var rows = trace.map(function(state) {
              var ans = [i, state];
              i += 1;
              return ans;
            });
            return tables.makeTable(["tick", "state"], rows);
          }
          else {
            runtime.ffi.throwMessageException("Tried to get trace of a reactor that isn't tracing; try calling start-trace() first");
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
              }, "react:" + handlerName);
            }
            else {
              runtime.ffi.throwMessageException("No " + handlerName + " handler defined");
            }
          }
          return runtime.safeCall(function() {
              if(handlers["stop-when"]) {
                return handlers["stop-when"].app(init);
              }
              else {
                return false;
              }
            }, function(stop) {
              if(stop) {
                return self;
              }
              else {
                return runtime.ffi.cases(isEvent, "Event", event, {
                  keypress: function(key) {
                    return callOrError("on-key", [init, key]);
                  },
                  "time-tick": function() {
                    return callOrError("on-tick", [init]);
                  },
                  mouse: function(x, y, kind) {
                    return callOrError("on-mouse", [init, x, y, kind]);
                  }
                });
              }
            }, "react:stop-when");
        }),
        "is-stopped": runtime.makeMethod0(function(self) {
          if(handlers["stop-when"]) {
            return handlers["stop-when"].app(init);
          }
          else {
            return false;
          }
        }),
        _output: runtime.makeMethod0(function(self) {
          return runtime.getField(VS, "vs-constr").app(
            "reactor",
            runtime.ffi.makeList([ gf(VS, "vs-value").app(init) ]));
        })
      });
      return applyBrand(brandReactor, o);
    }

    var c = function(name, ...argsAndAnns) {
      runtime.checkArgsInternalInline("reactors", name, ...argsAndAnns);
    }

    function getValue(reactor) {
      checkArity(1, arguments, "reactors", false);
      c("get-value", reactor, annReactor);
      return runtime.getField(reactor, "get-value").app();
    }

    function draw(reactor) {
      checkArity(1, arguments, "reactors", false);
      c("draw", reactor, annReactor);
      return runtime.getField(reactor, "draw").app();
    }

    function interact(reactor) {
      checkArity(1, arguments, "reactors", false);
      c("interact", reactor, annReactor);
      return runtime.getField(reactor, "interact").app();
    }

    function react(reactor, event) {
      checkArity(2, arguments, "reactors", false);
      c("react", reactor, annReactor, event, annEvent);
      return runtime.getField(reactor, "react").app(event);
    }

    function getTrace(reactor) {
      checkArity(1, arguments, "reactors", false);
      c("get-trace", reactor, annReactor);
      return runtime.getField(reactor, "get-trace").app();
    }

    function getTraceAsTable(reactor) {
      checkArity(1, arguments, "reactors", false);
      c("get-trace-as-table", reactor, annReactor);
      return runtime.getField(reactor, "get-trace-as-table").app();
    }

    function startTrace(reactor) {
      checkArity(1, arguments, "reactors", false);
      c("start-trace", reactor, annReactor);
      return runtime.getField(reactor, "start-trace").app();
    }

    function interactTrace(reactor) {
      checkArity(1, arguments, "reactors", false);
      c("interact-trace", reactor, annReactor);
      return runtime.getField(reactor, "interact-trace").app();
    }

    function simulateTrace(reactor, limit) {
      checkArity(2, arguments, "reactors", false);
      c("simulate-trace", reactor, annReactor, limit, runtime.NumInteger);
      return runtime.getField(reactor, "simulate-trace").app(limit);
    }

    function stopTrace(reactor) {
      checkArity(1, arguments, "reactors", false);
      c("stop-trace", reactor, annReactor);
      return runtime.getField(reactor, "stop-trace").app();
    }

    var F = runtime.makeFunction;

    var values = {
      mouse: gmf(reactorEvents, "mouse"),
      keypress: gmf(reactorEvents, "keypress"),
      "time-tick": gmf(reactorEvents, "time-tick"),
      "make-reactor": F(makeReactor, "make-reactor"),

      "get-value": F(getValue, "get-value"),
      "get-instance": F(getValue, "get-instance"),
      "draw": F(draw, "draw"),
      "get-trace": F(getTrace, "get-trace"),
      "get-trace-as-table": F(getTraceAsTable, "get-trace-as-table"),
      "start-trace": F(startTrace, "start-trace"),
      "stop-trace": F(stopTrace, "stop-trace"),
      "interact-trace": F(interactTrace, "interact-trace"),
      "simulate-trace": F(simulateTrace, "simulate-trace"),
      "react": F(react, "react"),
      "interact": F(interact, "interact")

    };

    var types = {
      Event: gtf(reactorEvents, "Event"),
      Reactor: annReactor
    };

    var internal = {
      setInteract: setInteract          
    };
    return runtime.makeModuleReturn(values, types, internal);
  }
})
