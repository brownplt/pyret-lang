({
  requires: [
    { "import-type": "builtin", "name": "reactor-events" },
    { "import-type": "builtin", "name": "valueskeleton" },
    { "import-type": "builtin", "name": "table" }
  ],
  nativeRequires: [
    "pyret-base/js/js-numbers",
  ],
  provides: {
    shorthands: {
      "RofA": ["tyapp", ["local", "Reactor"], [["tid", "a"]]],
      "ReactorEvent": { tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://reactor-events" },
                 name: "Event" },
      "RawKeyEventType": { tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://reactor-events" },
                 name: "RawKeyEventType" },
      "Image": { tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://image-lib" },
                 name: "Image" },
      "ValueSkeleton": { tag: "name",
                         origin: { "import-type": "uri", uri: "builtin://valueskeleton" },
                         name: "ValueSkeleton" },
      "Table": { tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://global" },
                 name: "Table" }
    },
    values: {
      "keypress": ["arrow", ["String"], ["local", "Event"]],
      "mouse": ["arrow", ["Number", "Number", "String"], ["local", "Event"]],
      "time-tick": ["local", "Event"],
      "raw-key": ["local", "Event"],
      "key-up": ["local", "RawKeyEventType"],
      "key-down": ["local", "RawKeyEventType"],
      "key-press": ["local", "RawKeyEventType"],

      "get-value": ["forall", ["a"], ["arrow", ["RofA"], ["tid", "a"]]],
      "get-instance": ["forall", ["a"], ["arrow", ["RofA"], ["tid", "a"]]],
      "draw": ["forall", ["a"], ["arrow", ["RofA"], "Image"]],
      "interact": ["forall", ["a"], ["arrow", ["RofA"], "RofA"]],
      "interact-trace": ["forall", ["a"], ["arrow", ["RofA"], "Table"]],
      "simulate-trace": ["forall", ["a"], ["arrow", ["RofA", "Number"], "Table"]],
      "start-trace": ["forall", ["a"], ["arrow", ["RofA"], "RofA"]],
      "stop-trace": ["forall", ["a"], ["arrow", ["RofA"], "RofA"]],
      "get-trace": ["forall", ["a"], ["arrow", ["RofA"], ["List", ["tid", "a"]]]],
      "get-trace-as-table": ["forall", ["a"], ["arrow", ["RofA"], "Table"]],
      "react": ["forall", ["a"], ["arrow", ["RofA", ["local", "Event"]], "RofA"]]
    },
    aliases: {
      "Event": "ReactorEvent",
      "RawKeyEventType": "RawKeyEventType",
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

  theModule: function(runtime, _, uri, reactorEvents, VSlib, tables, jsnums) {
    var gf = runtime.getField;
    var gmf = function(m, f) { return gf(runtime.getField(m, "values"), f); }
    var gtf = function(m, f) { return gf(m, "types")[f]; }
    var VS = runtime.getField(VSlib, "values");

    var brandReactor = runtime.namedBrander("reactors", ["reactors"]);
    var annReactor = runtime.makeBranderAnn(brandReactor, "Reactor");

    var checkArity = runtime.ffi.checkArity;

    const c = function(name, ...argsAndAnns) {
      runtime.checkArgsInternalInline("reactors", name, ...argsAndAnns);
    };
    const c1 = function(name, arg, ann) {
      runtime.checkArgsInternal1("reactors", name, arg, ann);
    };
    const c2 = function(name, arg1, ann1, arg2, ann2) {
      runtime.checkArgsInternal2("reactors", name, arg1, ann1, arg2, ann2);
    };
    const c3 = function(name, arg1, ann1, arg2, ann2, arg3, ann3) {
      runtime.checkArgsInternal3("reactors", name, arg1, ann1, arg2, ann2, arg3, ann3);
    };

    var ann = function(name, pred) {
      return runtime.makePrimitiveAnn(name, pred);
    };

    var annEvent = gtf(reactorEvents, "Event");
    var annNatural = runtime.makeFlatPredAnn(runtime.Number, runtime.makeFunction(function(val) {
        return jsnums.isInteger(val) && jsnums.greaterThanOrEqual(val, 0, runtime.NumberErrbacks);
    }, "Natural Number"), "Natural Number");
    var annObject = runtime.Object;
    
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
      c1("make-reactor", fields, annObject);
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
          checkArity(1, arguments, "get-value", true);
          return init;
        }),
        "draw": runtime.makeMethod0(function(self) {
          checkArity(1, arguments, "draw", true);
          if(!handlers.hasOwnProperty("to-draw")) {
            runtime.ffi.throwMessageException("Cannot draw() because no to-draw was specified on this reactor.");
          }
          var drawer = handlers["to-draw"];
          return drawer.app(init);
        }),
        "interact-trace": runtime.makeMethod0(function(self) {
          checkArity(1, arguments, "interact-trace", true);
          return runtime.safeThen(function() {
            return gf(self, "start-trace").app();
          }).then(function(val) {
            return gf(val, "interact").app();
          }).then(function(val) {
            return gf(val, "get-trace-as-table").app(); 
          }).start();
        }),
        "simulate-trace": runtime.makeMethod1(function(self, limit) {
          checkArity(2, arguments, "simulate-trace", true);
          c1("simulate-trace", limit, annNatural);
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
          checkArity(1, arguments, "interact", true);
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
          checkArity(1, arguments, "start-trace", true);
          return makeReactorRaw(init, handlers, true, [init]);
        }),
        "stop-trace": runtime.makeMethod0(function(self) {
          checkArity(1, arguments, "stop-trace", true);
          return makeReactorRaw(init, handlers, false, []);
        }),
        "get-trace": runtime.makeMethod0(function(self) {
          checkArity(1, arguments, "get-trace", true);
          if(tracing) {
            return runtime.ffi.makeList(trace);
          }
          else {
            runtime.ffi.throwMessageException("Tried to get trace of a reactor that isn't tracing; try calling start-trace() first");
          }
        }),
        "get-trace-as-table": runtime.makeMethod0(function(self) {
          checkArity(1, arguments, "get-trace-as-table", true);
          if(tracing) {
            var i = 0;
            var rows = trace.map(function(state) {
              var ans = [i, state];
              i += 1;
              return ans;
            });
            // console.log('tables =', tables);
            return gf(tables, "internal").makeTable(["tick", "state"], rows);
            // return runtime.makeTable(["tick", "state"], rows);
          }
          else {
            runtime.ffi.throwMessageException("Tried to get trace of a reactor that isn't tracing; try calling start-trace() first");
          }
        }),
        react: runtime.makeMethod1(function(self, event) {
          checkArity(2, arguments, "react", true);
          c1("react", event, annEvent);
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
                  },
                  "raw-key": function(key, type, caps, shift, alt, command, control) {
                    // NOTE(joe): we intentionally don't use all the fields above, assuming
                    // that users of on-raw-key are OK with consuming an event object
                    // rather than the fields of the event. This is mainly because typing
                    // out 8 parameters is pretty unreasonable, and this fancy version
                    // will mainly be used by folks who have gone through at least Reactive
                    return callOrError("on-raw-key", [init, event]);
                  }
                });
              }
            }, "react:stop-when");
        }),
        "is-stopped": runtime.makeMethod0(function(self) {
          checkArity(1, arguments, "is-stopped", true);
          if(handlers["stop-when"]) {
            return handlers["stop-when"].app(init);
          }
          else {
            return false;
          }
        }),
        _output: runtime.makeMethod0(function(self) {
          checkArity(1, arguments, "_output", true);
          return runtime.getField(VS, "vs-constr").app(
            "reactor",
            runtime.ffi.makeList([ gf(VS, "vs-value").app(init) ]));
        })
      });
      return applyBrand(brandReactor, o);
    }

    function getValue(reactor) {
      checkArity(1, arguments, "reactors", false);
      c1("get-value", reactor, annReactor);
      return runtime.getField(reactor, "get-value").app();
    }

    function draw(reactor) {
      checkArity(1, arguments, "reactors", false);
      c1("draw", reactor, annReactor);
      return runtime.getField(reactor, "draw").app();
    }

    function interact(reactor) {
      checkArity(1, arguments, "reactors", false);
      c1("interact", reactor, annReactor);
      return runtime.getField(reactor, "interact").app();
    }

    function react(reactor, event) {
      checkArity(2, arguments, "reactors", false);
      c2("react", reactor, annReactor, event, annEvent);
      return runtime.getField(reactor, "react").app(event);
    }

    function getTrace(reactor) {
      checkArity(1, arguments, "reactors", false);
      c1("get-trace", reactor, annReactor);
      return runtime.getField(reactor, "get-trace").app();
    }

    function getTraceAsTable(reactor) {
      checkArity(1, arguments, "reactors", false);
      c1("get-trace-as-table", reactor, annReactor);
      return runtime.getField(reactor, "get-trace-as-table").app();
    }

    function startTrace(reactor) {
      checkArity(1, arguments, "reactors", false);
      c1("start-trace", reactor, annReactor);
      return runtime.getField(reactor, "start-trace").app();
    }

    function interactTrace(reactor) {
      checkArity(1, arguments, "reactors", false);
      c1("interact-trace", reactor, annReactor);
      return runtime.getField(reactor, "interact-trace").app();
    }

    function simulateTrace(reactor, limit) {
      checkArity(2, arguments, "reactors", false);
      c2("simulate-trace", reactor, annReactor, limit, runtime.NumInteger);
      return runtime.getField(reactor, "simulate-trace").app(limit);
    }

    function stopTrace(reactor) {
      checkArity(1, arguments, "reactors", false);
      c1("stop-trace", reactor, annReactor);
      return runtime.getField(reactor, "stop-trace").app();
    }

    var F = runtime.makeFunction;

    var values = {
      mouse: gmf(reactorEvents, "mouse"),
      keypress: gmf(reactorEvents, "keypress"),
      "time-tick": gmf(reactorEvents, "time-tick"),
      "raw-key": gmf(reactorEvents, "raw-key"),
      "key-up": gmf(reactorEvents, "key-up"),
      "key-down": gmf(reactorEvents, "key-down"),
      "key-press": gmf(reactorEvents, "key-press"),
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
