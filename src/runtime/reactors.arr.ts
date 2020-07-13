const runtime = require("./runtime.js");
const reactorEvents = require("./reactor-events.arr.js");
const tables = require("./tables.arr.js");

var gtf = function(m, f) { return m.types[f]; }

var annEvent = gtf(reactorEvents, "Event");

var isEvent = reactorEvents["is-Event"];
var externalInteractionHandler = null;
var setInteract = function(newInteract) {
    externalInteractionHandler = newInteract;
}
var makeReactor = function(init, fields) {
    var handlerDict = {};
    Object.keys(fields.dict).forEach(function(f) {
        if(runtime.ffi.isSome(fields[f])) {
            handlerDict[f] = fields[f].value;
        }
    });
    return makeReactorRaw(init, handlerDict, false, []);
}
var makeReactorRaw = function(init, handlers, tracing, trace) {
    const self = {
        "$brand": "reactor",
        "get-value": () => {
            return init;
        },
        "draw": () => {
            if(!handlers.hasOwnProperty("to-draw")) {
                runtime.ffi.throwMessageException("Cannot draw() because no to-draw was specified on this reactor.");
            }
            var drawer = handlers["to-draw"];
            return drawer(init);
        },
        "interact-trace": () => {
            return runtime.safeThen(function() {
                return self["start-trace"]();
            }).then(function(val) {
                return val.interact();
            }).then(function(val) {
                return val["get-trace-as-table"]();
            }).start();
        },
        "simulate-trace": (limit) => {
            function help(r, i) {
                return r.then(function(rval) {
                    if(i <= 0) {
                        return rval["get-trace-as-table"]();
                    }
                    else {
                        return runtime.safeThen(function() {
                            return rval["is-stopped"]();
                        }).then(function(isStopped) {
                            if(isStopped) {
                                return rval["get-trace-as-table"]()
                            }
                            else {
                                return help(runtime.safeThen(function() {
                                    return rval.react(reactorEvents["time-tick"]);
                                }), i - 1).start();
                            }
                        }).start()
                    }
                });
            }
            var withTracing = runtime.safeThen(function() {
                return self["start-trace"]();
            });
            return help(withTracing, limit).start();
        },
        interact: () => {
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
        },
        "start-trace": () => {
            return makeReactorRaw(init, handlers, true, [init]);
        },
        "stop-trace": () => {
            return makeReactorRaw(init, handlers, false, []);
        },
        "get-trace": () => {
            if(tracing) {
                return runtime.ffi.makeList(trace);
            }
            else {
                runtime.ffi.throwMessageException("Tried to get trace of a reactor that isn't tracing; try calling start-trace() first");
            }
        },
        "get-trace-as-table": () => {
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
        },
        react: (event) => {
            function callOrError(handlerName, args) {
                if(handlers.hasOwnProperty(handlerName)) {
                    var funObj = handlers[handlerName];
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
                    return handlers["stop-when"](init);
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
        },
        "is-stopped": () => {
            if(handlers["stop-when"]) {
                return handlers["stop-when"](init);
            }
            else {
                return false;
            }
        },
    };

    return self;
}

function getValue(reactor) {
    return reactor["get-value"]();
}

function draw(reactor) {
    return reactor.draw();
}

function interact(reactor) {
    return reactor.interact();
}

function react(reactor, event) {
    return reactor.react(event);
}

function getTrace(reactor) {
    return reactor["get-trace"]();
}

function getTraceAsTable(reactor) {
    return reactor["get-trace-as-table"]();
}

function startTrace(reactor) {
    return reactor["start-trace"]();
}

function interactTrace(reactor) {
    return reactor["interact-trace"]();
}

function simulateTrace(reactor, limit) {
    return reactor["simulate-trace"](limit);
}

function stopTrace(reactor) {
    return reactor["stop-trace"]();
}

var internal = {
    setInteract: setInteract
};

module.exports = {
    mouse: reactorEvents["mouse"],
    keypress: reactorEvents["keypress"],
    "time-tick": reactorEvents["time-tick"],
    "make-reactor": makeReactor,
    "get-value": getValue,
    "get-instance": getValue,
    "draw": draw,
    "get-trace": getTrace,
    "get-trace-as-table": getTraceAsTable,
    "start-trace": startTrace,
    "stop-trace": stopTrace,
    "interact-trace": interactTrace,
    "simulate-trace": simulateTrace,
    "react": react,
    "interact": interact,
};
