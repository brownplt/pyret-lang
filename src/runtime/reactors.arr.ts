import { Table } from "./tables.arr";

const runtime = require("./runtime.js");
const reactorEvents = require("./reactor-events.arr.js");
const tables = require("./tables.arr.js");

var isEvent = reactorEvents["is-Event"];

// TODO: what's the type of externalInteractionHandler?
type IDKFunction = (...args: any[]) => any;
type InteractionHandler = null | IDKFunction;
var externalInteractionHandler: InteractionHandler = null;

type ReactorFields<A> = {
    init: A,
    "on-tick"?: (a: A) => A,
    "seconds-per-tick"?: number,
    // to-draw is supposed to return an Image, but the image library
    // isn't written in TypeScript, hence the `any`
    "to-draw"?: (a: A) => any,
    "on-key"?: (a: A, key: string) => A,
    "on-mouse"?: (a: A, x: number, y: number, eventType: string) => A,
    "stop-when"?: (a: A) => boolean,
    "close-when-stop"?: boolean,
    title?: string,
};

function makeReactor<A>(init: A, fields: ReactorFields<A>): Reactor<A> {
    return makeReactorRaw<A>(init, fields, false, []);
}

type Reactor<A> = {
    $brand: "reactor",
    "get-value": () => A,
    draw: () => any, // `any` means `Image` here
    "interact-trace": () => Table,
    "simulate-trace": (limit: number) => Table,
    interact: () => Reactor<A>,
    "start-trace": () => Reactor<A>,
    "stop-trace": () => Reactor<A>,
    "get-trace": () => A[], // should be List<A> type
    "get-trace-as-table": () => Table,
    react: (event: any) => Reactor<A>, // `any` should be ReactorEvent
    "is-stopped": () => boolean,
};

function makeReactorRaw<A>(init: A, handlers: ReactorFields<A>, tracing: boolean, trace: A[]): Reactor<A> {
    const self: Reactor<A> = {
        $brand: "reactor",
        "get-value": () => {
            return init;
        },
        draw: () => {
            if (!handlers.hasOwnProperty("to-draw")) {
                throw new Error("Cannot draw() because no to-draw was specified on this reactor.");
            }

            return handlers["to-draw"](init);
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
            if (externalInteractionHandler === null) {
                throw new Error("No interaction set up for this context (please report a bug if you are using code.pyret.org and see this message)")
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
                throw new Error("Tried to get trace of a reactor that isn't tracing; try calling start-trace() first")
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
                throw new Error("Tried to get trace of a reactor that isn't tracing; try calling start-trace() first")
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
                    throw new Error("No " + handlerName + " handler defined")
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

function getValue<A>(reactor: Reactor<A>): A {
    return reactor["get-value"]();
}

function draw<A>(reactor: Reactor<A>): any {
    return reactor.draw();
}

function interact<A>(reactor: Reactor<A>): Reactor<A> {
    return reactor.interact();
}

function react<A>(reactor: Reactor<A>, event: any): Reactor<A> {
    return reactor.react(event);
}

function getTrace<A>(reactor: Reactor<A>): A[] {
    return reactor["get-trace"]();
}

function getTraceAsTable<A>(reactor: Reactor<A>): Table {
    return reactor["get-trace-as-table"]();
}

function startTrace<A>(reactor: Reactor<A>): Reactor<A> {
    return reactor["start-trace"]();
}

function interactTrace<A>(reactor: Reactor<A>): Table {
    return reactor["interact-trace"]();
}

function simulateTrace<A>(reactor: Reactor<A>, limit: number): Table {
    return reactor["simulate-trace"](limit);
}

function stopTrace<A>(reactor: Reactor<A>): Reactor<A> {
    return reactor["stop-trace"]();
}

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
