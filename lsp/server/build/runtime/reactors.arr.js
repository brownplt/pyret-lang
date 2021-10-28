"use strict";
exports.__esModule = true;
var global = require('./global.arr.js');
var reactorEvents = require('./reactor-events.arr.js');
var tables = require('./tables.arr.js');
var option = require('./option.arr.js');
var world = require('./world.js');
var externalInteractionHandler = null;
function setInteract(newInteract) {
    externalInteractionHandler = newInteract;
}
function makeReactor(init, fields) {
    var rawFields = {};
    Object.getOwnPropertyNames(fields).forEach(function (field) {
        if (option['is-some'](fields[field])) {
            rawFields[field] = fields[field].value;
        }
    });
    return makeReactorRaw(init, rawFields, false, []);
}
function makeReactorRaw(init, handlers, tracing, trace) {
    var self = {
        $brand: 'reactor',
        'get-value': function () {
            return init;
        },
        draw: function () {
            if (!handlers.hasOwnProperty('to-draw')) {
                throw new Error('Cannot draw() because no to-draw was specified on this reactor.');
            }
            return handlers['to-draw'](init);
        },
        'interact-trace': function () {
            return self['start-trace']().interact()['get-trace-as-table']();
        },
        'simulate-trace': function (limit) {
            var r = self['start-trace']();
            for (var i = limit; i > 0; i -= 1) {
                if (r['is-stopped']()) {
                    break;
                }
                r = r.react(reactorEvents['time-tick']);
            }
            return r['get-trace-as-table']();
        },
        $interact: function (insertNode) {
            if (externalInteractionHandler === null) {
                throw new Error('No interaction set up for this context (please report a bug if you are using code.pyret.org and see this message)');
            }
            var thisInteractTrace = [];
            var tracer = null;
            if (tracing) {
                tracer = function (newVal, oldVal, k) {
                    thisInteractTrace.push(newVal);
                    k();
                };
            }
            var newVal = externalInteractionHandler(init, handlers, tracer, insertNode);
            // This unshift prevents duplicate first elements
            thisInteractTrace.shift();
            return makeReactorRaw(newVal, handlers, tracing, trace.concat(thisInteractTrace));
        },
        interact: function () {
            return self.$interact();
        },
        $interactNoPauseResume: function (insertNode) {
            var oldInteract = externalInteractionHandler;
            setInteract(world.$bigBangFromDictNoPauseResume);
            try {
                return self.$interact(insertNode);
            }
            catch (e) {
                setInteract(oldInteract);
                throw e;
            }
        },
        $shutdown: /* @stopify flat */ function () { return world.$shutdown({ cleanShutdown: true }); },
        'start-trace': function () {
            return makeReactorRaw(init, handlers, true, [init]);
        },
        'stop-trace': function () {
            return makeReactorRaw(init, handlers, false, []);
        },
        'get-trace': function () {
            if (tracing) {
                return trace;
            }
            else {
                throw new Error('Tried to get trace of a reactor that isn\'t tracing; try calling start-trace() first');
            }
        },
        'get-trace-as-table': function () {
            if (tracing) {
                var i = 0;
                var rows = trace.map(function (state) {
                    var ans = [i, state];
                    i += 1;
                    return ans;
                });
                return tables.makeTable(['tick', 'state'], rows);
            }
            else {
                throw new Error('Tried to get trace of a reactor that isn\'t tracing; try calling start-trace() first');
            }
        },
        react: function (event) {
            function callOrError(handlerName, args) {
                if (handlers.hasOwnProperty(handlerName)) {
                    var funObj = handlers[handlerName];
                    var newVal = funObj.apply(funObj, args);
                    if (tracing) {
                        var newTrace = trace.concat([newVal]);
                    }
                    else {
                        var newTrace = trace;
                    }
                    return makeReactorRaw(newVal, handlers, tracing, newTrace);
                }
                else {
                    throw new Error('No ' + handlerName + ' handler defined');
                }
            }
            var stop = handlers['stop-when'] ? handlers['stop-when'](init) : false;
            if (stop) {
                return self;
            }
            else {
                if (event['is-keypress']) {
                    var key = event.key;
                    return callOrError('on-key', [init, key]);
                }
                else if (event['is-time-tick']) {
                    return callOrError('on-tick', [init]);
                }
                else if (event['is-mouse']) {
                    var x = event.x, y = event.y, kind = event.kind;
                    return callOrError('on-mouse', [init, x, y, kind]);
                }
            }
        },
        'is-stopped': function () {
            if (handlers['stop-when']) {
                return handlers['stop-when'](init);
            }
            else {
                return false;
            }
        }
    };
    return self;
}
function getValue(reactor) {
    return reactor['get-value']();
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
    return reactor['get-trace']();
}
function getTraceAsTable(reactor) {
    return reactor['get-trace-as-table']();
}
function startTrace(reactor) {
    return reactor['start-trace']();
}
function interactTrace(reactor) {
    return reactor['interact-trace']();
}
function simulateTrace(reactor, limit) {
    return reactor['simulate-trace'](limit);
}
function stopTrace(reactor) {
    return reactor['stop-trace']();
}
module.exports = {
    mouse: reactorEvents['mouse'],
    keypress: reactorEvents['keypress'],
    'time-tick': reactorEvents['time-tick'],
    'make-reactor': makeReactor,
    'get-value': getValue,
    'get-instance': getValue,
    'draw': draw,
    'get-trace': getTrace,
    'get-trace-as-table': getTraceAsTable,
    'start-trace': startTrace,
    'stop-trace': stopTrace,
    'interact-trace': interactTrace,
    'simulate-trace': simulateTrace,
    'react': react,
    'interact': interact,
    '$setInteract': setInteract
};
global.$setMakeReactor(makeReactor);
setInteract(world.$bigBangFromDict);
