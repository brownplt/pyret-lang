import { Table } from './tables.arr';

const global = require('./global.arr.js');
const reactorEvents = require('./reactor-events.arr.js');
const tables = require('./tables.arr.js');
const option = require('./option.arr.js');
const world = require('./world.js');

// TODO: what's the type of externalInteractionHandler?
type IDKFunction<A> = (...args: any[]) => A;
type InteractionHandler<A> = null | IDKFunction<A>;
var externalInteractionHandler: InteractionHandler<any> = null;

function setInteract(newInteract: InteractionHandler<any>) {
    externalInteractionHandler = newInteract;
}

type Some<A> = {
    '$brand': {
        'names': ['elt'],
    },
    '$tag': 0,
    'elt': A,
};

type None = {
    '$brand': {
        'names' :false,
    },
    '$tag': 1,
}

type Option<A> = None | Some<A>;

type ReactorFields<A> = {
    'on-tick': Option<(a: A) => A>,
    'seconds-per-tick': Option<number>,
    // to-draw is supposed to return an Image, but the image library
    // isn't written in TypeScript, hence the `any`
    'to-draw': Option<(a: A) => any>,
    'on-key': Option<(a: A, key: string) => A>,
    'on-mouse': Option<(a: A, x: number, y: number, eventType: string) => A>,
    'stop-when': Option<(a: A) => boolean>,
    'close-when-stop': Option<boolean>,
    title: Option<string>,
};

type RawReactorFields<A> = {
    'on-tick'?: (a: A) => A,
    'seconds-per-tick'?: number,
    'to-draw'?: (a: A) => any,
    'on-key'?: (a: A, key: string) => A,
    'on-mouse'?: (a: A, x: number, y: number, eventType: string) => A,
    'stop-when'?: (a: A) => boolean,
    'close-when-stop'?: boolean,
    title?: string,
};

function makeReactor<A>(init: A, fields: ReactorFields<A>): Reactor<A> {
    const rawFields: RawReactorFields<A> = {};
    Object.getOwnPropertyNames(fields).forEach((field) => {
        if (option['is-some'](fields[field])) {
            rawFields[field] = fields[field].value;
        }
    });
    return makeReactorRaw<A>(init, rawFields, false, []);
}

type Reactor<A> = {
    $brand: 'reactor',
    'get-value': () => A,
    draw: () => any, // `any` means `Image` here
    'interact-trace': () => Table,
    'simulate-trace': (limit: number) => Table,
    interact: () => Reactor<A>,
    $interact: (insertNode?: (node: any, title: string, setupClose: (close: () => void) => void) => void) => Reactor<A>,
    $interactNoPauseResume: (insertNode?: (node: any, title: string, setupClose: (close: () => void) => void) => void) => Reactor<A>,
    $shutdown: () => void,
    'start-trace': () => Reactor<A>,
    'stop-trace': () => Reactor<A>,
    'get-trace': () => A[], // should be List<A> type
    'get-trace-as-table': () => Table,
    react: (event: any) => Reactor<A>, // `any` should be ReactorEvent
    'is-stopped': () => boolean,
};

function makeReactorRaw<A>(init: A, handlers: RawReactorFields<A>, tracing: boolean, trace: A[]): Reactor<A> {
    const self: Reactor<A> = {
        $brand: 'reactor',
        'get-value': () => {
            return init;
        },
        draw: () => {
            if (!handlers.hasOwnProperty('to-draw')) {
                throw new Error('Cannot draw() because no to-draw was specified on this reactor.');
            }

            return handlers['to-draw'](init);
        },
        'interact-trace': () => {
            return self['start-trace']().interact()['get-trace-as-table']();
        },
        'simulate-trace': (limit) => {
            let r: Reactor<A> = self['start-trace']();

            for (let i = limit; i > 0; i -= 1) {
                if (r['is-stopped']()) {
                    break;
                }

                r = r.react(reactorEvents['time-tick']);
            }

            return r['get-trace-as-table']();
        },
        $interact: (insertNode) => {
            if (externalInteractionHandler === null) {
                throw new Error('No interaction set up for this context (please report a bug if you are using code.pyret.org and see this message)')
            }
            var thisInteractTrace = [];
            var tracer = null;
            if(tracing) {
                tracer = function(newVal, oldVal, k) {
                    thisInteractTrace.push(newVal);
                    k();
                };
            }

            const newVal = externalInteractionHandler(init, handlers, tracer, insertNode);
            // This unshift prevents duplicate first elements
            thisInteractTrace.shift();
            return makeReactorRaw(newVal, handlers, tracing, trace.concat(thisInteractTrace));
        },
        interact: () => {
            return self.$interact();
        },
        $interactNoPauseResume: (insertNode) => {
            const oldInteract = externalInteractionHandler;
            setInteract(world.$bigBangFromDictNoPauseResume);

            try {
                return self.$interact(insertNode);
            } catch (e) {
                setInteract(oldInteract);
                throw e;
            }
        },
        $shutdown: /* @stopify flat */ () => world.$shutdown({cleanShutdown: true}),
        'start-trace': () => {
            return makeReactorRaw(init, handlers, true, [init]);
        },
        'stop-trace': () => {
            return makeReactorRaw(init, handlers, false, []);
        },
        'get-trace': () => {
            if(tracing) {
                return trace;
            }
            else {
                throw new Error('Tried to get trace of a reactor that isn\'t tracing; try calling start-trace() first')
            }
        },
        'get-trace-as-table': () => {
            if(tracing) {
                var i = 0;
                var rows = trace.map(function(state) {
                    var ans = [i, state];
                    i += 1;
                    return ans;
                });
                return tables.makeTable(['tick', 'state'], rows);
            }
            else {
                throw new Error('Tried to get trace of a reactor that isn\'t tracing; try calling start-trace() first')
            }
        },
        react: (event) => {
            function callOrError(handlerName, args) {
                if(handlers.hasOwnProperty(handlerName)) {
                    var funObj = handlers[handlerName];
                    const newVal = funObj.apply(funObj, args);
                    if (tracing) {
                        var newTrace = trace.concat([newVal]);
                    }
                    else {
                        var newTrace = trace;
                    }
                    return makeReactorRaw(newVal, handlers, tracing, newTrace);
                }
                else {
                    throw new Error('No ' + handlerName + ' handler defined')
                }
            }

            const stop = handlers['stop-when'] ? handlers['stop-when'](init) : false;
            if (stop) {
                return self;
            }
            else {
                if (event['is-keypress']) {
                    const { key } = event;
                    return callOrError('on-key', [init, key]);
                } else if (event['is-time-tick']) {
                    return callOrError('on-tick', [init]);
                } else if (event['is-mouse']) {
                    const { x, y, kind } = event;
                    return callOrError('on-mouse', [init, x, y, kind]);
                }
            }
        },
        'is-stopped': () => {
            if(handlers['stop-when']) {
                return handlers['stop-when'](init);
            }
            else {
                return false;
            }
        },
    };

    return self;
}

function getValue<A>(reactor: Reactor<A>): A {
    return reactor['get-value']();
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
    return reactor['get-trace']();
}

function getTraceAsTable<A>(reactor: Reactor<A>): Table {
    return reactor['get-trace-as-table']();
}

function startTrace<A>(reactor: Reactor<A>): Reactor<A> {
    return reactor['start-trace']();
}

function interactTrace<A>(reactor: Reactor<A>): Table {
    return reactor['interact-trace']();
}

function simulateTrace<A>(reactor: Reactor<A>, limit: number): Table {
    return reactor['simulate-trace'](limit);
}

function stopTrace<A>(reactor: Reactor<A>): Reactor<A> {
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
    '$setInteract': setInteract,
};

global.$setMakeReactor(makeReactor);
setInteract(world.$bigBangFromDict);
