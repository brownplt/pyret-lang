const imageLibrary = require('./image.arr.js');
const rawJsworld = require('./world-lib.js').jsworld;
const jsnums = require('./js-numbers.js');
const anchorRuntime = require('./runtime.js');

console.log('image library', imageLibrary);
console.log('rawJsworld', rawJsworld);
console.log('jsnums', jsnums);
console.log('anchorRuntime', anchorRuntime);
console.log('module.exports', module.exports);

// TODO(michael): this should throw pretty error messages via error.arr
function makeCheckType(pred, type) {
    return (val) => {
        if (!pred(val)) {
            throw new Error(`Evaluating an expression failed. It was supposed to produce to a ${type}, but it produced a non-${type} value: ${val}`);
        }
    };
}

function POpaque(val, equals) {
    this.val = val;
    this.equals = equals;
}

function isOpaque(val) {
    return val instanceof POpaque;
}

function makeOpaque(val, equals) {
    return new POpaque(val, equals);
}

var isImage = imageLibrary['is-image'];

//////////////////////////////////////////////////////////////////////

// An Opaque is a Pyret concept for a value wrapping a hidden
// implementation.  Check that a value is one of these, and internally is
// a WorldConfigOption
var isOpaqueWorldConfigOption = function(v) {
    return isOpaque(v) && isWorldConfigOption(v.val);
}
var isOpaqueOnTick = function(v) {
    return isOpaque(v) && (v.val instanceof OnTick);
}
var isOpaqueToDraw = function(v) {
    return isOpaque(v) && (v.val instanceof ToDraw);
}

var makeReactor = function(init, handlers) {
    var arr = handlers;
    var initialWorldValue = init;
    arr.map(function(h) { checkHandler(h); });
    return makeReactorRaw(init, arr, false, []);
}
var makeReactorRaw = function(init, handlersArray, tracing, trace) {
    const self = {
        "get-value": () => {
            return init;
        },
        "draw": () => {
            var drawer = handlersArray.filter(function(h) {
                return isOpaqueToDraw(h);
            })[0];
            if(drawer === undefined) {
                throw new Error("Tried to draw() a reactor with no to-draw");
            }
            return drawer.val.handler.app(init);
        },
        interact: () => {
            var thisInteractTrace = [];
            var tracer = null;
            if(tracing) {
                tracer = function(newVal, oldVal, k) {
                    thisInteractTrace.push(newVal);
                    k();
                };
            }
            const newVal = bigBang(init, handlersArray, tracer);
            return makeReactorRaw(newVal, handlersArray, tracing, trace.concat(thisInteractTrace));
        },
        "start-trace": () => {
            return makeReactorRaw(init, handlersArray, true, []);
        },
        "stop-trace": () => {
            return makeReactorRaw(init, handlersArray, false, []);
        },
        "get-trace": () => {
            return trace;
        },
        react: (event) => {
            if(event === "tick") {
                var ticker = handlersArray.filter(function(h) {
                    return isOpaqueOnTick(h);
                })[0];
                if(ticker === undefined) {
                    throw new Error("Tried to tick a reactor with no on-tick");
                }
                else {
                    const result = ticker.val.handler.app(init);
                    var newTrace = trace;
                    if(tracing) {
                        newTrace = trace.concat([result]);
                    }
                    return makeReactorRaw(result, handlersArray, tracing, newTrace);
                }
            }
            else {
                throw new Error("Only the literal event \"tick\" is supported");
            }
        },
    };

    return self;
}

function makeBigBangFromDict(shouldPauseAndResume) {
    return function bigBangFromDict(init, dict, tracer, insertNode) {
        var handlers = [];
        function add(k, constr) {
            if(dict.hasOwnProperty(k)) {
                handlers.push(makeOpaque(new constr(dict[k])));
            }
        }
        var title = "Reactor";
        if (dict.hasOwnProperty("title")) {
            title = dict["title"];
        }
        if(dict.hasOwnProperty("on-tick")) {
            if(dict.hasOwnProperty("seconds-per-tick")) {
                var delay = dict["seconds-per-tick"];
                delay = jsnums.toFixnum(delay);
                handlers.push(makeOpaque(new OnTick(dict["on-tick"], delay * 1000)));
            }
            else {
                handlers.push(makeOpaque(new OnTick(dict["on-tick"], DEFAULT_TICK_DELAY * 1000)));
            }
        }
        add("on-mouse", OnMouse);
        add("on-key", OnKey);
        add("to-draw", ToDraw);
        add("stop-when", StopWhen);
        add("close-when-stop", CloseWhenStop);

        if (shouldPauseAndResume) {
            return bigBang(init, handlers, tracer, title);
        }

        return bigBangNoPauseResume(init, handlers, tracer, title, insertNode);
    };
}

var bigBangFromDict = makeBigBangFromDict(true);
var bigBangFromDictNoPauseResume = makeBigBangFromDict(false);

var makeBigBang = function(shouldPauseAndResume) {
    return function(initW, handlers, tracer, title, insertNode) {
        var closeBigBangWindow = null;
        var outerToplevelNode = document.createElement('span');
        outerToplevelNode.style.padding = '0px';
        // TODO(joe): This obviously can't stay
        // if(!runtime.hasParam("current-animation-port")) {
        if (insertNode !== undefined) {
            insertNode(outerToplevelNode);
        } else {
            document.body.appendChild(outerToplevelNode);
        }
        // } else {
        //     runtime.getParam("current-animation-port")(
        //         outerToplevelNode,
        //         title,
        //         function(closeWindow) {
        //             closeBigBangWindow = closeWindow;
        //         }
        //     );
        // }

        var toplevelNode = document.createElement('span');
        toplevelNode.style.padding = '0px';
        outerToplevelNode.appendChild(toplevelNode);
        outerToplevelNode.tabindex = 1;
        outerToplevelNode.focus();

        var configs = [];
        var isOutputConfigSeen = false;
        var closeWhenStop = false;

        for (var i = 0 ; i < handlers.length; i++) {
            if (isOpaqueCloseWhenStopConfig(handlers[i])) {
                closeWhenStop = handlers[i].val.isClose;
            } else if (isOpaqueWorldConfigOption(handlers[i])) {
                configs.push(handlers[i].val.toRawHandler(toplevelNode));
            }
            else {
                configs.push(handlers[i]);
            }
            if (isOpaqueOutputConfig(handlers[i])) { isOutputConfigSeen = true; }
        }

        // If we haven't seen an onDraw function, use the default one.
        if (! isOutputConfigSeen) {
            configs.push(new DefaultDrawingOutput().toRawHandler(toplevelNode));
        }


        if (shouldPauseAndResume) {
            return anchorRuntime.pauseStack(function(restarter) {
                rawJsworld.bigBang(
                    toplevelNode,
                    initW,
                    configs,
                    {},
                    function(finalWorldValue) {
                        restarter.resume(finalWorldValue);
                    },
                    function(err) {
                        restarter.error(err);
                    },
                    {
                        closeWhenStop: closeWhenStop,
                        closeBigBangWindow: closeBigBangWindow,
                        tracer: tracer
                    });
            });
        } else {
            return rawJsworld.bigBang(
                toplevelNode,
                initW,
                configs,
                {},
                function(_) {
                    return;
                },
                function(_) {
                    return;
                },
                {
                    closeWhenStop: closeWhenStop,
                    closeBigBangWindow: closeBigBangWindow,
                    tracer: tracer
                });
        }
    };
};

var bigBang = makeBigBang(true);
var bigBangNoPauseResume = makeBigBang(false);





//////////////////////////////////////////////////////////////////////

// Every world configuration function (on-tick, stop-when, ...)
// produces a WorldConfigOption instance.
var WorldConfigOption = function(name) {
    this.name = name;
};

WorldConfigOption.prototype.configure = function(config) {
    throw new Error('unimplemented WorldConfigOption');
};


WorldConfigOption.prototype.toDomNode = function(params) {
    var span = document.createElement('span');
    span.appendChild(document.createTextNode("(" + this.name + " ...)"));
    return span;
};

WorldConfigOption.prototype.toWrittenString = function(cache) {
    return "(" + this.name + " ...)";
};

WorldConfigOption.prototype.toDisplayedString = function(cache) {
    return "(" + this.name + " ...)";
};

var isWorldConfigOption = function(v) { return v instanceof WorldConfigOption; };

//////////////////////////////////////////////////////////////////////




// adaptWorldFunction: Racket-function -> World-CPS
// Takes a pyret function and converts it to the CPS-style function
// that our world implementation expects.
// NOTE(joe):  This expects there to be no active run for runtime
// (it should be paused).  The run gets paused by pauseStack() in the
// call to bigBang, so these runs will all be fresh
var adaptWorldFunction = function(worldFunction) {
    return function() {
        // Consumes any number of arguments.
        var success = arguments[arguments.length - 1];
        var pyretArgs = [].slice.call(arguments, 0, arguments.length - 1);
        const result = worldFunction.apply(null, pyretArgs);
        success(result);
    };
};

//////////////////////////////////////////////////////////////////////

// OnTick: racket-function javascript-float -> handler
var OnTick = function(handler, aDelay) {
    WorldConfigOption.call(this, 'on-tick');
    this.handler = handler;
    this.delay = aDelay;
};

OnTick.prototype = Object.create(WorldConfigOption.prototype);

OnTick.prototype.toRawHandler = function(toplevelNode) {
    var that = this;
    var worldFunction = adaptWorldFunction(that.handler);
    return rawJsworld.on_tick(this.delay, worldFunction);
};


//////////////////////////////////////////////////////////////////////
var OnKey = function(handler) {
    WorldConfigOption.call(this, 'on-key');
    this.handler = handler;
}

OnKey.prototype = Object.create(WorldConfigOption.prototype);

OnKey.prototype.toRawHandler = function(toplevelNode) {
    var that = this;
    var worldFunction = adaptWorldFunction(that.handler);
    return rawJsworld.on_key(
        function(w, e, success) {
            worldFunction(w, getKeyCodeName(e), success);
        });
};


var getKeyCodeName = function(e) {
    var code = e.charCode || e.keyCode;
    var keyname;
    switch(code) {
    case 8: keyname = "backspace"; break;
    case 9: keyname = "tab"; break;
    case 13: keyname = "enter"; break;
    case 16: keyname = "shift"; break;
    case 17: keyname = "control"; break;
    case 19: keyname = "pause"; break;
    case 27: keyname = "escape"; break;
    case 33: keyname = "prior"; break;
    case 34: keyname = "next"; break;
    case 35: keyname = "end"; break;
    case 36: keyname = "home"; break;
    case 37: keyname = "left"; break;
    case 38: keyname = "up"; break;
    case 39: keyname = "right"; break;
    case 40: keyname = "down"; break;
    case 42: keyname = "print"; break;
    case 45: keyname = "insert"; break;
    case 46: keyname = "delete"; break;
    case 106: keyname = "*"; break;
    case 107: keyname = "+"; break;
    case 109: keyname = "-"; break;
    case 110: keyname = "."; break;
    case 111: keyname = "/"; break;
    case 144: keyname = "numlock"; break;
    case 145: keyname = "scroll"; break;
    case 186: keyname = ";"; break;
    case 187: keyname = "="; break;
    case 188: keyname = ","; break;
    case 189: keyname = "-"; break;
    case 190: keyname = "."; break;
    case 191: keyname = "/"; break;
    case 192: keyname = "`"; break;
    case 219: keyname = "["; break;
    case 220: keyname = "\\"; break;
    case 221: keyname = "]"; break;
    case 222: keyname = "'"; break;
    default:
        if (code >= 96 && code <= 105) {
            keyname = (code - 96).toString();
        } else if (code >= 112 && code <= 123) {
            keyname = "f" + (code - 111);
        } else {
            keyname = String.fromCharCode(code).toLowerCase();
        }
        break;
    }
    return keyname;
}
//////////////////////////////////////////////////////////////////////





var OnMouse = function(handler) {
    WorldConfigOption.call(this, 'on-mouse');
    this.handler = handler;
}

OnMouse.prototype = Object.create(WorldConfigOption.prototype);

OnMouse.prototype.toRawHandler = function(toplevelNode) {
    var that = this;
    var worldFunction = adaptWorldFunction(that.handler);
    return rawJsworld.on_mouse(
        function(w, x, y, type, success) {
            worldFunction(w, x, y, type, success);
        });
};








var OutputConfig = function() {}
OutputConfig.prototype = Object.create(WorldConfigOption.prototype);
var isOutputConfig = function(v) { return v instanceof OutputConfig; };
var isOpaqueOutputConfig = function(v) {
    return isOpaque(v) && isOutputConfig(v.val);
}





// // ToDraw

var ToDraw = function(handler) {
    WorldConfigOption.call(this, 'to-draw');
    this.handler = handler;
};

ToDraw.prototype = Object.create(OutputConfig.prototype);

ToDraw.prototype.toRawHandler = function(toplevelNode) {
    var that = this;
    var reusableCanvas;
    var reusableCanvasNode;
    var adaptedWorldFunction = adaptWorldFunction(this.handler);

    var worldFunction = function(world, success) {

        adaptedWorldFunction(
            world,
            function(v) {
                // fixme: once jsworld supports fail continuations, use them
                // to check the status of the scene object and make sure it's an
                // image.

                var checkImagePred = function(val) {
                    return isImage(val);
                };
                var checkImageType = makeCheckType(checkImagePred, "Image");
                checkImageType(v);

                var theImage = v;
                var width = theImage.getWidth();
                var height = theImage.getHeight();

                if (! reusableCanvas) {
                    reusableCanvas = imageLibrary.makeCanvas(width, height);
                    // Note: the canvas object may itself manage objects,
                    // as in the case of an excanvas.  In that case, we must make
                    // sure jsworld doesn't try to disrupt its contents!
                    reusableCanvas.jsworldOpaque = true;
                    reusableCanvasNode = rawJsworld.node_to_tree(reusableCanvas);
                }
                if (reusableCanvas.width !== width) {
                    reusableCanvas.width = width;
                }
                if (reusableCanvas.height !== height) {
                    reusableCanvas.height = height;
                }
                var ctx = reusableCanvas.getContext("2d");
                ctx.save();
                ctx.fillStyle = "rgba(255,255,255,1)";
                ctx.fillRect(0, 0, width, height);
                ctx.restore();
                theImage.render(ctx, 0, 0);
                success([toplevelNode, reusableCanvasNode]);
            });
    };

    var cssFunction = function(w, k) {
        if (reusableCanvas) {
            k([[reusableCanvas,
                ["padding", "0px"],
                ["width", reusableCanvas.width + "px"],
                ["height", reusableCanvas.height + "px"]]]);
        } else {
            k([]);
        }
    }

    return rawJsworld.on_draw(worldFunction, cssFunction);
};







var DefaultDrawingOutput = function() {
    WorldConfigOption.call(this, 'to-draw');
};

DefaultDrawingOutput.prototype = Object.create(WorldConfigOption.prototype);

DefaultDrawingOutput.prototype.toRawHandler = function(toplevelNode) {
    var that = this;
    var worldFunction = function(world, success) {
        var textNode = document.createElement('pre');
        const str = JSON.stringify(world);
        textNode.innerText = str;
        success([toplevelNode,
                 rawJsworld.node_to_tree(textNode)]);
        return;
    };
    var cssFunction = function(w, success) { success([]); }
    return rawJsworld.on_draw(worldFunction, cssFunction);
};




//////////////////////////////////////////////////////////////////////

var CloseWhenStop = function(isClose) {
    WorldConfigOption.call(this, 'close-when-stop');
    this.isClose = isClose === true;
};

CloseWhenStop.prototype = Object.create(WorldConfigOption.prototype);

var isCloseWhenStopConfig = function(v) { return v instanceof CloseWhenStop; };
var isOpaqueCloseWhenStopConfig = function(v) {
    return isOpaque(v) && isCloseWhenStopConfig(v.val);
}

var StopWhen = function(handler) {
    WorldConfigOption.call(this, 'stop-when');
    this.handler = handler;
};

StopWhen.prototype = Object.create(WorldConfigOption.prototype);

StopWhen.prototype.toRawHandler = function(toplevelNode) {
    var that = this;
    var worldFunction = adaptWorldFunction(that.handler);
    return rawJsworld.stop_when(worldFunction);
};

var checkHandler = makeCheckType(isOpaqueWorldConfigOption, "WorldConfigOption");
//////////////////////////////////////////////////////////////////////


// The default tick delay is 28 times a second.
var DEFAULT_TICK_DELAY = 1/28;

module.exports = {
    "reactor": makeReactor,
    "big-bang": (init, handlers) => {
        var arr = handlers;
        var initialWorldValue = init;
        arr.map(function(h) { checkHandler(h); });
        return bigBang(initialWorldValue, arr, null, 'big-bang');
    },
    "on-tick": (handler) => {
        return makeOpaque(new OnTick(handler, Math.floor(DEFAULT_TICK_DELAY * 1000)));
    },
    "on-tick-n": (handler, n) => {
        var fixN = jsnums.toFixnum(n);
        return makeOpaque(new OnTick(handler, fixN * 1000));
    },
    "to-draw": (drawer) => {
        return makeOpaque(new ToDraw(drawer));
    },
    "stop-when": (stopper) => {
        return makeOpaque(new StopWhen(stopper));
    },
    "close-when-stop": (isClose) => {
        return makeOpaque(new CloseWhenStop(isClose));
    },
    "on-key": (onKey) => {
        return makeOpaque(new OnKey(onKey));
    },
    "on-mouse": (onMouse) => {
        return makeOpaque(new OnMouse(onMouse));
    },
    "is-world-config": (v) => {
        if(!isOpaque(v)) { return false; }
        return isWorldConfigOption(v.val);
    },
    "is-key-equal": (key1, key2) => {
        return key1.toString().toLowerCase() === key2.toString().toLowerCase();
    },
    $WorldConfigOption: WorldConfigOption,
    $adaptWorldFunction: adaptWorldFunction,
    $bigBangFromDict: bigBangFromDict,
    $bigBangFromDictNoPauseResume: bigBangFromDictNoPauseResume,
    $shutdown: rawJsworld.shutdown,
};
