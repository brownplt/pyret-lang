define(["js/runtime-util", "trove/image-lib", "trove/world-lib", "js/ffi-helpers"], function(util, imageLib, worldLib, ffiLib) {

  return util.memoModule("world", function(runtime, namespace) {
    var imageLibrary = imageLib(runtime, namespace);
    var rawJsworld = worldLib(runtime, namespace);
    var ffi = ffiLib(runtime, namespace);
    var isImage = imageLibrary.isImage;

    //////////////////////////////////////////////////////////////////////

    // An Opaque is a Pyret concept for a value wrapping a hidden
    // implementation.  Check that a value is one of these, and internally is
    // a WorlConfigOption
    var isOpaqueWorldConfigOption = function(v) {
      return runtime.isOpaque(v) && isWorldConfigOption(v.val);
    }

    var bigBang = function(initW, handlers) {
        var outerToplevelNode = jQuery('<span/>').css('padding', '0px').get(0);
        // TODO(joe): This obviously can't stay
        if(!runtime.hasParam("current-animation-port")) {
          document.body.appendChild(outerToplevelNode);
        } else {
          runtime.getParam("current-animation-port")(outerToplevelNode);
        }

        var toplevelNode = jQuery('<span/>').css('padding', '0px').appendTo(outerToplevelNode).get(0);

        var configs = [];
        var isOutputConfigSeen = false;

        for (var i = 0 ; i < handlers.length; i++) {
            if (isOpaqueWorldConfigOption(handlers[i])) {
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


        runtime.pauseStack(function(restarter) {
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
            });
        });
    };





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
    // Takes a racket function and converts it to the CPS-style function
    // that our world implementation expects.
    // NOTE(joe):  This expects there to be no active run for runtime
    // (it should be paused).  The run gets paused by pauseStack() in the
    // call to bigBang, so these runs will all be fresh
    var adaptWorldFunction = function(worldFunction) {
        return function() {
            // Consumes any number of arguments.
            var success = arguments[arguments.length - 1];
            // NOTE(joe): don't move this line down, it's *these* args, not
            // any other nested function's args
            var pyretArgs = [].slice.call(arguments, 0, arguments.length - 1);
            runtime.run(function(_, _) {
                return worldFunction.app.apply(null, pyretArgs);
              }, runtime.namespace,
              { sync: false },
              function(result) {
                if(runtime.isSuccessResult(result)) {
                  success(result.result);
                }
                else {
                  return rawJsworld.shutdown({errorShutdown: result.exn});
                }
              });
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
        case 46: keyname = String.fromCharCode(127); break;
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
      return runtime.isOpaque(v) && isOutputConfig(v.val);
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

                    
                    if (runtime.isOpaque(v) && isImage(v.val) ) {
            var theImage = v.val;
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
            theImage.render(ctx, 0, 0);
            success([toplevelNode, reusableCanvasNode]);
        } else {
            // TODO(joe): Maybe torepr below
            success([toplevelNode, rawJsworld.node_to_tree(String(v))]);
        }
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
            success([toplevelNode,
                     rawJsworld.node_to_tree(String(world))]);
        };
        var cssFunction = function(w, success) { success([]); }
        return rawJsworld.on_draw(worldFunction, cssFunction);
    };




    //////////////////////////////////////////////////////////////////////



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

    var checkHandler = runtime.makeCheckType(isOpaqueWorldConfigOption, "WorldConfigOption");



    // The default tick delay is 28 times a second.
    var DEFAULT_TICK_DELAY = 1/28;

    var makeObject = runtime.makeObject;
    var makeFunction = runtime.makeFunction;

    return makeObject({
      "provide": makeObject({
        "big-bang": makeFunction(function(init, handlers) {
          ffi.checkArity(2, arguments, "big-bang");
          var arr = ffi.toArray(handlers);
          var initialWorldValue = init;
          arr.map(function(h) { checkHandler(h); });
          bigBang(initialWorldValue, arr);
          ffi.throwMessageException("Internal error in bigBang: stack not properly paused and stored.");
        }),
        "on-tick": makeFunction(function(handler) {
          ffi.checkArity(1, arguments, "on-tick");
          runtime.checkFunction(handler);
          return runtime.makeOpaque(new OnTick(handler, Math.floor(DEFAULT_TICK_DELAY * 1000)));
        }),
        "on-tick-n": makeFunction(function(handler, n) {
          ffi.checkArity(2, arguments, "on-tick-n");
          runtime.checkFunction(handler);
          runtime.checkNumber(n);
          var fixN = typeof n === "number" ? fixN : n.toFixnum();
          return runtime.makeOpaque(new OnTick(handler, fixN * 1000));
        }),
        "to-draw": makeFunction(function(drawer) {
          ffi.checkArity(1, arguments, "to-draw");
          runtime.checkFunction(drawer);
          return runtime.makeOpaque(new ToDraw(drawer));
        }),
        "stop-when": makeFunction(function(stopper) {
          ffi.checkArity(1, arguments, "stop-when");
          runtime.checkFunction(stopper);
          return runtime.makeOpaque(new StopWhen(stopper));
        }),
        "on-key": makeFunction(function(onKey) {
          ffi.checkArity(1, arguments, "on-key");
          runtime.checkFunction(onKey);
          return runtime.makeOpaque(new OnKey(onKey));
        }),
        "on-mouse": makeFunction(function(onMouse) {
          ffi.checkArity(1, arguments, "on-mouse");
          runtime.checkFunction(onMouse);
          return runtime.makeOpaque(new OnMouse(onMouse));
        }),
        "is-world-config": makeFunction(function(v) {
          ffi.checkArity(1, arguments, "is-world-config");
          if(!runtime.isOpaque(v)) { return runtime.pyretFalse; }
          return runtime.makeBoolean(isWorldConfigOption(v.val));
        }),
        "is-key-equal": makeFunction(function(key1, key2) {
          ffi.checkArity(2, arguments, "is-key-equal");
          runtime.checkString(key1);
          runtime.checkString(key2);
          return key1.toString().toLowerCase() === key2.toString().toLowerCase();
        })
      })
    });
  });
});
