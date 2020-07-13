({
  requires: [
    { "import-type": "builtin", "name": "image-lib" },
    { "import-type": "builtin", "name": "world-lib" },
    { "import-type": "builtin", "name": "valueskeleton" }
  ],
  nativeRequires: ["pyret-base/js/js-numbers"],
  provides: {
    shorthands: {
      "WCOofA": ["tyapp", ["local", "WorldConfigOption"], [["tid", "a"]]],
      "Image": { tag: "name",
                 origin: { "import-type": "uri", uri: "builtin://image" },
                 name: "Image" }
    },
    values: {
      "reactor": ["forall", ["a"], ["arrow", [["tid", "a"], ["List", "WCOofA"]], "Any"]],
      "big-bang": ["forall", ["a"], ["arrow", [["tid", "a"], ["List", "WCOofA"]], ["tid", "a"]]],
      "on-tick": ["forall", ["a"],
          ["arrow",
             [["arrow", [ ["tid", "a"] ], ["tid", "a"]]],
             "WCOofA"]],
      "on-tick-n": ["forall", ["a"],
          ["arrow",
             [["arrow", [ ["tid", "a"], "Number" ], ["tid", "a"]]],
             "WCOofA"]],
      "on-mouse": ["forall", ["a"],
          ["arrow",
             [["arrow", [ ["tid", "a"], "Number", "Number", "String" ], ["tid", "a"]]],
             "WCOofA"]],
      "on-key": ["forall", ["a"],
          ["arrow",
             [["arrow", [ ["tid", "a"], "String" ], ["tid", "a"]]],
             "WCOofA"]],
      "to-draw": ["forall", ["a"],
          ["arrow",
             [["arrow", [ ["tid", "a"] ], "Image"]],
             "WCOofA"]],
      "stop-when": ["forall", ["a"],
          ["arrow",
             [["arrow", [ ["tid", "a"] ], "Boolean"]],
             "WCOofA"]],
      "close-when-stop": ["forall", ["a"],
          ["arrow",
             ["Boolean"],
             "WCOofA"]],
      "is-world-config": ["arrow", [ "Any" ], "Boolean"],
      "is-key-equal": ["arrow", [ "String", "String" ], "Boolean"]
    },
    aliases: {},
    datatypes: {
      "WorldConfigOption": ["data", "WorldConfigOption", ["a"], [], {}]
    }
  },
  theModule: function(runtime, namespace, uri, imageLibrary, rawJsworld, VSlib, jsnums) {
    var isImage = imageLibrary.isImage;
    var VS = runtime.getField(VSlib, "values");

    //////////////////////////////////////////////////////////////////////

    // An Opaque is a Pyret concept for a value wrapping a hidden
    // implementation.  Check that a value is one of these, and internally is
    // a WorldConfigOption
    var isOpaqueWorldConfigOption = function(v) {
      return runtime.isOpaque(v) && isWorldConfigOption(v.val);
    }
    var isOpaqueOnTick = function(v) {
      return runtime.isOpaque(v) && (v.val instanceof OnTick);
    }
    var isOpaqueToDraw = function(v) {
      return runtime.isOpaque(v) && (v.val instanceof ToDraw);
    }

    var makeReactor = function(init, handlers) {
      runtime.ffi.checkArity(2, arguments, "reactor", false);
      runtime.checkList(handlers);
      var arr = runtime.ffi.toArray(handlers);
      var initialWorldValue = init;
      arr.map(function(h) { checkHandler(h); });
      return makeReactorRaw(init, arr, false, []);
    }
    var makeReactorRaw = function(init, handlersArray, tracing, trace) {
      return runtime.makeObject({
        "get-value": runtime.makeMethod0(function(self) {
          return init;
        }),
        "draw": runtime.makeMethod0(function(self) {
          var drawer = handlersArray.filter(function(h) {
            return isOpaqueToDraw(h);
          })[0];
          if(drawer === undefined) {
            runtime.throwMessageException("Tried to draw() a reactor with no to-draw");
          }
          return drawer.val.handler.app(init);
        }),
        interact: runtime.makeMethod0(function(self) {
          var thisInteractTrace = [];
          var tracer = null;
          if(tracing) {
            tracer = function(newVal, oldVal, k) {
              thisInteractTrace.push(newVal);
              k();
            };
          }
          return runtime.safeCall(function() {
            return bigBang(init, handlersArray, tracer);
          }, function(newVal) {
            return makeReactorRaw(newVal, handlersArray, tracing, trace.concat(thisInteractTrace));
          }, "interact");
        }),
        "start-trace": runtime.makeMethod0(function(self) {
          return makeReactorRaw(init, handlersArray, true, []);
        }),
        "stop-trace": runtime.makeMethod0(function(self) {
          return makeReactorRaw(init, handlersArray, false, []);
        }),
        "get-trace": runtime.makeMethod0(function(self) {
          return runtime.ffi.makeList(trace);
        }),
        react: runtime.makeMethod1(function(self, event) {
          if(event === "tick") {
            var ticker = handlersArray.filter(function(h) {
              return isOpaqueOnTick(h);
            })[0];
            if(ticker === undefined) {
              runtime.throwMessageException("Tried to tick a reactor with no on-tick");
            }
            else {
              return runtime.safeCall(function() {
                return ticker.val.handler.app(init);
              }, function(result) {
                var newTrace = trace;
                if(tracing) {
                  newTrace = trace.concat([result]);
                }
                return makeReactorRaw(result, handlersArray, tracing, newTrace);
              }, "react:on-tick");
            }
          }
          else {
            runtime.throwMessageException("Only the literal event \"tick\" is supported");
          }
        }),
        _output: runtime.makeMethod0(function(self) {
          return runtime.getField(VS, "vs-constr").app(
            "reactor",
            runtime.ffi.makeList([
              runtime.getField(VS, "vs-value").app(init),
              runtime.getField(VS, "vs-value").app(runtime.ffi.makeList(trace))]));
        })
      });
    }

    function bigBangFromDict(init, dict, tracer) {
      var handlers = [];
      function add(k, constr) {
        if(dict.hasOwnProperty(k)) {
          handlers.push(runtime.makeOpaque(new constr(dict[k])));
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
          handlers.push(runtime.makeOpaque(new OnTick(dict["on-tick"], delay * 1000)));
        }
        else {
          handlers.push(runtime.makeOpaque(new OnTick(dict["on-tick"], DEFAULT_TICK_DELAY * 1000)));
        }
      }
      add("on-mouse", OnMouse);
      add("on-key", OnKey);
      add("to-draw", ToDraw);
      add("stop-when", StopWhen);
      add("close-when-stop", CloseWhenStop);

      return bigBang(init, handlers, tracer, title);
    }

    var bigBang = function(initW, handlers, tracer, title) {
      var closeBigBangWindow = null;
      var outerToplevelNode = jQuery('<span/>').css('padding', '0px').get(0);
      // TODO(joe): This obviously can't stay
      if(!runtime.hasParam("current-animation-port")) {
        document.body.appendChild(outerToplevelNode);
      } else {
        runtime.getParam("current-animation-port")(
          outerToplevelNode,
          title,
          function(closeWindow) {
            closeBigBangWindow = closeWindow;
          }
        );
      }

      var toplevelNode = jQuery('<span/>')
          .css('padding', '0px')
          .appendTo(outerToplevelNode)
          .attr('tabindex', 1)
          .focus()
          .get(0);

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


      return runtime.pauseStack(function(restarter) {
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
    // Takes a pyret function and converts it to the CPS-style function
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
          // NOTE(joe): adding safecall here to get some meaningful caller frame
          // so error messages know where the call is coming from
          return runtime.safeCall(function() {
            return worldFunction.app.apply(null, pyretArgs);
          }, function(result) {
            return result;
          }, "big-bang");
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

              var checkImagePred = function(val) {
                return runtime.isOpaque(val) && isImage(val.val);
              };
              var checkImageType = runtime.makeCheckType(checkImagePred, "Image");
              checkImageType(v);

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
        var textNode = jQuery("<pre>");
        return runtime.safeCall(function() {
          return runtime.toReprJS(world, runtime.ReprMethods._torepr);
        }, function(str) {
          textNode.text(str);
          success([toplevelNode,
                   rawJsworld.node_to_tree(textNode[0])]);
        }, "default-drawing:toRepr");
      };
      var cssFunction = function(w, success) { success([]); }
      return rawJsworld.on_draw(worldFunction, cssFunction);
    };




    //////////////////////////////////////////////////////////////////////

    var CloseWhenStop = function(isClose) {
      WorldConfigOption.call(this, 'close-when-stop');
      this.isClose = runtime.isPyretTrue(isClose);
    };

    CloseWhenStop.prototype = Object.create(WorldConfigOption.prototype);

    var isCloseWhenStopConfig = function(v) { return v instanceof CloseWhenStop; };
    var isOpaqueCloseWhenStopConfig = function(v) {
      return runtime.isOpaque(v) && isCloseWhenStopConfig(v.val);
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

    var checkHandler = runtime.makeCheckType(isOpaqueWorldConfigOption, "WorldConfigOption");
    //////////////////////////////////////////////////////////////////////


    // The default tick delay is 28 times a second.
    var DEFAULT_TICK_DELAY = 1/28;

    var makeObject = runtime.makeObject;
    var makeFunction = runtime.makeFunction;

    return runtime.makeModuleReturn(
      {
        "reactor": makeFunction(makeReactor, "reactor"),
        "big-bang": makeFunction(function(init, handlers) {
          runtime.ffi.checkArity(2, arguments, "big-bang", false);
          runtime.checkList(handlers);
          var arr = runtime.ffi.toArray(handlers);
          var initialWorldValue = init;
          arr.map(function(h) { checkHandler(h); });
          return bigBang(initialWorldValue, arr, null, 'big-bang');
          runtime.ffi.throwMessageException("Internal error in bigBang: stack not properly paused and stored.");
        }, "big-bang"),
        "on-tick": makeFunction(function(handler) {
          runtime.ffi.checkArity(1, arguments, "on-tick", false);
          runtime.checkFunction(handler);
          return runtime.makeOpaque(new OnTick(handler, Math.floor(DEFAULT_TICK_DELAY * 1000)));
        }),
        "on-tick-n": makeFunction(function(handler, n) {
          runtime.ffi.checkArity(2, arguments, "on-tick-n", false);
          runtime.checkFunction(handler);
          runtime.checkNumber(n);
          var fixN = jsnums.toFixnum(n);
          return runtime.makeOpaque(new OnTick(handler, fixN * 1000));
        }),
        "to-draw": makeFunction(function(drawer) {
          runtime.ffi.checkArity(1, arguments, "to-draw", false);
          runtime.checkFunction(drawer);
          return runtime.makeOpaque(new ToDraw(drawer));
        }),
        "stop-when": makeFunction(function(stopper) {
          runtime.ffi.checkArity(1, arguments, "stop-when", false);
          runtime.checkFunction(stopper);
          return runtime.makeOpaque(new StopWhen(stopper));
        }),
        "close-when-stop": makeFunction(function(isClose) {
          runtime.ffi.checkArity(1, arguments, "close-when-stop", false);
          runtime.checkBoolean(isClose);
          return runtime.makeOpaque(new CloseWhenStop(isClose));
        }),
        "on-key": makeFunction(function(onKey) {
          runtime.ffi.checkArity(1, arguments, "on-key", false);
          runtime.checkFunction(onKey);
          return runtime.makeOpaque(new OnKey(onKey));
        }),
        "on-mouse": makeFunction(function(onMouse) {
          runtime.ffi.checkArity(1, arguments, "on-mouse", false);
          runtime.checkFunction(onMouse);
          return runtime.makeOpaque(new OnMouse(onMouse));
        }),
        "is-world-config": makeFunction(function(v) {
          runtime.ffi.checkArity(1, arguments, "is-world-config", false);
          if(!runtime.isOpaque(v)) { return runtime.pyretFalse; }
          return runtime.makeBoolean(isWorldConfigOption(v.val));
        }),
        "is-key-equal": makeFunction(function(key1, key2) {
          runtime.ffi.checkArity(2, arguments, "is-key-equal", false);
          runtime.checkString(key1);
          runtime.checkString(key2);
          return key1.toString().toLowerCase() === key2.toString().toLowerCase();
        })
      },
      {},
      {
        WorldConfigOption: WorldConfigOption,
        adaptWorldFunction: adaptWorldFunction,
        bigBangFromDict: bigBangFromDict
      }
    );
  }
})
