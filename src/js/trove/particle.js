define(["js/runtime-util", "trove/json", "trove/string-dict", "trove/world", "trove/world-lib", "trove/particle-shim-structs"], function(util, json, sDictLib, world, worldLib, pShimStruct) {

  return util.definePyretModule(
    "particle",
    [],
    [],
    {
      values: {
      },
      aliases: {
      },
      datatypes: {
      }
    },
    function(runtime, namespace) {
      return runtime.loadJSModules(namespace, [worldLib], function(rawJsworld) {
        return runtime.loadModulesNew(namespace, [world, json, sDictLib, pShimStruct], function(pWorld, pJSON, sDict, pStruct) {
          var WorldConfigOption = runtime.getField(pWorld, "internal").WorldConfigOption;
          var adaptWorldFunction = runtime.getField(pWorld, "internal").adaptWorldFunction;
          var p_read_json = runtime.getField(runtime.getField(pJSON, "values"),"read-json")
          var read_json = function(s) { return p_read_json.app(s) }
          var serialize = function(j) { return runtime.getField(j, "serialize").app() }

          var OnParticle = function(handler, name, options) {
            WorldConfigOption.call(this, 'on-particle');
            this.handler = handler;
            this.event = name;
            this.options = options;
          };

          OnParticle.prototype = Object.create(WorldConfigOption.prototype);

          OnParticle.prototype.toRawHandler = function(toplevelNode) {
            var that = this;
            var handler = adaptWorldFunction(that.handler);
            var eName = that.event;
            var options = that.options;
            var uri = "";
            if(typeof options.host == "string") {
              uri = "https://" + host
            } else {
              uri = "https://api.particle.io"
            }
            if(typeof options.core == "string") {
              uri += "/v1/devices/" + options.core + "/events/"
            } else {
              uri += "/v1/devices/events/"
            }
            uri += "?access_token=" + options.acc;
            return function() {
              var evtSource;
              return {
                onRegister: function(top) {
                  evtSource = new EventSource(uri);
                  evtSource.addEventListener(eName,
                                             function(e) {
                                               data = read_json(JSON.parse(e.data).data)
                                               rawJsworld.change_world(function(w,k) {
                                                 handler(w, data, k);
                                               }, rawJsworld.doNothing)});
                },
                onUnregister: function(top) {
                  evtSource.close();
                }
              };
            };
          };


          //////////////////////////////////////////////////////////////////////

          var sendEvent = function(ename, data, options) {
            var xhr = new XMLHttpRequest();
            var uri = "";
            var contents = "";
            if(typeof options.host == "string") {
              uri = "https://" + host
            } else {
              uri = "https://api.particle.io"
            }
            uri += "/v1/devices/events/";
            if(typeof options.raw != 'undefined' && options.raw) {
              contents = "&name=" + ename + "&data=" +
                data;
            } else if (typeof options.core == "string") {
              contents = "&name=" + options.core + "_event&data=" +
                ename + ":" + data;
            } else {
              contents = "&name=_event&data=" + ename + ":" + data;
            }
            contents = "access_token=" + options.acc +
              contents + "&private=true&ttl=60";
            xhr.open("POST", uri);
            xhr.setRequestHeader("Content-type","application/x-www-form-urlencoded");
            xhr.send(contents);
          }

          var ToParticle = function(handler, ename, options) {
            WorldConfigOption.call(this, 'to-particle');
            this.handler = handler;
            this.event = ename;
            this.options = options;
          };
          
          ToParticle.prototype = Object.create(WorldConfigOption.prototype);
          
          ToParticle.prototype.toRawHandler = function(toplevelNode) {
            var that = this;
            var worldFunction = adaptWorldFunction(that.handler);
            var options = that.options;
            var eventGen = function(w, k) {
              worldFunction(w, function(v) {
                if(runtime.ffi.isSome(v)) {
                  sendEvent(that.event,
                            serialize(runtime.getField(v, "value")),
                            options);
                }
                k(w);
              });
            };
            return rawJsworld.on_world_change(eventGen);
          };
          

          //////////////////////////////////////////////////////////////////////
          
          var configCore = function(config, options) {
            var config_str = runtime.getField(config, "_shim-convert").app();
            var ename = ""
            if(typeof options.core == "string") {
              ename = options.core + "_config"
            } else {
              ename = "_config"
            }
            options.raw = true;
            sendEvent(ename, config_str, options);
          }
          
          var pStruct_vals = runtime.getField(pStruct, "values");
          
          //////////////////////////////////////////////////////////////////////
          var makeObject = runtime.makeObject;
          var makeFunction = runtime.makeFunction;

          var pyIsCore = runtime.getField(pStruct_vals, "is-core");
          var pyIsNoCore = runtime.getField(pStruct_vals, "is-no-core");

          var pyIsWrite = runtime.getField(pStruct_vals, "is-write");
          var pyIsDigRead = runtime.getField(pStruct_vals, "is-digital-read");
          var pyIsAnaRead = runtime.getField(pStruct_vals, "is-analog-read");

          var pyIsEvent = runtime.getField(pStruct_vals, "is-event");

          var pyIsConfig = runtime.getField(pStruct_vals, "is-config");

          var pyConfig = runtime.getField(pStruct_vals, "config");

          var checkCoreInfoType =
              runtime.makeCheckType(function(v) {
                return ((pyIsCore.app(v) == runtime.pyretTrue) ||
                        (pyIsNoCore.app(v) == runtime.pyretTrue)); },
                                    "CoreInfo");
          var checkCoreObj =
              runtime.makeCheckType(function(v) {
                return (pyIsCore.app(v) == runtime.pyretTrue); },
                                    "core");
          var checkPinConfigType =
              runtime.makeCheckType(function(v) {
                return ((pyIsWrite.app(v) == runtime.pyretTrue) ||
                        (pyIsDigRead.app(v) == runtime.pyretTrue) ||
                        (pyIsAnaRead.app(v) == runtime.pyretTrue)); },
                                    "PinConfig");
          var checkEventType =
              runtime.makeCheckType(function(v) {
                return (pyIsEvent.app(v) == runtime.pyretTrue); },
                                    "Event");
          var checkConfigInfoType =
              runtime.makeCheckType(function(v) {
                return (pyIsConfig.app(v) == runtime.pyretTrue); },
                                    "ConfigInfo");
          
          var core_to_options = function(core) {
            if(pyIsNoCore.app(core) == runtime.pyretTrue) {
              return {
                'acc': runtime.getField(core, "access"),
                'raw': true
              };
            } else {
              return {
                'core': runtime.getField(core, "name"),
                'acc': runtime.getField(core, "access"),
                'raw': runtime.getField(core, "shim") == runtime.pyretFalse
              };
            }
          }


          return makeObject({
            "provide": makeObject({
              "on-particle": makeFunction(function(onEvent,config) {
                runtime.ffi.checkArity(2, arguments, "on-particle");
                runtime.checkFunction(onEvent);
                checkConfigInfoType(config);
                core = runtime.getField(config, "core")
                eName = runtime.getField(runtime.getField(config, "pin"),
                                         "event");
                options = core_to_options(core);
                return runtime.makeOpaque(new OnParticle(onEvent,eName,options));
              }),
              "to-particle": makeFunction(function(toEvent,config) {
                runtime.ffi.checkArity(2, arguments, "to-particle");
                runtime.checkFunction(toEvent);
                checkConfigInfoType(config);
                core = runtime.getField(config, "core")
                eName = runtime.getField(runtime.getField(config, "pin"),
                                         "event");
                options = core_to_options(core);
                return runtime.makeOpaque(new ToParticle(toEvent,eName,options));
              }),
              // direct Particle stream access
              "send-event": makeFunction(function(core, event) {
                runtime.ffi.checkArity(2, arguments, "send-event");
                checkCoreInfoType(core);
                checkEventType(event);
                eName = runtime.getField(event, "name");
                eData = runtime.getField(event, "body");
                options = core_to_options(core);
                sendEvent(eName, eData, options);
                return runtime.ffi.makeNone();
              }),
              // core configuration
              "core": runtime.getField(pStruct_vals, "core"),
              "no-core": runtime.getField(pStruct_vals, "no-core"),
              "is-core": pyIsCore,
              "is-no-core": pyIsNoCore,
              "configure-core": makeFunction(function(core, config) {
                runtime.ffi.checkArity(2, arguments, "configure-core");
                checkCoreObj(core);
                checkPinConfigType(config);
                options = core_to_options(core);
                configCore(config, options);
                return pyConfig.app(core, config);
              }),
              "clear-core-config": makeFunction(function(core) {
                runtime.ffi.checkArity(1, arguments, "clear-core-config");
                checkCoreObj(core);
                options = {
                  'acc': runtime.getField(core, "access"),
                  'raw': true
                };
                sendEvent(runtime.getField(core, "name") + "_config", "", options);
                return runtime.ffi.makeNone();
              }),
              "enters": runtime.getField(pStruct_vals, "enters"),
              "exits": runtime.getField(pStruct_vals, "exits"),
              "crosses": runtime.getField(pStruct_vals, "crosses"),
              "poll": runtime.getField(pStruct_vals, "poll"),
              "is-enters": runtime.getField(pStruct_vals, "is-enters"),
              "is-exits": runtime.getField(pStruct_vals, "is-exits"),
              "is-crosses": runtime.getField(pStruct_vals, "is-crosses"),
              "is-poll": runtime.getField(pStruct_vals, "is-poll"),
              "write": runtime.getField(pStruct_vals, "write"),
              "digital-read": runtime.getField(pStruct_vals, "digital-read"),
              "analog-read": runtime.getField(pStruct_vals, "analog-read"),
              "is-write": pyIsWrite,
              "is-digital-read": pyIsDigRead,
              "is-analog-read": pyIsAnaRead,
              "event": runtime.getField(pStruct_vals, "event"),
              "is-event": pyIsEvent,
              "config": runtime.getField(pStruct_vals, "config"),
              "is-config": pyIsConfig,
              "A0": runtime.getField(pStruct_vals, "A0"),
              "A1": runtime.getField(pStruct_vals, "A1"),
              "A2": runtime.getField(pStruct_vals, "A2"),
              "A3": runtime.getField(pStruct_vals, "A3"),
              "A4": runtime.getField(pStruct_vals, "A4"),
              "A5": runtime.getField(pStruct_vals, "A5"),
              "A6": runtime.getField(pStruct_vals, "A6"),
              "A7": runtime.getField(pStruct_vals, "A7"),
              "D0": runtime.getField(pStruct_vals, "D0"),
              "D1": runtime.getField(pStruct_vals, "D1"),
              "D2": runtime.getField(pStruct_vals, "D2"),
              "D3": runtime.getField(pStruct_vals, "D3"),
              "D4": runtime.getField(pStruct_vals, "D4"),
              "D5": runtime.getField(pStruct_vals, "D5"),
              "D6": runtime.getField(pStruct_vals, "D6"),
              "D7": runtime.getField(pStruct_vals, "D7")
            })
          });
        });
      });
    });
});
