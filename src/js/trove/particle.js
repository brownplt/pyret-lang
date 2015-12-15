define(["js/runtime-util", "js/ffi-helpers", "trove/json", "trove/string-dict", "trove/world", "trove/world-lib", "trove/particle-shim-structs"], function(util, ffiLib, json, sDictLib, world, worldLib, pShimStruct) {

  return util.definePyretModule(
    "particle",
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
      return runtime.loadJSModules(namespace, [worldLib, ffiLib], function(rawJsworld, ffi) {
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
                if(ffi.isSome(v)) {
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
          
          var configCore = function(configs, options) {
            var config_str = configs.map(function(c){
              return runtime.getField(c, "_shim-convert").app();}).join("");
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
          
          var sd_to_js = function(sd) {
            var ret = {};
            var arr = ffi.toArray(runtime.getField(sd, "keys-list").app());
            for(var i in arr) {
              var k = arr[i];
              var v = runtime.getField(sd, "get-value").app(k);
              if(typeof(v) === "string") {
                ret[k] = v;
              } else if(v === runtime.pyretTrue) {
                ret[k] = true;
              } else if(v === runtime.pyretFalse) {
                ret[k] = false;
              } else {
                throw new Error('unimplemented value conversion for StringDict');
              }
            }
            return ret;
          };

          var checkStringDict = runtime.getField(sDict, "internal").checkISD;

          return makeObject({
            "provide": makeObject({
              "on-particle": makeFunction(function(onEvent,eName,sd) {
                ffi.checkArity(3, arguments, "on-particle");
                runtime.checkFunction(onEvent);
                runtime.checkString(eName);
                checkStringDict(sd);
                var options = sd_to_js(sd);
                return runtime.makeOpaque(new OnParticle(onEvent,eName,options));
              }),
              "to-particle": makeFunction(function(toEvent,eName,sd) {
                ffi.checkArity(3, arguments, "to-particle");
                runtime.checkFunction(toEvent);
                runtime.checkString(eName);
                checkStringDict(sd);
                var options = sd_to_js(sd);
                return runtime.makeOpaque(new ToParticle(toEvent,eName,options));
              }),
              // direct Particle stream access
              "send-event": makeFunction(function(eName, eData, sd) {
                ffi.checkArity(3, arguments, "send-event");
                runtime.checkString(eName);
                runtime.checkString(eData);
                checkStringDict(sd);
                var options = sd_to_js(sd);
                sendEvent(eName, eData, options);
                return ffi.makeNone();
              }),
              // core configuration
              "coreinfo": runtime.getfield(pStruct_vals, "coreinfo"),
              "nocore": runtime.getfield(pStruct_vals, "nocore"),
              "is-coreinfo": runtime.getfield(pStruct_vals, "is-coreinfo"),
              "is-nocore": runtime.getfield(pStruct_vals, "is-nocore"),
              "configure-core": makeFunction(function(config, sd) {
                ffi.checkArity(2, arguments, "configure-core");
                runtime.checkList(config);
                checkStringDict(sd);
                var options = sd_to_js(sd);
                configCore(ffi.toArray(config), options);
              }),
              "enters": runtime.getField(pStruct_vals, "enters"),
              "exits": runtime.getField(pStruct_vals, "exits"),
              "crosses": runtime.getField(pStruct_vals, "crosses"),
              "poll": runtime.getField(pStructs_vals, "poll"),
              "is-enters": runtime.getField(pStruct_vals, "is-enters"),
              "is-exits": runtime.getField(pStruct_vals, "is-exits"),
              "is-crosses": runtime.getField(pStruct_vals, "is-crosses"),
              "is-poll": runtime.getField(pStructs_vals, "is-poll"),
              "write": runtime.getField(pStruct_vals, "write"),
              "digital-read": runtime.getField(pStruct_vals, "digital-read"),
              "analog-read": runtime.getField(pStruct_vals, "analog-read"),
              "is-write": runtime.getField(pStruct_vals, "is-write"),
              "is-digital-read": runtime.getField(pStruct_vals, "is-digital-read"),
              "is-analog-read": runtime.getField(pStruct_vals, "is-analog-read"),
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
