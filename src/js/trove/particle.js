define(["js/runtime-util", "js/ffi-helpers", "trove/world", "trove/world-lib", "trove/particle-shim-structs"], function(util, ffiLib, world, worldLib, pShimStruct) {

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
        return runtime.loadModulesNew(namespace, [world, pShimStruct], function(pWorld, pStruct) {
          var WorldConfigOption = runtime.getField(pWorld, "internal").WorldConfigOption;
          var adaptWorldFunction = runtime.getField(pWorld, "internal").adaptWorldFunction;

          var sd_to_js = function(sd) {
            var ret = {};
            var arr = ffi.toArray(runtime.getField(sd, "keys-list").app());
            for(var i in arr) {
              var k = arr[i];
              var v = runtime.getField(sd, "get-value").app(k);
              if(typeof(v) === "string") {
                ret[k] = v;
              } else {
                throw new Error('unimplemented value conversion for StringDict');
              }
            }
            return ret;
          };

          var OnParticle = function(handler, name, sdict) {
            WorldConfigOption.call(this, 'on-particle');
            this.handler = handler;
            this.event = name;
            this.options = sd_to_js(sdict);
          };

          OnParticle.prototype = Object.create(WorldConfigOption.prototype);

          OnParticle.prototype.toRawHandler = function(toplevelNode) {
            var that = this;
            var worldFunction = adaptWorldFunction(that.handler);
            return rawJsworld.on_particle(worldFunction, that.event, that.options);
          };


          //////////////////////////////////////////////////////////////////////
          
          var ToParticle = function(handler, ename, sdict) {
            WorldConfigOption.call(this, 'to-particle');
            this.handler = handler;
            this.event = ename;
            this.options = sd_to_js(sdict);
          };
          
          ToParticle.prototype = Object.create(WorldConfigOption.prototype);
          
          ToParticle.prototype.toRawHandler = function(toplevelNode) {
            var that = this;
            var worldFunction = adaptWorldFunction(that.handler);
            var eventGen = function(w, k) {
              worldFunction(w, function(v) {
                if(ffi.isSome(v)) {
                  var xhr = new XMLHttpRequest();
                  xhr.open("POST","https://api.particle.io/v1/devices/events");
                  xhr.setRequestHeader("Content-type","application/x-www-form-urlencoded");
                  xhr.send("access_token=" + that.options.acc +
                           "&name=_event&data=" + that.event + ":" +
                           runtime.getField(v, "value") + "&private=true&ttl=60");
                }
                k(w);
              });
            };
            return rawJsworld.on_world_change(eventGen);
          };
          

          //////////////////////////////////////////////////////////////////////
          
          var configCore = function(coreid, acc, configs) {
            var config_str = configs.map(function(c){
              return runtime.getField(c, "_shim-convert").app();}).join("");
            var xhr = new XMLHttpRequest();
            xhr.open("POST","https://api.particle.io/v1/devices/events");
            xhr.setRequestHeader("Content-type","application/x-www-form-urlencoded");
            xhr.send("access_token=" + acc + "&name=_config&data=" + config_str + "&private=true&ttl=60");
          }
          
          var pStruct_vals = runtime.getField(pStruct, "values");
          
          //////////////////////////////////////////////////////////////////////
          var makeObject = runtime.makeObject;
          var makeFunction = runtime.makeFunction;
          
          return makeObject({
            "provide": makeObject({
              "on-particle": makeFunction(function(onEvent,eName,sd) {
                ffi.checkArity(3, arguments, "on-particle");
                runtime.checkFunction(onEvent);
                runtime.checkString(eName);
                return runtime.makeOpaque(new OnParticle(onEvent,eName,sd));
              }),
              "to-particle": makeFunction(function(toEvent,eName,sd) {
                ffi.checkArity(3, arguments, "to-particle");
                runtime.checkFunction(toEvent);
                runtime.checkString(eName);
                return runtime.makeOpaque(new ToParticle(toEvent,eName,sd));
              }),
              // core configuration
              "configure-core": makeFunction(function(coreid, acc, config) {
                ffi.checkArity(3, arguments, "configure-core");
                runtime.checkString(coreid);
                runtime.checkString(acc);
                runtime.checkList(config);
                configCore(coreid, acc, ffi.toArray(config));
              }),
              "ait-enters": runtime.getField(pStruct_vals, "ait-enters"),
              "ait-exits": runtime.getField(pStruct_vals, "ait-exits"),
              "ait-crosses": runtime.getField(pStruct_vals, "ait-crosses"),
              "is-ait-enters": runtime.getField(pStruct_vals, "is-ait-enters"),
              "is-ait-exits": runtime.getField(pStruct_vals, "is-ait-exits"),
              "is-ait-crosses": runtime.getField(pStruct_vals, "is-ait-crosses"),
              "pc-write": runtime.getField(pStruct_vals, "pc-write"),
              "pc-digital-read": runtime.getField(pStruct_vals, "pc-digital-read"),
              "pc-analog-read": runtime.getField(pStruct_vals, "pc-analog-read"),
              "is-pc-write": runtime.getField(pStruct_vals, "is-pc-write"),
              "is-pc-digital-read": runtime.getField(pStruct_vals, "is-pc-digital-read"),
              "is-pc-analog-read": runtime.getField(pStruct_vals, "is-pc-analog-read"),
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
