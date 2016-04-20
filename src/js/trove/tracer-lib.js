define(["js/runtime-util", "js/ffi-helpers"], function(util, ffiLib) {
    return util.memoModule("tracer-lib", function(runtime, namespace) {
        return runtime.loadJSModules(namespace, [ffiLib], function(ffi) {
            
            var LOG = [];
            var ID = 0;
            var POPUP = null;

            function reset() {
                LOG = [];
                ID = 0;
            }
            
            function next_id() {
                ID = ID + 1;
                return ID;
            }

            function debug_log() {
                for (var i in LOG) {
                    var event = LOG[i];
                    var data;
                    switch (event.type) {
                    case "ENTER":  data = event.loc; break;
                    case "EXIT":   data = event.loc; break;
                    case "CALL":   data = event.func; break;
                    case "RETURN": data = event.value; break;
                    }
                    console.log("\t" + event.type +
                                "\t" + event.id +
                                "\t" + data);
                }
            }
            
            return runtime.makeObject({
                "provide": runtime.makeObject({
                    "enter": runtime.makeFunction(function(srcloc) {
                        ffi.checkArity(1, arguments, "enter");
                        var srcloc = runtime.makeSrcloc(srcloc);
                        var id = next_id();
                        LOG.push({"type": "ENTER",
                                  "loc": srcloc,
                                  "id": id});
                        return runtime.makeNumber(id);
                    }),
                    "exit": runtime.makeFunction(function(srcloc, id) {
                        ffi.checkArity(2, arguments, "exit");
                        var srcloc = runtime.makeSrcloc(srcloc);
                        runtime.checkNumber(id);
                        var id = runtime.unwrap(id);
                        LOG.push({"type": "EXIT",
                                  "loc": srcloc,
                                  "id": id});
                        return runtime.nothing;
                    }),
                    "log-call": runtime.makeFunction(function(func, args) {
                        ffi.checkArity(2, arguments, "log-call");
                        runtime.checkString(func);
                        runtime.checkList(args);
                        var func = runtime.unwrap(func);
                        var args = ffi.toArray(args);
                        var id = next_id();
                        LOG.push({"type": "CALL",
                                  "func": func,
                                  "args": args,
                                  "id": id});
                        return runtime.makeNumber(id);
                    }),
                    "log-return": runtime.makeFunction(function(value, id) {
                        ffi.checkArity(2, arguments, "log-return");
                        runtime.checkString(value);
                        runtime.checkNumber(id);
                        var value = runtime.unwrap(value);
                        var id = runtime.unwrap(id);
                        LOG.push({"type": "RETURN",
                                  "value": value,
                                  "id": id});
                        return runtime.nothing;
                    }),
                    "show-trace": runtime.makeFunction(function() {
                        ffi.checkArity(0, arguments, "show-trace");
                        if (POPUP) {
                            POPUP.close();
                            POPUP = null;
                        }
                        POPUP = window.open("/tracer");
                        if (POPUP) {
                            POPUP.window.LOG = LOG;
                        } else {
                            console.log("Tracer popup blocked");
                        }
                        // TODO: Temporary logging
                        debug_log();
                        reset();
                        return runtime.nothing;
                    })
                }),
                "answer": namespace.get("nothing")
            });
        });
    });
});
