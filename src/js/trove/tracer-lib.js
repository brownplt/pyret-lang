define(["js/runtime-util", "js/ffi-helpers"], function(util, ffiLib) {
    return util.memoModule("tracer-lib", function(runtime, namespace) {
        return runtime.loadJSModules(namespace, [ffiLib], function(ffi) {
            
            var LOG = [];
            var POPUP = null;
            
            return runtime.makeObject({
                "provide": runtime.makeObject({
                    "begin-trace": runtime.makeFunction(function() {
                        ffi.checkArity(0, arguments, "begin-trace");
                        LOG = [];
                        return runtime.nothing;
                    }),
                    "end-trace": runtime.makeFunction(function() {
                        ffi.checkArity(0, arguments, "end-trace");
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
                        return runtime.nothing;
                    }),
                    "log-call": runtime.makeFunction(function(data) {
                        ffi.checkArity(1, arguments, "log-call");
                        runtime.checkString(data);
                        var data_str = runtime.unwrap(data);
                        LOG.push({"type": "CALL", "data": data_str});
                        return runtime.nothing;
                    }),
                    "log-return": runtime.makeFunction(function(data) {
                        ffi.checkArity(1, arguments, "log-return");
                        runtime.checkString(data);
                        var data_str = runtime.unwrap(data);
                        LOG.push({"type": "RETURN", "data": data_str});
                        return runtime.nothing;
                    })
                }),
                "answer": namespace.get("nothing")
            });
        });
    });
});
