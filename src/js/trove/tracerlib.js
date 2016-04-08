define(["js/runtime-util", "js/ffi-helpers"], function(util, ffiLib) {
    return util.memoModule("tracerlib", function(RUNTIME, NAMESPACE) {
        return RUNTIME.loadJSModules(NAMESPACE, [ffiLib], function(ffi) {
            
            var LOG = [];
            
            return RUNTIME.makeObject({
                "provide": RUNTIME.makeObject({
                    "begin-trace": RUNTIME.makeFunction(function() {
                        console.log("BEGIN TRACE");
                        ffi.checkArity(0, arguments, "begin-trace");
                        LOG = [];
                    }),
                    "end-trace": RUNTIME.makeFunction(function() {
                        ffi.checkArity(0, arguments, "end-trace");
                        LOG.forEach(function(event) {
                            console.log("  " + event.type + ": " + event.data);
                        });
                        console.log("END TRACE");
                        // TODO: Call out to code.pyret.org to display
                    }),
                    "log-call": RUNTIME.makeFunction(function(data) {
                        ffi.checkArity(1, arguments, "log-call");
                        RUNTIME.checkString(data);
                        var data_str = RUNTIME.unwrap(data);
                        LOG.push({"type": "CALL", "data": data_str});
                    }),
                    "log-return": RUNTIME.makeFunction(function(data) {
                        ffi.checkArity(1, arguments, "log-return");
                        RUNTIME.checkString(data);
                        var data_str = RUNTIME.unwrap(data);
                        LOG.push({"type": "RETURN", "data": data_str});
                    })
                }),
                "answer": NAMESPACE.get("nothing")
            });
        });
    });
});
