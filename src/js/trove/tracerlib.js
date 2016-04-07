define(["js/runtime-util", "js/ffi-helpers"], function(util, ffiLib) {
    return util.memoModule("tracerlib", function(RUNTIME, NAMESPACE) {
        return RUNTIME.loadJSModules(NAMESPACE, [ffiLib], function(ffi) {
            
            var LOG = [];
            
            return RUNTIME.makeObject({
                "provide": RUNTIME.makeObject({
                    "init": RUNTIME.makeFunction(function() {
                        ffi.checkArity(0, arguments, "init");
                        LOG = [];
                    }),
                    "log-event": RUNTIME.makeFunction(function(type, data) {
                        ffi.checkArity(2, arguments, "log-event");
                        RUNTIME.checkString(type);
                        RUNTIME.checkString(data);
                        var type_str = RUNTIME.unwrap(type);
                        var data_str = RUNTIME.unwrap(data);
                        LOG.push({ "type": type_str, "data": data_str });
                    })
                }),
                "answer": NAMESPACE.get("nothing")
            });
        });
    });
});
