({
    requires: [],
    nativeRequires: ["escodegen", "path"],
    provides: {
        values : {
            "check-well-formed": "tany"
        }
    },
    theModule: function(runtime, _, __) {
        function checkWellFormed(ast, options) {
            options.dict.log.app("In check-wellformed TS!\n", runtime.ffi.makeNone());
        }
        return runtime.makeModuleReturn({
            "check-well-formed": runtime.makeFunction(checkWellFormed)
        }, {});
    }
})