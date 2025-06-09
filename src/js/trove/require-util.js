({
    requires: [],
    provides: {
        values: {
            'resolve': ["arrow", ["String"], "String"],
        },
        types: {}
    },
    nativeRequires: [],
    theModule: function(runtime, _, _) {
        function resolve(moduleName) {
            runtime.checkArgsInternal1('require-util', 'resolve', moduleName, runtime.String);
            try {
                return require.resolve(moduleName);
            } catch (err) {
                throw runtime.throwMessageException(`Error resolving ${moduleName}: ${String(err)}`);
            }
        }
        function cannotResolve(moduleName) {
            throw runtime.throwMessageException(`Cannot resolve module: ${moduleName}; require.resolve is not available in this context (perhaps you're running a script meant for the command line in the browser?)`);
        }
        let _resolve;
        if(typeof require === 'undefined' || !require.resolve) {
            _resolve = cannotResolve;
        } else {
            _resolve = resolve;
        }
        return runtime.makeModuleReturn({
            resolve: runtime.makeFunction(_resolve, 'resolve')
        }, {});
    }
})