({
    requires: [],
    provides: {
        values: {
            'resolve': ["arrow", ["String"], "String"],
        },
        types: {}
    },
    nativeRequires: ["resolve"],
    theModule: function(runtime, _, _, browserifyResolve) {
        function resolve(moduleName, baseDir) {
            console.log(moduleName, baseDir);
            runtime.checkArgsInternal2('require-util', 'resolve', moduleName, runtime.String, baseDir, runtime.String);
            try {
                return runtime.pauseStack((restarter) => {
                  browserifyResolve(moduleName, { basedir: baseDir }, (err, resolved) => {
                      if(err) { restarter.error(runtime.makeMessageException(`Error resolving ${moduleName} from ${baseDir}: ${String(err)}`)); }
                      restarter.resume(resolved);
                  });
                });
            } catch (err) {
                throw runtime.throwMessageException(`Error resolving ${moduleName} from ${baseDir}: ${String(err)}`);
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
