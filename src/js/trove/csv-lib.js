({
    requires: [],
    nativeRequires: ['fast-csv'],
    provides: {
        values: {
            'parse-string': ["arrow", ["String"], ["RawArray", ["RawArray", "String"]]]
        },
        types: {}
    },
    theModule: function(runtime, _, uri, csv) {
        function parseString(str, opts) {
            runtime.ffi.checkArity(2, arguments, "parse-string", false);
            runtime.checkArgsInternal2("csv-lib", "parse-string", str, runtime.String, opts, runtime.Object);
            return runtime.pauseStack((restarter) => {
                const results = [];
                try {
                    const asStream = csv.parseString(str, opts.dict);
                    let errored = false;
                    asStream
                        .on('error', (error) => { restarter.error(runtime.ffi.makeMessageException("Error reading CSV: " + String(error))); })
                        .on('data', (data) => { results.push(data); })
                        .on('end', () => { restarter.resume(results); })
                }
                catch(e) {
                    restarter.error(runtime.ffi.makeMessageException("Error reading CSV: " + String(error))); 
                }
            })
        }
        return runtime.makeModuleReturn({
            'parse-string': runtime.makeFunction(parseString)
        }, {});
    }
})