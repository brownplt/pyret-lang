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
            return runtime.pauseStack((restarter) => {
                const results = [];
                const asStream = csv.parseString(str, opts.dict);
                asStream
                    .on('data', (data) => { results.push(data); })
                    .on('end', () => { restarter.resume(results); })
            })
        }
        return runtime.makeModuleReturn({
            'parse-string': runtime.makeFunction(parseString)
        }, {});
    }
})