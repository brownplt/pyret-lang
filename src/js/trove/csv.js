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
        console.log("In csv: ", uri, csv);
        function parseString(str) {
            return runtime.pauseStack((restarter) => {
                const results = [];
                const asStream = csv.parseString(str);
                asStream
                    .on('data', (data) => {console.log("data: ", data); results.push(data)})
                    .on('end', () => { restarter.resume(results); })
            })
        }
        return runtime.makeModuleReturn({
            'parse-string': runtime.makeFunction(parseString)
        }, {});
    }
})