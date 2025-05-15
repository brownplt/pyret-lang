({
    requires: [
        { "import-type": "builtin", "name": "filesystem-internal" },
    ],
    provides: {
        values: {
            'read-file-string': ["arrow", ["String"], "String"],
            'write-file-string': ["arrow", ["String", "String"], "String"],
            'stat': ["arrow", ["String"], "Any"],
            'resolve': ["arrow", ["String"], "String"],
        },
        types: {}
    },
    nativeRequires: ['buffer'],
    theModule: function(runtime, _, _, fsInternal, buffer) {
        const Buffer = buffer.Buffer;
        function readFileString(path) {
            runtime.checkArgsInternal1('filesystem', 'read-file-string', path, runtime.String);
            const result = fsInternal.readFile(path)
                .then((contents) => Buffer.from(contents).toString('utf8'));
            return runtime.await(result);
        }
        function writeFileString(path, data) {
            runtime.checkArgsInternal2('filesystem', 'write-file-string', path, runtime.String, data, runtime.String);
            const result = fsInternal.writeFile(path, Buffer.alloc(data.length, data, 'utf8'));
            return runtime.await(result);
        }
        function resolve(path) {
            const result = fsInternal.resolve(path);
            return runtime.await(result);
        }
        function stat(path) {
            runtime.checkArgsInternal1('filesystem', 'stat', path, runtime.String);
            const result = fsInternal.stat(path).then((stats) => {
                return runtime.makeObject({
                    ctime: stats.ctime,
                    mtime: stats.mtime,
                    size: stats.size,
                    native: stats
                });
            });
            return runtime.await(result);
        }
        return runtime.makeModuleReturn({
            'read-file-string': runtime.makeFunction(readFileString),
            'write-file-string': runtime.makeFunction(writeFileString),
            'stat': runtime.makeFunction(stat),
            'resolve': runtime.makeFunction(resolve),
        }, {});
    }
})