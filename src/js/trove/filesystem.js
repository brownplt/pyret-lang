({
    requires: [
        { "import-type": "builtin", "name": "filesystem-internal" },
    ],
    provides: {
        values: {
            'read-file-string': ["arrow", ["String"], "String"]
        }
    },
    nativeRequires: [],
    theModule: function(runtime, _, _, fsInternal) {
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
        function stat(path) {
            runtime.checkArgsInternal1('filesystem', 'stat', path, runtime.String);
            return runtime.makeObj
        }
        return runtime.makeModuleReturn({
            'read-file-string': runtime.makeFunction(readFileString),
            'write-file-string': runtime.makeFunction(writeFileString),
        }, {});
    }
})