({
    requires: [
        { "import-type": "builtin", "name": "filesystem-internal" },
    ],
    provides: {
        values: {
            'read-file-string': ["arrow", ["String"], "String"],
            'write-file-string': ["arrow", ["String", "String"], "Nothing"],
            'stat': ["arrow", ["String"], "Any"],
            'resolve': ["arrow", ["String"], "String"],
            'exists': ["arrow", ["String"], "Boolean"],
            'join': ["arrow", ["String", "String"], "String"],
            'create-dir': ["arrow", ["String"], "String"],
            'basename': ["arrow", ["String"], "String"],
            'dirname': ["arrow", ["String"], "String"],
            'relative': ["arrow", ["String", "String"], "String"],
            'is-absolute': ["arrow", ["String"], "Boolean"],
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
            const result = fsInternal.writeFile(path, Buffer.alloc(data.length, data, 'utf8')).then(() => runtime.nothing);
            return runtime.await(result);
        }
        function resolve(path) {
            const result = fsInternal.resolve(path);
            return runtime.await(result);
        }
        function join(path1, path2) {
            const result = fsInternal.join(path1, path2);
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
        function exists(path) {
            runtime.checkArgsInternal1('filesystem', 'exists', path, runtime.String);
            const result = fsInternal.exists(path);
            return runtime.await(result);
        }
        function createDir(path) {
            runtime.checkArgsInternal1('filesystem', 'create-dir', path, runtime.String);
            const result = fsInternal.createDir(path).then(() => runtime.nothing);
            return runtime.await(result);
        }
        function basename(path) {
            runtime.checkArgsInternal1('filesystem', 'basename', path, runtime.String);
            const result = fsInternal.basename(path);
            return runtime.await(result);
        }
        function dirname(path) {
            runtime.checkArgsInternal1('filesystem', 'dirname', path, runtime.String);
            const result = fsInternal.dirname(path);
            return runtime.await(result);
        }
        function relative(from, to) {
            runtime.checkArgsInternal2('filesystem', 'relative', from, runtime.String, to, runtime.String);
            const result = fsInternal.relative(from, to);
            return runtime.await(result);
        }
        function isAbsolute(path) {
            runtime.checkArgsInternal1('filesystem', 'is-absolute', path, runtime.String);
            const result = fsInternal.isAbsolute(path);
            return runtime.await(result);
        }
        return runtime.makeModuleReturn({
            'read-file-string': runtime.makeFunction(readFileString),
            'write-file-string': runtime.makeFunction(writeFileString),
            'stat': runtime.makeFunction(stat),
            'resolve': runtime.makeFunction(resolve),
            'exists': runtime.makeFunction(exists),
            'join': runtime.makeFunction(join),
            'create-dir': runtime.makeFunction(createDir),
            'basename': runtime.makeFunction(basename),
            'dirname': runtime.makeFunction(dirname),
            'relative': runtime.makeFunction(relative),
            'is-absolute': runtime.makeFunction(isAbsolute),
        }, {});
    }
})