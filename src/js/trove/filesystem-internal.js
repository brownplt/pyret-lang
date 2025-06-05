({
    provides: {
        values: {},
        types: {},
    },
    requires: [ ],
    nativeRequires: ['fs', 'path'],
    /**
     * Provides a Pyret-specific filesystem API based on `node`. The API is
     * designed to give a consistent view of the filesystem and path utilities
     * across the node fs and path libraries:
     * 
     * https://nodejs.org/docs/latest/api/fs.html
     * https://nodejs.org/docs/latest/api/path.html
     * 
     * and the VScode FileSystemProvider and vscode-uri library:
     * 
     * https://code.visualstudio.com/api/references/vscode-api#FileSystemProvider
     * https://github.com/microsoft/vscode-uri?tab=readme-ov-file#usage-util
     * 
     * If Pyret needs to run on new environments with new filesystem
     * definitions, this is the module to replace with --allow-builtin-overrides.
     * 
     * Since these don't provide a consistent set of names, the names here don't
     * always exactly match the corresponding underlying function, and sometimes
     * have their inputs simplified or outputs manipulated to match a common
     * interface.
     */
    theModule: function(runtime, _, uri, fs, path) {
        let initializedOK = true;
        if(!('promises' in fs)) {
            console.warn("Could not find 'promises' in node fs library, cannot initialize filesystem-internal and its functions will throw.");
            initializedOK = false;
        }
        const fsp = fs.promises;
        function wrap(f) {
            if(initializedOK) { return f; }
            else {
                return async function(...args) {
                    throw runtime.ffi.makeMessageException(`filesystem-internal: Cannot call ${f.name} because fs.promises not available`)
                }
            }
        }
        async function readFile(p) {
            return fsp.readFile(p);
        }
        async function writeFile(p, data) {
            return fsp.writeFile(p, data);
        }
        /**
         * Guaranteed fields are
         *  - `ctime` and `mtime` in epoch ms
         *  - `size` in bytes
         * 
         * The underlying `fs` return value is in the `native` field
         */
        async function stat(p) {
            const stats = await fsp.stat(p);
            return {
                ctime: stats.ctimeMs,
                mtime: stats.mtimeMs,
                size: stats.size,
                native: stats
            };
        }

        async function resolve(...paths) {
            return path.resolve(...paths);
        }

        async function exists(p) {
            // NOTE(joe): this is sync because the async version is deprecated
            // See https://nodejs.org/dist/latest-v10.x/docs/api/fs.html#fs_fs_existssync_path
            // Also, `exists` is not defined on the `fs.promises` api
            return fs.existsSync(p);
        }

        async function join(...paths) {
            return path.join(...paths);
        }

        async function createDir(p) {
            /* NOTE(joe): this does not create parent dirs because other
             * platforms may not support it (in particular VScode
             * filesystemprovider) */
            return fsp.mkdir(p);
        }
        async function relative(from, to) {
            return path.relative(from, to);
        }
        async function isAbsolute(p) {
            return path.isAbsolute(p);
        }
        async function basename(p) {
            return path.basename(p);
        }
        async function dirname(p) {
            return path.dirname(p);
        }
        return runtime.makeJSModuleReturn({
            readFile: wrap(readFile),
            writeFile: wrap(writeFile),
            stat: wrap(stat),
            resolve: wrap(resolve),
            exists: wrap(exists),
            join: wrap(join),
            'path-sep': path.sep,
            createDir: wrap(createDir),
            relative: wrap(relative),
            isAbsolute: wrap(isAbsolute),
            basename: wrap(basename),
            dirname: wrap(dirname),
        });
    }
})