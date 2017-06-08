({
    baseUrl: './optimizer',
    name: "all",
    optimize: 'none',
    out: 'compiled.js',
    skipSemiColonInsertion: true,
    paths: {
        "source-map": "../node_modules/source-map/dist/source-map",
        "q": "../node_modules/q/q",
        "seedrandom": "../node_modules/seedrandom/seedrandom",
        "s-expression": "../node_modules/s-expression/index",
        "path": "../node_modules/path/path",
        "fs": "empty:",
        "requirejs": "../node_modules/requirejs/require",
        "js-sha256": "../node_modules/js-sha256/src/sha256"
    }
})
// Run with node node_modules/requirejs/bin/r.js -o build.js
