({
    baseUrl: './optimizer',
    name: "main",
    //include: ["requireLib"],
    optimize: 'none',
    out: 'optimizer/compiled.js',
    //wrap: true,
    skipSemiColonInsertion: true,
    paths: {
        "source-map": "../node_modules/source-map/dist/source-map",
        "q": "../node_modules/q/q",
        "seedrandom": "../node_modules/seedrandom/seedrandom",
        "s-expression": "../node_modules/s-expression/index",
        "path": "../node_modules/path/path",
        "fs": "empty:",
        "requireLib": "../node_modules/requirejs/require",
        "js-sha256": "../node_modules/js-sha256/src/sha256",
    },
    wrap: {
        startFile: "optimizer/header.js.frag"
    }
})
