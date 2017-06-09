if(typeof window === 'undefined') {
var requirejs = require("requirejs");
var define = requirejs.define;
require = requirejs;
}

// require.config({
//     baseUrl: "..",
//     paths: {
//         "local-module": "optimizer/local-module",
//         "source-map": "node_modules/source-map/dist/source-map",
//     }
// });

requirejs(["source-map", "local-module", "q", "seedrandom", "s-expression"], function(sourcemap, localmod, q, seedrandom, sexpr) {
    console.log(sourcemap != undefined);
    console.log(localmod != undefined);
    console.log(q != undefined);
    console.log(seedrandom != undefined);
    console.log(sexpr);
    return 0;
});
