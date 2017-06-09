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

requirejs(["source-map", "local-module"], function(sourcemap, localmod) {
    console.log(sourcemap);
    console.log(localmod);
    return 0;
});
