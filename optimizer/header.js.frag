if(typeof window === 'undefined') {
var requirejs = require("requirejs");
var define = requirejs.define;
require = requirejs;
}

if (typeof module !== "undefined" && typeof module.exports !== "undefined") {
   oldExports = exports;
   exports = undefined;
   module.exports = undefined;
}
