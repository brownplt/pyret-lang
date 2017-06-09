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

if (typeof process === "undefined") {
    var process = {
        stdout: {write: function(s) { console.log(s); } },
        stderr: {write: function(s) { console.error(s); } },
        argv: [],
        exit: function(err) {
            if (err != 0) {
                throw new Error("Program ending with signal: ", err);
            }
        }
    }
}
