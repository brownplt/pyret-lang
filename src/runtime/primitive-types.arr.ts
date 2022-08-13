const runtime = require("./runtime.js");
const primitives = require("./primitives.js");

module.exports = {
    'is-nothing': function(x : any) { return x === runtime.nothing; },
    nothing: runtime["nothing"],
    'is-boolean': function(x : any) { return typeof x === 'boolean'; },
    'is-object': function(x : any) {
        return primitives.isRawObject(x) || primitives.isDataVariant(x);
    },
    'is-function': function(x : any) { return primitives.isFunction(x); },
    'is-tuple': function(x : any) { return primitives.isPTuple(x); },
};
