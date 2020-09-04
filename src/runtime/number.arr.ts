const NUMBERS = require('./js-numbers.js');
const EQUALITY = require("./equality.js");


function numToString(n) {
    return String(n);
}

function wrap1(f) {
    return function(v1) {
        return f(v1, EQUALITY.NumberErrbacks);
    };
}

function wrap2(f) {
    return function(v1, v2) {
        return f(v1, v2, EQUALITY.NumberErrbacks);
    };
}

module.exports = {

    'num-equal': wrap2(NUMBERS['equals']),

    'num-max': function(a, b) {
        if (NUMBERS['greaterThanOrEqual'](a, b, EQUALITY.NumberErrbacks)) {
            return a;
        } else {
            return b;
        }
    },

    'num-min': function(a, b) {
        if (NUMBERS['lessThanOrEqual'](a, b, EQUALITY.NumberErrbacks)) {
            return a;
        } else {
            return b;
        }
    },

    'num-abs': wrap1(NUMBERS['abs']),
    'num-sin': wrap1(NUMBERS['sin']),
    'num-cos': wrap1(NUMBERS['cos']),
    'num-tan': wrap1(NUMBERS['tan']),
    'num-asin':wrap1(NUMBERS['asin']),
    'num-acos':wrap1(NUMBERS['acos']),
    'num-atan':wrap1(NUMBERS['atan']),
    'num-atan2': wrap2(NUMBERS['atan2']),

    'num-modulo': wrap2(NUMBERS['modulo']),
    'num-truncate': function(n) {
        if (NUMBERS['greaterThan'](n, 0, EQUALITY.NumberErrbacks)) {
            return NUMBERS['floor'](n, EQUALITY.NumberErrbacks);
        } else {
            return NUMBERS['ceiling'](n, EQUALITY.NumberErrbacks);
        }
    },
    'num-sqrt': wrap1(NUMBERS['sqrt']),
    'num-sqr': wrap1(NUMBERS['sqr']),

    'num-ceiling': wrap1(NUMBERS['ceiling']),
    'num-floor': wrap1(NUMBERS['floor']),

    'num-round': wrap1(NUMBERS['round']),
    'num-round-even': wrap1(NUMBERS['roundEven']),

    'num-log': wrap1(NUMBERS['log']),
    'num-exp': wrap1(NUMBERS['exp']),
    'num-expt': wrap2(NUMBERS['expt']),

    'num-to-roughnum': wrap1(NUMBERS['toRoughnum']),

    'num-is-integer': NUMBERS['isInteger'],
    'num-is-rational': NUMBERS['isRational'],
    'num-is-roughnum': NUMBERS['isRoughnum'],
    'num-is-positive': NUMBERS['isPositive'],
    'num-is-negative': NUMBERS['isNegative'],
    'num-is-non-positive': NUMBERS['isNonPositive'],
    'num-is-non-negative': NUMBERS['isNonNegative'],


    'num-to-string': numToString,
    'num-to-string-digits': NUMBERS['toStringDigits'],

    'num-within-abs': EQUALITY["withinAbs"],
    'num-within-rel': EQUALITY["withinRel"],
    'within': EQUALITY["within"],
    'within-abs': EQUALITY["withinAbs"],
    'within-rel': EQUALITY["withinRel"],
    'within-abs-now': EQUALITY["withinAbsNow"],
    'within-rel-now': EQUALITY["withinRelNow"],
    'within-abs3': EQUALITY["withinAbs3"],
    'within-rel3': EQUALITY["withinRel3"],
    'within-abs-now3': EQUALITY["withinAbsNow3"],
    'within-rel-now3': EQUALITY["withinRelNow3"],



    'num-is-fixnum': function(num): boolean {
        return typeof(num) === "number";
    },

    'num-exact': function(num): number {
        return NUMBERS["toExact"](num);
    },

    'num-to-rational': function(num): number {
        return NUMBERS["toRational"](num);
    },
};
