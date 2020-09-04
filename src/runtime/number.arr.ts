const NUMBERS = require('./js-numbers.js');
const EQUALITY = require("./equality.js");


function numToString(n) {
    return String(n);
}

module.exports = {

    'num-equal': NUMBERS['equals'],

    'num-max': function(a, b) {
        if (NUMBERS['greaterThan'](a, b)) {
            return a;
        } else {
            return b;
        }
    },

    'num-min': function(a, b) {
        if (NUMBERS['greaterThan'](a, b)) {
            return b;
        } else {
            return a;
        }
    },

    'num-abs': NUMBERS['abs'],
    'num-sin': NUMBERS['sin'],
    'num-cos': NUMBERS['cos'],
    'num-tan': NUMBERS['tan'],
    'num-asin': NUMBERS['asin'],
    'num-acos': NUMBERS['acos'],
    'num-atan': NUMBERS['atan'],
    'num-atan2': NUMBERS['atan2'],

    'num-modulo': NUMBERS['modulo'],
    'num-truncate': function(n) {
        if (NUMBERS['greaterThan'](n, 0)) {
            return NUMBERS['floor'](n);
        } else {
            return NUMBERS['ceiling'](n);
        }
    },
    'num-sqrt': NUMBERS['sqrt'],
    'num-sqr': NUMBERS['sqr'],

    'num-ceiling': NUMBERS['ceiling'],
    'num-floor': NUMBERS['floor'],

    'num-round': NUMBERS['round'],
    'num-round-even': NUMBERS['round-even'],

    'num-log': NUMBERS['log'],
    'num-exp': NUMBERS['exp'],
    'num-expt': NUMBERS['expt'],

    'num-to-roughnum': NUMBERS['toRoughnum'],

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
        return NUMBERS["isFixnum"](num);
    },

    'num-to-exact': function(num): number {
        return NUMBERS["toExact"](num);
    },

    'num-to-rational': function(num): number {
        return NUMBERS["toRational"](num);
    },
};
