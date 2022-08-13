import { round } from './js-numbers';
import * as PRIM_TYPES from './primitives';
const NUMBERS = require('./js-numbers.js');
const EQUALITY = require("./equality.js");
const PRIMS = require('./primitives.js') as typeof PRIM_TYPES;
const seedrandom = require('seedrandom');

const NumberErrbacks = EQUALITY.NumberErrbacks;

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
var rng = seedrandom(Number(new Date()));
var num_random = function(max) {
    var f = rng();
    return NUMBERS.floor(NUMBERS.toFixnum(max) * f);
};
var num_random_seed = function(seed) {
    rng = seedrandom(String(seed));
    return PRIMS.$nothing;
  }

var num_truncate = function(n) {
    if (NUMBERS['greaterThan'](n, 0, EQUALITY.NumberErrbacks)) {
        return NUMBERS['floor'](n, EQUALITY.NumberErrbacks);
    } else {
        return NUMBERS['ceiling'](n, EQUALITY.NumberErrbacks);
    }
}

const num_ceiling = wrap1(NUMBERS.ceiling);
const num_floor = wrap1(NUMBERS.floor);
const num_round = wrap1(NUMBERS.round);
const num_round_even = wrap1(NUMBERS.roundEven);

module.exports = {

    'num-random': num_random,
    'random': num_random,

    'num-random-seed': num_random_seed,

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

    'num-remainder': wrap2(NUMBERS['remainder']),
    'num-modulo': wrap2(NUMBERS['modulo']),
    'num-truncate': num_truncate,
    'num-sqrt': wrap1(NUMBERS['sqrt']),
    'num-sqr': wrap1(NUMBERS['sqr']),

    'num-ceiling': num_ceiling,
    'num-floor': num_floor,

    'num-round': num_round,
    'num-round-even': num_round_even,

    'num-log': wrap1(NUMBERS['log']),
    'num-exp': wrap1(NUMBERS['exp']),
    'num-expt': wrap2(NUMBERS['expt']),

    'num-to-roughnum': wrap1(NUMBERS['toRoughnum']),

    'is-number': NUMBERS["isPyretNumber"],
    'is-roughnum': NUMBERS["isRoughnum"],
    'num-is-integer': NUMBERS['isInteger'],
    'num-is-rational': NUMBERS['isRational'],
    'num-is-roughnum': NUMBERS['isRoughnum'],
    'num-is-positive': NUMBERS['isPositive'],
    'num-is-negative': NUMBERS['isNegative'],
    'num-is-non-positive': NUMBERS['isNonPositive'],
    'num-is-non-negative': NUMBERS['isNonNegative'],


    'num-to-string': numToString,
    'num-to-string-digits': wrap2(NUMBERS['toStringDigits']),

    'num-within': EQUALITY["within"],
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

    'num-clamp': function(num, minInclusive, maxInclusive): number {
      if (EQUALITY["_lessthan"](num, minInclusive)) {
        return num;
      } else if (EQUALITY["_greaterthan"](num, maxInclusive)) {
        return maxInclusive;
      } else {
        return num;
      }
    },

    'num_truncate_digits': function(n, digits) {
        var tenDigits = NUMBERS.expt(10, digits, NumberErrbacks);
        return NUMBERS.divide(num_truncate(NUMBERS.multiply(n, tenDigits, NumberErrbacks)), tenDigits, NumberErrbacks);
    },
    'num_ceiling_digits': function(n, digits) {
      var tenDigits = NUMBERS.expt(10, digits, NumberErrbacks);
      return NUMBERS.divide(num_ceiling(NUMBERS.multiply(n, tenDigits, NumberErrbacks)), tenDigits, NumberErrbacks);
    },
    'num_floor_digits': function(n, digits) {
      var tenDigits = NUMBERS.expt(10, digits, NumberErrbacks);
      return NUMBERS.divide(num_floor(NUMBERS.multiply(n, tenDigits, NumberErrbacks)), tenDigits, NumberErrbacks);
    },
    'num_round_digits': function(n, digits) {
      var tenDigits = NUMBERS.expt(10, digits, NumberErrbacks);
      return NUMBERS.divide(num_round(NUMBERS.multiply(n, tenDigits, NumberErrbacks)), tenDigits, NumberErrbacks);
    },
    'num_round_even_digits': function(n, digits) {
      var tenDigits = NUMBERS.expt(10, digits, NumberErrbacks);
      return NUMBERS.divide(num_round_even(NUMBERS.multiply(n, tenDigits, NumberErrbacks)), tenDigits, NumberErrbacks);
    },
    'num_truncate_place': function(n, place) {
      var tenPlace = NUMBERS.expt(10, place, NumberErrbacks);
      return NUMBERS.multiply(num_truncate(NUMBERS.divide(n, tenPlace, NumberErrbacks)), tenPlace, NumberErrbacks);
    },
    'num_ceiling_place': function(n, place) {
      var tenPlace = NUMBERS.expt(10, place, NumberErrbacks);
      return NUMBERS.multiply(num_ceiling(NUMBERS.divide(n, tenPlace, NumberErrbacks)), tenPlace, NumberErrbacks);
    },
    'num_floor_place': function(n, place) {
      var tenPlace = NUMBERS.expt(10, place, NumberErrbacks);
      return NUMBERS.multiply(num_floor(NUMBERS.divide(n, tenPlace, NumberErrbacks)), tenPlace, NumberErrbacks);
    },
    'num_round_place': function(n, place) {
      var tenPlace = NUMBERS.expt(10, place, NumberErrbacks);
      return NUMBERS.multiply(num_round(NUMBERS.divide(n, tenPlace, NumberErrbacks)), tenPlace, NumberErrbacks);
    },
    'num_round_even_place': function(n, place) {
      var tenPlace = NUMBERS.expt(10, place, NumberErrbacks);
      return NUMBERS.multiply(num_round_even(NUMBERS.divide(n, tenPlace, NumberErrbacks)), tenPlace, NumberErrbacks);
    }
};
