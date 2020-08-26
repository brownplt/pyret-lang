const NUMBERS = require('./js-NUMBERS.js');

function numToString(n) {
  return String(n);
}

module.exports = {
  'num-equal': NUMBERS['equals'],
  'num-is-integer': NUMBERS['isInteger'],
  'num-is-rational': NUMBERS['isRational'],
  'num-is-roughnum': NUMBERS['isRoughnum'],
  'num-is-positive': NUMBERS['isPositive'],
  'num-is-negative': NUMBERS['isNegative'],
  'num-is-non-positive': NUMBERS['isNonPositive'],
  'num-is-non-negative': NUMBERS['isNonNegative'],
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
  'num-to-string': numToString,
  'num-to-string-digits': NUMBERS['toStringDigits'],
  'num-within-abs': function(tol) {
    return function(l, r) {
      return NUMBERS['roughlyEquals'](l, r, tol);
    };
  },
  'num-within-rel': function(relTol) {
    return function(l, r) {
      return NUMBERS['roughlyEqualsRel'](l, r, relTol);
    };
  }
};
