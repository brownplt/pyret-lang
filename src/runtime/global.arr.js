var runtime = require('./runtime.js');
var array = require('./array.js');
var numbers = require('./js-numbers.js');
var assert = require('assert');

function _plus(l, r) { return l + r; }
function _minus(l, r) { return l - r; }
function _times(l, r) { return l * r; }
function _divide(l, r) { return l / r; }
function _lessthan(l, r) { return l < r; }
function _greaterthan(l, r) { return l > r; }
function _lessequal(l, r) { return l <= r; }
function _greaterequal(l, r) { return l >= r; }

function _not(x) { return !x; }

function numToString(n) {
  return String(n);
}

function timeNow() {
  return new Date().getTime();
}

module.exports = {
  'num-to-str': numToString,
  'time-now' : timeNow,
  'js-to-string': function(v) { return String(v); },
  'raw-array': array['raw-array'],
  'display-string': function(s) { process.stdout.write(s); },
  "console-log": function(v) { console.log(v); },
  'assert': function( lv, rv, msg ) {
    if(!(lv === rv)) {
      throw new Error(msg);
    }
    else {
      return true;
    }
  },
  print: function(v) {
    process.stdout.write(String(v));
  },
  '_plus': _plus,
  '_minus': _minus,
  '_times': _times,
  '_divide': _divide,
  '_lessthan': _lessthan,
  '_greaterthan': _greaterthan,
  '_lessequal': _lessequal,
  '_greaterequal': _greaterequal,
  'not': _not,

  'Equal': runtime['Equal'],
  'NotEqual': runtime['NotEqual'],
  'Unknown': runtime['Unknown'],
  'is-Equal': runtime['is-Equal'],
  'is-NotEqual': runtime['is-NotEqual'],
  'is-Unknown': runtime['is-Unknown'],

  'equal-always': runtime['equalAlways'],
  'equal-always3': runtime['equalAlways3'],
  'identical': runtime['identical'],
  'identical3': runtime['identical3'],
  'trace-value': runtime['traceValue'],

  // TODO(alex): Think of better way to expose runtime
  'runtime': runtime,

  // Number Functions

  'num-equal': numbers['equals'],
  'num-is-integer': numbers['isInteger'],
  'num-is-rational': numbers['isRational'],
  'num-is-roughnum': numbers['isRoughnum'],
  'num-is-positive': numbers['isPositive'],
  'num-is-negative': numbers['isNegative'],
  'num-is-non-positive': numbers['isNonPositive'],
  'num-is-non-negative': numbers['isNonNegative'],
  'num-max': function(a, b) {
    if (numbers['greaterThan'](a, b)) {
      return a;
    } else {
      return b;
    }
  },
  'num-min': function(a, b) {
    if (numbers['greaterThan'](a, b)) {
      return b;
    } else {
      return a;
    }
  },
  'num-abs': numbers['abs'],
  'num-sin': numbers['sin'],
  'num-cos': numbers['cos'],
  'num-tan': numbers['tan'],
  'num-asin': numbers['asin'],
  'num-acos': numbers['acos'],
  'num-atan': numbers['atan'],
  'num-atan2': numbers['atan2'],
  'num-modulo': numbers['modulo'],
  'num-truncate': function(n) {
    if (numbers['greaterThan'](n, 0)) {
      return numbers['floor'](n);
    } else {
      return numbers['ceiling'](n);
    }
  },
  'num-sqrt': numbers['sqrt'],
  'num-sqr': numbers['sqr'],
  'num-ceiling': numbers['ceiling'],
  'num-floor': numbers['floor'],
  'num-round': numbers['round'],
  'num-round-even': numbers['round-even'],
  'num-log': numbers['log'],
  'num-exp': numbers['exp'],
  'num-expt': numbers['expt'],
  'num-to-roughnum': numbers['toRoughnum'],
  'num-is-integer': numbers['isInteger'],
  'num-is-rational': numbers['isRational'],
  'num-is-roughnum': numbers['isRoughnum'],
  'num-to-string': function(n) {
    return String(n);
  },
  'num-to-string-digits': numbers['toStringDigits'],
};
