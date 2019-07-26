var runtime = require('./runtime.js');
var array = require('./array.js');
//var assert = require('assert');

function _plus(l, r) { return l + r; }
function _minus(l, r) { return l - r; }
function _times(l, r) { return l * r; }
function _divide(l, r) { return l / r; }
function _lessthan(l, r) { return l < r; }
function _greaterthan(l, r) { return l > r; }
function _lessequal(l, r) { return l <= r; }
function _greaterequal(l, r) { return l >= r; }

function numToString(n) {
  return String(n);
}

function timeNow( otherTime = undefined ) {
  if ( otherTime === undefined ) {
    return process.hrtime();
  } else {
    return process.hrtime( otherTime );
  }
}

function _spy(spyObject) {
  const message = spyObject.message();
  if (message != null) {
    console.log("Spy message: " + message);
  }

  const exprs = spyObject.exprs;
  for (let i = 0; i < exprs.length; i++) {
    const key = exprs[i].key;
    const loc = exprs[i].loc;
    const value = exprs[i].expr();
    console.log("Spying " + key + " at " + loc);
    console.log(key + ":" + value);
  }
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
  'equal-always': runtime['py_equal'],
  'trace-value': runtime['trace-value'],
  '$spy': _spy,
};

