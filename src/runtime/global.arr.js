var runtime = require('./runtime.js');
var array = require('./array.js');
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

function timeNow( otherTime ) {
  if ( otherTime === undefined ) {
    return process.hrtime();
  } else {
    return process.hrtime( otherTime );
  }
}

function _spy(spyObject) {
  const message = spyObject.message();
  const spyLoc = spyObject.loc;
  if (message != null) {
    console.log("Spying \"" + message + "\" (at " + spyLoc + ")");
  } else {
    console.log("Spying (at " + spyLoc + ")");
  }

  const exprs = spyObject.exprs;
  for (let i = 0; i < exprs.length; i++) {
    const key = exprs[i].key;
    const loc = exprs[i].loc;
    const value = exprs[i].expr();
    console.log("  " + key + ": " + value);
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
  '$spy': _spy,

  // TODO(alex): Think of better way to expose runtime
  'runtime': runtime,
};
