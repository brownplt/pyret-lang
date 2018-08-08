var runtime = require('./runtime.js');
var array = require('./array.js');
var assert = require('assert').strict;

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

module.exports = {
  'num-to-str': numToString,
  'time-now' : timeNow,
  'raw-array': array['raw-array'],
  'display-string': function(s) { process.stdout.write(s); },
  "console-log": function(v) { console.log(v); },
  'assert': function( lv, rv, msg ) { assert( lv === rv, msg ) },
  print: function(v) {
    process.stdout.write(String(v));
  }
};

