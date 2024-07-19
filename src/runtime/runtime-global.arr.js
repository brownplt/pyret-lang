// NOTE(alex): A stripped-down global to be used by runtime files
const runtime = require('./runtime.js');

function timeNow() {
  return new Date().getTime();
}

module.exports = {
  'nothing': runtime["nothing"],
  'time-now' : timeNow,
  'js-to-string': function(v) { return String(v); },
  'to-repr': runtime["$torepr"],
  'to-string': runtime["$tostring"],
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
  'print-error': function(v) {
    process.stderr.write(String(v));
  },
  '_plus': runtime["_plus"],
  '_minus': runtime["_minus"],
  '_times': runtime["_times"],
  '_divide': runtime["_divide"],
  '_lessthan': runtime["_lessthan"],
  '_greaterthan': runtime["_greaterthan"],
  '_lessequal': runtime["_lessequal"],
  '_greaterequal': runtime["_greaterequal"],
  'not': runtime["_not"],

  '$traceValue': runtime['traceValue'],

   // TODO(alex): Think of better way to expose runtime
  'runtime': runtime,

  'raise': runtime["raise"],

  'typecast': function(x) { return x; },
  'debug': runtime["debug"],

};
