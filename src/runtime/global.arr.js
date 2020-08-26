//
// BIG NOTE(alex): Do NOT "require()" [directly OR transitively] any builtin module in runtime-arr-stage-1
//    It creates a cyclic dependency while compiling runtime-arr-stage-1 Pyret code.
//    This causes a compilation failure b/c the compiler will try to find its compiled version
//      in the "build/runtime" directory and fail.
// To lift this restriction, see the note for "copy-js-dependencies()" in cli-module-loader.arr
//


var runtime = require('./runtime.js');
var array = require('./array.js');

function timeNow() {
  return new Date().getTime();
}

var realMakeReactor = null;


// (Big Hack): Alias for require to stop the Pyret compiler's dependency
// tracing. Needed because requiring option introduces a circular dependency
// with runtime-arr-stage-1.
const untracedRequire = require;

function makeSome(v) {
  const option = untracedRequire('./option.arr.js');
  return option.some(v);
}

function makeNone() {
  const untracedRequire = require;
  const option = untracedRequire('./option.arr.js');
  return option.none;
}

module.exports = {
  makeSome,
  makeNone,
  makeReactor: (init, fields) => {
    return realMakeReactor(init, fields);
  },
  $setMakeReactor: (f) => {
    realMakeReactor = f;
  },
  'time-now' : timeNow,
  'js-to-string': function(v) { return String(v); },
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
  '_plus': runtime["_plus"],
  '_minus': runtime["_minus"],
  '_times': runtime["_times"],
  '_divide': runtime["_divide"],
  '_lessthan': runtime["_lessthan"],
  '_greaterthan': runtime["_greaterthan"],
  '_lessequal': runtime["_lessequal"],
  '_greaterequal': runtime["_greaterequal"],
  'not': runtime["_not"],

  'equal-now': runtime['equal-now'],
  'equal-now3': runtime['equal-now3'],
  'equal-always': runtime['equal-always'],
  'equal-always3': runtime['equal-always3'],
  'identical': runtime['identical'],
  'identical3': runtime['identical3'],

  'trace-value': runtime['traceValue'],

   // TODO(alex): Think of better way to expose runtime
  'runtime': runtime,

  'raise': runtime["raise"],

  'loop-a-while': function(n) {
    let s = 0;
    for (let i = 0; i < n; i += 1) {
      s += i;
    }
    return s;
  },

  'throwUnfinishedTemplate': function(srcloc) {
    throw {
      '$template-not-finished': srcloc,
    };
  },
};
