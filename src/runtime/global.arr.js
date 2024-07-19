//
// BIG NOTE(alex): Do NOT "require()" [directly OR transitively] any builtin module in runtime-arr-stage-1
//    It creates a cyclic dependency while compiling runtime-arr-stage-1 Pyret code.
//    This causes a compilation failure b/c the compiler will try to find its compiled version
//      in the "build/runtime" directory and fail.
// To lift this restriction, see the note for "copy-js-dependencies()" in cli-module-loader.arr
//


var NUMBER = require("./js-numbers.js");
var runtime = require('./runtime.js');

function timeNow() {
  return new Date().getTime();
}

var realMakeReactor = null;


function makeSome(v) {
  const option = require('./option.arr.js');
  return option.some(v);
}

function makeNone() {
  const option = require('./option.arr.js');
  return option.none;
}

function makeReactor(init, fields) {
  return realMakeReactor(init, fields);
}

runtime.makeSome = makeSome;
runtime.makeNone = makeNone;
runtime.makeReactor = makeReactor;

module.exports = {
  makeSome,
  makeNone,
  makeReactor,
  $setMakeReactor: (f) => {
    realMakeReactor = f;
  },
  'run-task' : runtime.$runTask,
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

  'equal-now': runtime['$equalNow'],
  'equal-now3': runtime['$equalNow3'],
  'equal-always': runtime['$equalAlways'],
  'equal-always3': runtime['$equalAlways3'],
  'identical': runtime['$identical'],
  'identical3': runtime['$identical3'],
  
  'to-repr': runtime["$torepr"],
  'to-string': runtime["$tostring"],
  'to-output': runtime["$tooutput"],

  '$traceValue': runtime['$traceValue'],

  'is-boolean': runtime['$isBoolean'],
  'is-function': runtime['$isFunction'],

   // TODO(alex): Think of better way to expose runtime
  'runtime': runtime,

  'raise': runtime["raise"],

  'debug': runtime["debug"],
};
