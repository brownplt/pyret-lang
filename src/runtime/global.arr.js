var runtime = require('./runtime.js');
var array = require('./array.js');

function numToString(n) {
  return String(n);
}

module.exports = {
  'num-to-str': numToString,

  'raw-array': array['raw-array'],
  'display-string': function(s) { process.stdout.write(s); },
  "console-log": function(v) { console.log(v); },
  print: function(v) {
    process.stdout.write(String(v));
  }
};

