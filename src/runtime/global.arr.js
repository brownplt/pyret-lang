var runtime = require('./runtime.js');
var array = require('./array.js');

module.exports = {
  'raw-array': array['raw-array'],
  print: function(v) {
    console.log(runtime);
    process.stdout.write(String(v));
  }
};

