var runtime = require('./runtime.js')

module.exports = {
  print: function(v) {
    console.log(runtime);
    process.stdout.write(String(v));
  }
};

