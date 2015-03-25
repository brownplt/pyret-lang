if (process.argv.indexOf('--help') >= 0 || process.argv.indexOf('-h') >= 0) {
  console.log('Options:');
  console.log('-h, --help\tprint this message');
  console.log('--type-check\tUse the type-checker');
  console.log(
    'In the resultant data,\n\t \'hz\' means \'hertz\' (i.e. ops/sec), \
    \n\t \'rme\' means \'relative margin of error\' (i.e. reliability), and \
    \n\t \'samples\' just means the number of times the function was run.'
  );
} else {

  var typeCheck = (process.argv.indexOf('--type-check') >= 0);

  var b = require('benchmark-pyret');
  var fs = require('fs');
  var Q = require('q');

  var dir = 'auto-report-programs';
  var files = fs.readdirSync(dir);
  var defer = Q.defer();

  var i = 0;

  var print_to_stdout = function (data) {
    console.log('name:', data[0].name);
    console.log('success:', data[0].success);
    if (data[0].success) {
      console.log('parse:', data[0].results.parse);
      console.log('load:', data[0].results.load);
      console.log('eval_loaded:', data[0].results.eval_loaded);
      console.log('\n');
    }
  };

  defer.promise.then(
    function (v) {throw new Error('resolve should not happen'); },
    function (v) {throw new Error('reject should not happen'); },
    function (notifyValue) {
      if (i < files.length) {
        b.runFile(dir + '/' + files[i], {'typeCheck': typeCheck}, false,
          function (data) {
            print_to_stdout(data);
            i++;
            defer.notify();
          });
      }
    }
  );

  defer.notify();

}
