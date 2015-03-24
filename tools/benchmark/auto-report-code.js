var b = require('benchmark-pyret');
var fs = require('fs');
var Q = require('q');

var dir = 'auto-report-programs';
var files = fs.readdirSync(dir);
var defer = Q.defer();

var i = 0;

defer.promise.then(
	function(v){throw new Error('resolve should not happen');},
	function(v){throw new Error('reject should not happen');},
	function(notifyValue){
		if(i < files.length){
			b.runFile(dir + '/' + files[i], {}, false, function(data){
				console.log('name:', data[0].name);
				console.log('success:', data[0].success)
				if(data[0].success){
					console.log('parse:', data[0].results.parse);
					console.log('load:', data[0].results.load);
					console.log('eval_loaded:', data[0].results.eval_loaded);
					console.log('\n');
				}
				i++;
				defer.notify();
			});	
		}
	}
);

defer.notify();