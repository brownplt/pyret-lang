if(process.argv.length < 3){
	console.log('Please specify Pyret programs via commandline argument.');
} else {	
	var b = require('benchmark-pyret');
	var Q = require('q');
	var defer = Q.defer();
	var files = process.argv.slice(2);

	var i = 0;

	defer.promise.then(
		function(v){throw new Error('resolve should not happen');},
		function(v){throw new Error('reject should not happen');},
		function(notifyValue){
			if(i < files.length){
				b.runFile(files[i], {}, true, function(data){
					i++;
					defer.notify();
				});	
			}
		}
	);

	defer.notify();	
}
