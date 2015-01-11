if(process.argv.length <= 2){
	console.log('Please specify at least one Pyret program via commandline argument.');
} else {	
	var fs = require('fs');
	var b = require('benchmark-pyret');

	var filenames = process.argv.slice(2);
	var benchmarks = new Array(filenames.length);
	for(var i = 0; i < filenames.length; i++){
		var programSrc = fs.readFileSync(filenames[i], {'encoding': 'utf8'});
		benchmarks[i] = {
			program: programSrc,
			name: filenames[i]
		}
	}
	b.runBenchmarks(benchmarks, {}, function(){});
}