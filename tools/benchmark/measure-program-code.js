if(process.argv.length == 2){
	console.log('Please specify a Pyret program via commandline argument.');
} else {	
	var b = require('benchmark-pyret');
	
	var filename = process.argv.slice(2)[0];
	b.runFile(filename, {}, true, function(){});
}