var b = require('benchmark-pyret');


var benchmarks = 
[
{program: '', name: '"" (empty)'},
{program: '1', name: '"1"'},
{program: 'lam(x): x end', name: '"lam(x): x end"'}
];

console.log('WITH TYPE CHECKING');
b.runBenchmarks(benchmarks, {'typeCheck': true}, function(){
	console.log('WITHOUT TYPE CHECKING');
	b.runBenchmarks(benchmarks, {'typeCheck': false}, function(){});
});



