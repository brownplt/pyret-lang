### Setup

Note: all commands assume a current working directory of `<repo>/tools/benchmark`. Also, before using, ensure phase1 is built.

Benchmark tool uses [benchmark.js v1.0.0](http://benchmarkjs.com/) to measure Pyret performance, which in turn also depends on platform.js and lodash.js. These should be set up when installing Pyret, as they are listed as dependencies.

One run the tests of these benchmarking tools from the tools/benchmark directory via `node tests` to make sure nothing is broken, but these are also run when running `make test` when setting up Pyret, so one can also just not worry about it.

### Profile a Pyret Program

To measure the performance of (a) Pyret program(s) from a file, simply run

`node measure-program <filename0> <filename1> ... <filename_n>`

#### Programmatic Options
To measure the performance of programs which are Javascript strings, it may be easiest to copy examples.js and examples-code.js, editing the latter, then running examples.js in node.

For an example of something with options passed into the evaluator, check out type-check-compare.js and type-check-compare-code.js (run the former in node).

### What's Going On?

At the moment, we are measuring parsing (source -> ast), loading (ast -> js), and evaluating (js -> result) programs with eval-lib.js.

When a program is benchmarked, you'll see something like this:

```
CURRENT BENCHMARK: longmap2.arr  
Ensuring program runs successfully...  
...done.  
parse x 127 ops/sec +/- 6.48% (72 runs sampled)  
load x 2.38 ops/sec +/- 1.25% (16 runs sampled)  
eval_loaded x 39.38 ops/sec +/- 184.01% (33 runs sampled)  
Fastest is parse  
Slowest is load
```  

(This was from measuring the program 'range(0,10000).map(lam(x): x + 1 end)')

The number immediately preceding 'ops/sec' is the main thing to focus on. The higher this number, the faster that function ran. In order to be as accurate, benchmark.js runs the given function as many times as it can within some elapsed time, measuring each run indivudually. Then it samples some subset of the measurements and gives back a mean with a percentage uncertainty. For information, see this StackOverflow post: [http://stackoverflow.com/a/4996963](http://stackoverflow.com/a/4996963)

### Auto Report
Run `./auto-report.run` from `tools/benchmark`; this will create a file auto-report.csv, which will roughly look like this:
```
name,success,function,hz,rme,samples
0_empty.arr,true,parse,249.6111453557809,1.6570118309046231,37
0_empty.arr,true,load,8.873037616118994,5.045351099815703,46
0_empty.arr,true,eval,251.99298336529355,10.120326585442323,76
1_empty-with-comments.arr,true,parse,8.037688242099486,7.707525046697203,23
1_empty-with-comments.arr,true,load,6.841507799082858,8.419724334489054,39
1_empty-with-comments.arr,true,eval,222.18062488593227,12.99515319300458,59
...
```

For example, the sixth line is the measurement of loading the ast into javascript after parsing 0_empty.arr. It was rated at roughly 6.84Hz with a relative margin of error of roughly 8.42%; it sampled 39 runs to determine these data.

#### Testing progress
The build phase that the auto-report measures is determined in the file auto-report.js. To compare benchmarks of phase1 and phase2, first run
```
node auto-report > phase1.csv
```
then, edit the paths in auto-report.js, replacing all instances of 'phase1' with 'phase2'. Then run
```
node auto-report > phase2.csv
```
Now you have auto reports to compare!

### Visualization
See this separate repo: [pyret-bencmark-visualization](https://github.com/awstlaur/pyret-benchmark-visualization).
