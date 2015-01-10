Before using, ensure phase1 is built.


Benchmark tool uses [benchmark.js v1.0.0](http://benchmarkjs.com/) to measure Pyret performance. To get benchmark, run

$ npm install benchmark

To measure the performance of Pyret programs which are in *.arr files, simply run

$ node measure-program-wrapper <filename1> <filename2> ... <filenameN>

To measure the performance of programs which are Javascript strings, copy examples.js and examples-wrapper.js, running the latter in node. The data directory contains the output of these examples.

-----------------------------------------------------------------

At the moment, we are measuring parsing, compiling, and evaluating programs with eval-lib.js (specifically, we are using runParsePyret, runEvalParsedPyret, and runCompilePyret). The latter two functions use parsePyret as part of a setup phase, so the parsing won't be double-counted there.

When a program is benchmarked, you'll see something like this:

CURRENT BENCHMARK: insertion-sort.arr  
Ensuring program runs successfully...  
...done.  
Parse    (src -> ast) x 35.87 ops/sec +/- 0.77% (61 runs sampled)  
Compile  (ast -> js)  x 1.48 ops/sec +/- 6.25% (12 runs sampled)  
Evaluate (ast -> res) x 1.18 ops/sec +/- 13.15% (11 runs sampled)  
Fastest is Parse    (src -> ast)  
Slowest is Evaluate (ast -> res)

The number immediately preceding 'ops/sec' is the main thing to focus on. The higher this number, the faster that function ran. In order to be as accurate, benchmark.js runs the given function as many times as it can within some elapsed time, and measures each run indivudually. Then it samples some subset of the measurements and gives back a mean with a percentage uncertainty.