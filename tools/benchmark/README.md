Before using, ensure phase1 is built.


Benchmark tool uses [benchmark.js v1.0.0](http://benchmarkjs.com/) to measure Pyret performance, which in turn also depends on platform.js (and lodash.js, but you need that for Pyret anyway). To get platform and benchmark, run

`npm install platform && npm install benchmark`

To measure the performance of a Pyret program from a file, simply run

`node measure-program <filename>`

To measure the performance of programs which are Javascript strings, it may be easiest to copy examples.js and examples-code.js, editing the latter, then running examples.js in node.

To run the tests, ensuring that the individual components run successfully, run tests.js in node.

For an example of something with options passed in, check out type-check-compare.js and type-check-compare-code.js (run the former in node).

-----------------------------------------------------------------

At the moment, we are measuring parsing (source -> ast), loading (ast -> js), and evaluating (js -> result) programs with eval-lib.js.

When a program is benchmarked, you'll see something like this:

CURRENT BENCHMARK: longmap2.arr

Ensuring program runs successfully...

...done.

parse x 127 ops/sec +/- 6.48% (72 runs sampled)

load x 2.38 ops/sec +/- 1.25% (16 runs sampled)

eval_loaded x 39.38 ops/sec +/- 184.01% (33 runs sampled)

Fastest is parse

Slowest is load

(This was from measuring the program 'range(0,10000).map(lam(x): x + 1 end)')

The number immediately preceding 'ops/sec' is the main thing to focus on. The higher this number, the faster that function ran. In order to be as accurate, benchmark.js runs the given function as many times as it can within some elapsed time, measuring each run indivudually. Then it samples some subset of the measurements and gives back a mean with a percentage uncertainty. For information, see this StackOverflow post: [http://stackoverflow.com/a/4996963](http://stackoverflow.com/a/4996963)
