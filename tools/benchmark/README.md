### Setup

Note: all commands assume a current working directory of `<repo>/tools/benchmark`. Also, before using, ensure `phase1` is built.

Benchmark tool uses [benchmark.js v2.0.0](http://benchmarkjs.com/) to measure Pyret performance. This is listed as a Pyret dependency, so you should be all set.

### Profile a Pyret Program

To measure the performance of (a) Pyret program(s) from a file, simply run

`node measure-program <filename0> <filename1> ... <filename_n>`

#### Programmatic Options
To measure the performance of programs which are Javascript strings, it may be easiest to copy `examples.js` and `examples-code.js`, editing the latter, then running `examples.js` in node.

For an example of something with options passed into the evaluator, check out `type-check-compare.js` and `type-check-compare-code.js` (run the former in `node`).

### What's Going On?

At the moment, we are measuring parsing (source -> ast), loading (ast -> js), evaluating (js -> result), and, 
(as of this writing), running (source -> result) programs with `eval-lib.js`.

When a program is benchmarked, you'll see something like this:

```
CURRENT BENCHMARK: auto-report-programs/0_empty.arr
Ensuring program runs successfully...
...done.
parse x 3,600 ops/sec +/- 2.27% (661 runs sampled)
load x 11.19 ops/sec +/- 2.15% (612 runs sampled)
eval x 408 ops/sec +/- 9.16% (717 runs sampled)
all x 10.75 ops/sec +/- 2.08% (587 runs sampled)
Fastest is parse
Slowest is all
```  

The number immediately preceding 'ops/sec' is the main thing to focus on. The higher this number, the faster that function ran. In order to be as accurate, benchmark.js runs the given function as many times as it can within some elapsed time (at the time of this writing, 60 seconds), measuring each run indivudually. Then it samples some subset of the measurements and gives back a mean with a percentage uncertainty. For information, see this StackOverflow post: [http://stackoverflow.com/a/4996963](http://stackoverflow.com/a/4996963)

### Auto Report
Run `make auto-report` from `tools/benchmark`; this will write to the file auto-report.csv, which will roughly look like this:
```
name,success,function,hz,rme,samples
0_empty.arr,true,parse,6176.69175523141,1.244321068194214,494
0_empty.arr,true,load,15.427001119649182,1.4138665690206345,823
0_empty.arr,true,eval,948.8748480060468,2.1931220159104425,886
0_empty.arr,true,all,14.794606212340604,1.6641796375252562,791
1_empty-with-comments.arr,true,parse,13.194408241667716,0.3590716381664291,370
1_empty-with-comments.arr,true,load,14.414203448248402,2.244396359422877,777
1_empty-with-comments.arr,true,eval,863.3275004318019,2.7801930688818572,874
1_empty-with-comments.arr,true,all,5.528898651071109,2.8511964215648886,320
adding-ones-2000.arr,true,parse,0.6701637188890052,12.888863521237942,23
adding-ones-2000.arr,true,load,0.08960989307958635,6.2891684025681975,10
adding-ones-2000.arr,true,eval,72.12649051919735,2.0941564398375805,782
adding-ones-2000.arr,true,all,0.08706414040879977,1.9960447153817464,10
...
```

For example, the third line is the measurement of loading the ast into javascript after parsing [0_empty.arr](https://github.com/brownplt/pyret-lang/blob/master/tools/benchmark/auto-report-programs/0_empty.arr). It was rated at roughly 15.43Hz with a relative margin of error of roughly 1.41%; it sampled 823 runs to determine these data.

#### Visualization
Head over to the [visualization site](http://cs.brown.edu/~awstlaur/pyret-benchmark-visualization/), where you can upload either a single `.csv` file or select a [Jenkins build number](http://mainmast.cs.brown.edu/job/pyret-benchmark/) to visualize the auto report. Also, you can upload two files, or select two builds, to get a graph of containing both data sets.
