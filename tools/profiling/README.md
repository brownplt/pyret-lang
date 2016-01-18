# Profiling Tools
## Setup
First, make sure you have [`node v0.12.X`](https://nodejs.org/en/download/releases/). The versions of V8 that come with `node v0.10.x` and `node v0.12.x` have different profiler APIs.

All commands should run from _this_ directory.

To install the dependencies, run:
```
make install
```
At the time of this writing, this only installs [v8-profiler](https://github.com/node-inspector/v8-profiler) from the npm repo.

## Use

### Quick Instructions
Build `phase3`. Run
```
make callgrind THIS_FILE=<file.arr>
```
where <file.arr> is in [tools/benchmark/auto-report-programs](https://github.com/brownplt/pyret-lang/tree/master/tools/benchmark/auto-report-programs). This will invoke the v8 Profiler on Pyret compiling and executing the program in `<file.arr>`. The result will be both the file `processed.cpuprofile`, and the file `callgrind.profile`, a file in the [Callgrind format](http://valgrind.org/docs/manual/cl-format.html). Use a tool like `kcachegrind` to visualize such `callgrind.profile` files individually. If you have two, run
```
node analyze-callgrind.js <callgrind1.profile> <callgrind2.profile>
```
The script will do its best to see which function calls are significantly different, and report some info to stdout. At the time of this writing, if the function name has a different source line number in each profile, it'll assume they are completely different functions.

If you wish, the `processed.cpuprofile` file can be loaded into Chrome to visualize. But I find the `kcachegrind` method much more informative.

`make clean` simply removes the two files.

### Detailed

Note: many examples to come call the GNU utility `make` with command-line options like `VAR=value`. These variables are defined in the Makefile itself, and such a command-line invocation overrides their values! You may also, of course, edit your local copy of the Makefile to suit your needs.

To get `.cpuprofile` data (like what Chrome gives), run:
```
make cpuprofile THIS_FILE=<file.arr>
```
where `THIS_FILE` is a file sitting inside [tools/benchmark/auto-report-programs](https://github.com/brownplt/pyret-lang/tree/master/tools/benchmark/auto-report-programs). If you want to use a file from elsewhere, pass in a value for `PYRET_FILE`. You can also specify a different phase to use (the default is phase3). Example:
```
make cpuprofile PYRET_FILE=</path/to/file.arr> PHASE=<phasex>
```

Because cpuprofile data is hard to deal with, we then want to convert this to the [Callgrind format](http://valgrind.org/docs/manual/cl-format.html). From here, if you have a `.cpuprofile` from a Chrome profiling session, it can stand in place of our generated one (just save it as `processed.cpuprofile` to meet the `make` dependencies).
```
make callgrind PHASE=<phasex>
```
Note: because of `make` dependencies, it will fail if `PHASE` is not built, even though this doesn't depend directly on the build.

The callgrind format will give a total number of `ms` (milliseconds) and `hits` (cpu samples) both overall, and per function call. It also shows parent-child relationships of function calls. For psychologically interpreting the data, install [kcachegrind](http://kcachegrind.sourceforge.net/) and open up the `callgrind.profile` there. It shows very nice orderings, groupings, cycle detection, relative vs absolute data, etc. If the build use to generate the `.cpuprofile` data is still on your file system, this tool can also give you a source code preview!

If you have two different `.profile` files from `make callgrind`, and you want to programmatically check the difference, run
```
node analyze-callgrind.js <callgrind1.profile> <callgrind2.profile>
```


#### Saving artifacts
`make archive-profile` and `make archive-cpuprofile` will respectively copy callgrind.profile and processed.cpuprofile into an `artifacts/$GIT_BRANCH.$PHASE` directory, where `PHASE` is as before. The file names will be `$THIS_FILE.callgrind.profile` and `$THIS_FILE.processed.cpuprofile`, where `THIS_FILE` is as before. `make archive-all` just runs both of these.

The shell script `auto-report.sh` reads the file `auto-report-jobs.txt` line-by-line for command-line arguments to `make auto-report`. This `make` target calls `make archive-profile` and cleans up the mess.

#### Typical use case
My typical use to _just_ call
```
make archive-profile THIS_FILE=<file.arr>
```
Or sometimes,
```
make archive-profile PYRET_FILE=</path/to/file.arr> THIS_FILE=<file.arr>
```
(Yes, the latter is redundant, but I currently don't feel like writing a separate script just to parse `$PYRET_FILE`.)

## What Is Happening?
The V8 Profiler takes many many samples per second of the state of the active function calls. So, if a particular function is a "hot spot", it'll show up with a higher number of hits. We take advantage of the fact that is able to be started and stopped programmatically, and only run it on a call to `runEvalPyret` (from `eval-lib.js`). This lets us exclude things like node/requirejs loading modules and the runtime creation.