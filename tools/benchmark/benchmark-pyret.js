define(['js/runtime-anf', 'js/eval-lib', 'benchmark', 'q', 'fs'],

  function(RT, evalLib, Benchmark, Q, fs){

    //DO NOT REMOVE
    // needed for benchmarked functions to 
    // have access to evalLib
    global.evalLib = evalLib;

    function parsePyret(deferred){
      global.evalLib.runParsePyret(global.rt,global.programSrc,global.pyretOptions,function(parsed){
        deferred.resolve();
      });   
    }

    function parsePyretSetup(){ 
      global.ast = global.evalLib.parsePyret(global.rt,global.programSrc,global.pyretOptions);
    }

    function evaluatePyret(deferred){
      global.evalLib.runEvalParsedPyret(global.rt,global.ast,global.pyretOptions,function(result){
        deferred.resolve();
      });
    }

    function compilePyret(deferred){  
      global.evalLib.runCompilePyret(global.rt,global.ast,global.pyretOptions,function(compiled){
        deferred.resolve();
      });  
    }

    //TODO
    // figure out if we can easily separate out the portion of
    //  evaluation that happens after compliation
    function loadPyretSetup(){
      //global.mod = global.evalLib.loadParsedPyret(global.rt,global.ast,global.pyretOptions);
      throw new Error('unimplemented');
    }
    function evaluateLoadedPyret(deferred){
      throw new Error('unimplemented');
    }

    //used by ensureSuccess
    function checkResult(runtime, result){
      if(runtime.isSuccessResult(result)) {
        return true;
      } else if (runtime.isFailureResult(result)) {
        return false;
      }
    }

    //run internall before each benchmark        
    function ensureSuccess(src, deferred){
      console.log('Ensuring program runs successfully...');
      var newRT = RT.makeRuntime({
        initialGas: 500,
        stdout: function(str) {},
        stderr: function(str) {}
      })
      global.evalLib.runEvalPyret(newRT, src, global.pyretOptions, function(result){ 
        console.log('...done.');
        var check = checkResult(newRT, result);
        if(check){
          deferred.resolve(check);
        }else{          
          deferred.reject(check);
        }
      }); 
    }

    function runBenchmarks(tests, options, onDone){      
      global.rt = RT.makeRuntime({
        initialGas: 500,
        stdout: function(str) {},
        stderr: function(str) {}
      });

      global.ast = undefined;
      global.mod = undefined;
      global.pyretOptions = options;

      var suite = new Benchmark.Suite();

      //suite.add('Just Resolve Deffered Object', function(deferred){deferred.resolve();}, {'defer': true});
      suite.add('Parse    (src -> ast)', parsePyret, {'defer': true});
      suite.add('Compile  (ast -> js) ', compilePyret, {'setup': parsePyretSetup, 'defer': true});
      suite.add('Evaluate (ast -> res)', evaluatePyret, {'setup': parsePyretSetup, 'defer': true});
      //suite.add('Evaluate (js -> res)', evaluateLoadedPyret, {'setup': loadPyretSetup, 'defer': true});


      suite.on('cycle', function(event) {
        console.log(String(event.target).replace('\xb1', '+/- ')); 
      });

      suite.on('complete', function() {
        console.log('Fastest is ' + this.filter('fastest').pluck('name'));
        console.log('Slowest is ' + this.filter('slowest').pluck('name'));
        console.log('\n');
        suiteRunDefer.notify(true);
      });

      suite.on('error', function(event) {  
        console.log(event.target.error); 
        suiteRunDefer.notify(false);
      });

      var suiteRunDefer = Q.defer();
      

      var i = 0;
      suiteRunDefer.promise.then(
        function(v){},
        function(v){},
        function(notifyValue){
          if(!notifyValue){
            console.log('There was an error in the benchmark.')
          }
          if(i < tests.length){
            console.log('CURRENT BENCHMARK: ' + tests[i].name);
            var ensureSuccessDefer = Q.defer();
            ensureSuccess(tests[i].program, ensureSuccessDefer);
            ensureSuccessDefer.promise.then(
              function(v){
                global.programSrc = tests[i].program;
                suite.run({'async': false});
                i++;
                //suite.run will notify suiteRunDefer
              },
              function(v){
                console.log('Program did not run successfully. Moving on to next benchmark.\n');
                i++;
                suiteRunDefer.notify(true);
              },
              function(v){}
            );        
          }else{
            onDone();
          }
      });
      suiteRunDefer.notify(true);
    }

    // doesn't benchmark anything,
    // just for development/testing purposes
    //TODO:
    // output results of checker
    function evaluateProgram(src, options){  
      console.log('Evaluating...');
      var newRT = RT.makeRuntime({
        initialGas: 500,
        stdout: function(str) {process.stdout.write(str); },
        stderr: function(str) {process.stderr.write(str); }
      })
      global.evalLib.runEvalPyret(newRT, src, options, function(result){         
        console.log('done.');
      });  
    }

    function runFile(filename, options, onDone){
      var programSrc = fs.readFileSync(filename, {'encoding': 'utf8'});
      benchmarks = [{
        program: programSrc,
        name: filename
      }];
      runBenchmarks(benchmarks, options, onDone);
    }

    return {
      runBenchmarks: runBenchmarks,
      evaluateProgram: evaluateProgram,
      runFile: runFile
    };

  });