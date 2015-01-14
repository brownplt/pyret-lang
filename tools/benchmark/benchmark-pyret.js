define(['js/runtime-anf', 'js/eval-lib', 'benchmark', 'q', 'fs'],

  function(RT, evalLib, Benchmark, Q, fs){

    //DO NOT REMOVE
    // needed for benchmarked functions to 
    // have access to evalLib
    global.evalLib = evalLib;

    function initializeGlobalRuntime(){
      global.rt = RT.makeRuntime({
        initialGas: 500,
        stdout: function(str) {},
        stderr: function(str) {}
      });
    }
    function parsePyret(deferred){
      global.evalLib.runParsePyret(global.rt,global.programSrc,global.pyretOptions,function(parsed){
        deferred.resolve(parsed);
      });   
    }

    function parsePyretSetup(){ 
      global.ast = global.evalLib.parsePyret(global.rt,global.programSrc,global.pyretOptions);      
    }

    function evaluatePyret(deferred){
      global.evalLib.runEvalParsedPyret(global.rt,global.ast,global.pyretOptions,function(result){        
        deferred.resolve(result);
      });
    }

    function compilePyret(deferred){  
      global.evalLib.runCompilePyret(global.rt,global.ast,global.pyretOptions,function(compiled){        
        deferred.resolve(compiled);
      });  
    }

    //TODO
    // figure out if we can easily separate out the portion of
    //  evaluation that happens after compliation
    function loadPyretSetup(){
      throw new Error('unimplemented');
    }

    function evaluateLoadedPyret(deferred){
      throw new Error('unimplemented');
    }



    function checkResult(runtime, result){
      if(runtime.isSuccessResult(result)) {
        return true;
      } else if (runtime.isFailureResult(result)) {
        return false;
      }
    }

    //run internally before each benchmark        
    function ensureSuccess(src, deferred){      
      var newRT = RT.makeRuntime({
        initialGas: 500,
        stdout: function(str) {},
        stderr: function(str) {}
      })
      global.evalLib.runEvalPyret(newRT, src, global.pyretOptions, function(result){ 
        var check = checkResult(newRT, result);
        if(check){
          deferred.resolve(check);
        }else{          
          deferred.reject(check);
        }
      }); 
    }

    function createSuite(){
      var suite = new Benchmark.Suite();

      suite.add('Parse    (src -> ast)', parsePyret, {'defer': true});
      suite.add('Compile  (ast -> js) ', compilePyret, {'setup': parsePyretSetup, 'defer': true});
      suite.add('Evaluate (ast -> res)', evaluatePyret, {'setup': parsePyretSetup, 'defer': true});     


      suite.on('cycle', function(event) {
        console.log(String(event.target).replace('\xb1', '+/- ')); 
      });      

      return suite;      
    }

    /**
    Runs the actual benchmarks
    @param {Array<Object>} tests is an array of objects
    of the form {program: str, name: str}, where program is
    the source code to test, and name is just a label (e.g. the program's name)
    @param options {Object} are the options passed directly into eval-lib
    @param onDone {Function} is the continuation
    */
    function runBenchmarks(tests, options, onDone){      
      initializeGlobalRuntime();

      global.ast = undefined;
      global.mod = undefined;
      global.pyretOptions = options;

      var suite = createSuite();
      var suiteRunDefer = Q.defer();

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
      
      var i = 0;
      suiteRunDefer.promise.then(
        function(v){throw new Error('resolve should not happen');},
        function(v){throw new Error('reject should not happen');},
        function(notifyValue){
          if(!notifyValue){
            console.log('There was an error in the benchmark.')
          }
          if(i < tests.length){
            console.log('CURRENT BENCHMARK: ' + tests[i].name);
            var ensureSuccessDefer = Q.defer();
            console.log('Ensuring program runs successfully...');
            ensureSuccess(tests[i].program, ensureSuccessDefer);
            ensureSuccessDefer.promise.then(
              function(v){
                console.log('...done.');
                global.programSrc = tests[i].program;
                suite.run({'async': false});
                i++;
                //suite.run will notify suiteRunDefer
              },
              function(v){
                console.log('...program did not run successfully. Moving on to next benchmark.\n');
                i++;
                suiteRunDefer.notify(true);
              },
              function(v){throw new Error('notify should not happen');}
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

    function testDeferredFunction(src, options, funName, onDone){
      initializeGlobalRuntime();
      var d = Q.defer();
      d.promise.then(
        function(result){
        if(checkResult(global.rt, result)){
          onDone(true);
        } else {
          onDone(false);
        }
        //onDone();
      },
      function(v){throw new Error('reject should not happen');},
      function(v){throw new Error('notify should not happen');});

      initializeGlobalRuntime()      
      global.programSrc = src;
      global.pyretOptions = options;

      switch(funName){
        case 'parsePyret': 
          parsePyret(d);
        break;
        case 'compilePyret':
          parsePyretSetup();
          compilePyret(d);
        break;
        case 'evaluatePyret':
          parsePyretSetup();
          evaluatePyret(d);
        break;
        default:
          throw new Error('Invalid Function Name: ' + funName);
      }
    }    

    function testEnsureSuccess(src, options, onDone){
      global.pyretOptions = options;
      var d = Q.defer();
      d.promise.then(
        function(resolveValue){onDone(true);},
        function(rejectValue){onDone(false);},
        function(v){throw new Error('notify should not happen');}
      )
      ensureSuccess(src, d);
    }

    function testInitializeGlobalRuntime(){
      global.rt = undefined;
      initializeGlobalRuntime();
      return (typeof global.rt !== 'undefined');
    }

    function testParsePyretSetup(src, options){
      global.ast = undefined;
      
      global.pyretOptions = options;      
      global.programSrc = src;
      initializeGlobalRuntime();

      parsePyretSetup();
      return (typeof global.ast !== 'undefined');
    }

    function testCheckResult(testSuccessResult){
      initializeGlobalRuntime();
      if(testSuccessResult){
        var result = global.rt.makeSuccessResult(undefined);
        return checkResult(global.rt, result);
      }else{
        var result = global.rt.makeFailureResult(undefined);
        return checkResult(global.rt, result);
      }
    }

    function testCreateSuite(){
      var SUITE_LENGTH = 3;
      var suite = createSuite();
      return (suite instanceof Benchmark.Suite && suite.length == SUITE_LENGTH);
    }

    return {
      runBenchmarks: runBenchmarks,      
      runFile: runFile,
      evaluateProgram: evaluateProgram,
      test: {
        testDeferredFunction: testDeferredFunction,
        testEnsureSuccess: testEnsureSuccess,
        testInitializeGlobalRuntime: testInitializeGlobalRuntime,
        testParsePyretSetup: testParsePyretSetup,
        testCheckResult: testCheckResult,
        testCreateSuite: testCreateSuite
      }
    };

  });