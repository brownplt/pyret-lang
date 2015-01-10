define(['js/runtime-anf', 'js/eval-lib', 'benchmark', 'q'],

  function(RT, evalLib, Benchmark, Q){


    global.evalLib = evalLib;

    function parsePyret(deferred){
      global.evalLib.runParsePyret(global.rt,global.programSrc,{},function(parsed){
        deferred.resolve();
      });   
    }

    function parsePyretSetup(){ 
      global.ast = global.evalLib.parsePyret(global.rt,global.programSrc,{});
    }

    function evaluatePyret(deferred){
      global.evalLib.runEvalParsedPyret(global.rt,global.ast,{},function(result){
        deferred.resolve();
      });
    }


    function compilePyret(deferred){  
      global.evalLib.runCompilePyret(global.rt,global.ast,{},function(compiled){
        deferred.resolve();
      });  
    }

    function loadPyretSetup(){
      //global.mod = global.evalLib.loadParsedPyret(global.rt,global.ast,{});
      throw new Error('unimplemented');
    }

    function evaluateLoadedPyret(deferred){
      //todo?
      throw new Error('unimplemented');
    }

    function checkResult(runtime, result){
      if(runtime.isSuccessResult(result)) {
        return true;
      } else if (runtime.isFailureResult(result)) {
        return false;
      }
    }

    var runBenchmarks = function(tests){      
      global.rt = RT.makeRuntime({
        initialGas: 500,
        stdout: function(str) {},
        stderr: function(str) {}
      });

      global.ast = undefined;
      global.mod = undefined;
      //console.log('Setting up benchmark suite...');

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
          }
      });
      suiteRunDefer.notify(true);
    }

    function evaluateProgram(src){  
      console.log('Evaluating...');
      var newRT = RT.makeRuntime({
        initialGas: 500,
        stdout: function(str) {process.stdout.write(str); },
        stderr: function(str) {process.stderr.write(str); }
      })
      global.evalLib.runEvalPyret(newRT, src, {}, function(result){         
        //debugger;  
        var s = newRT.getField(result.result,'checks');
        debugger;
        console.log('done.');
      });  
    }

    function compileProgram(src){
      global.evalLib.runCompileSrcPyret(global.rt,src,{},function(compiled){    
      });
    }

    function ensureSuccess(src, deferred){
      console.log('Ensuring program runs successfully...');
      var newRT = RT.makeRuntime({
        initialGas: 500,
        stdout: function(str) {},
        stderr: function(str) {}
      })
      global.evalLib.runEvalPyret(newRT, src, {}, function(result){ 
        console.log('...done.');
        var check = checkResult(newRT, result);
        if(check){
          deferred.resolve(check);
        }else{          
          deferred.reject(check);
        }
      }); 
    }

    return {
      runBenchmarks: runBenchmarks
    };

  });