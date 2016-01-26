define(['js/runtime-anf', 'js/eval-lib','fs','v8-profiler'],
function (RT, evalLib, fs, profiler) {

  var GAS = 500;
  

  function run(filename, options){

    var src = fs.readFileSync(filename, {'encoding': 'UTF-8'});

    var runtime = RT.makeRuntime({
      initialGas: GAS,
      stdout: function (str) {process.stdout.write(str); },
      stderr: function (str) {process.stderr.write(str); }
    });

/** START PROFILING **/

    profiler.startProfiling();

    evalLib.runEvalPyret(runtime, src, options, function(){

      var profile1 = profiler.stopProfiling();

/** STOP PROFILING **/

      profile1.export(function(error, result) {
        fs.writeFileSync('processed.cpuprofile', result);
        profile1.delete();
      });


    });
  }

  // args were checked in profiling.js
  run(process.argv[2], {});  
 

});