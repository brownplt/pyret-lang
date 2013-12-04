function makeRuntime() {
  function TrampolineException(k) {
    this.k = k;
  }
  function trampoline(k) { throw new TrampolineException(k); }
  var runtime = {
    trampoline: trampoline,
    onDone: function(v) {
      console.log("Success: ", v);
    }
  };

  function PauseAction(onPause) {
    this.onPause = onPause; 
  }

  var pauseRequested = false;
  var nopause = function() { throw "No current pause"; }
  var currentPause = nopause;
  function start(src, extraLibs) {
    function nextTurn(k) { setTimeout(k, 0); }
    function run(k) {
      try {
        k();
      } catch(e) {
        console.log("Caught ", e);
        if(e instanceof TrampolineException) {
          if(!pauseRequested) {
            nextTurn(function() { run(e.k); });
          }
          else {
            var restart = function() { run(e.k); };
            pauseRequested = false;
            var thisPause = currentPause;
            currentPause = nopause;
            nextTurn(function() { thisPause(restart); });
          }
        }
        else {
          console.error("[start] Uncaught exception: ", e);
        }
      }
    }
    var fun = (1,eval)("(function(runtime, libs) { " + src + " })");
    run(function() { fun(runtime, extraLibs); });
  }

  function requestPause(k) {
    pauseRequested = true;
    currentPause = k;
  }

  return {
    start: start,
    requestPause: requestPause
  };
}
