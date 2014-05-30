define(["js/ffi-helpers"], function(ffiLib) {

  function drawCheckResults(container, editor, runtime, checkResults) {
    var ffi = ffiLib(runtime, runtime.namespace);
    var checkResultsArr = ffi.toArray(checkResults);
    checkResultsArr.forEach(function(cr) {
      var name = runtime.getField(cr, "name");
      console.log("The name of the check block is: ", name, cr);
    });
    
  }

  return {
    drawCheckResults: drawCheckResults
  }

});
