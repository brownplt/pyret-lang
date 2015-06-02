define(["q"], function(Q) {

  // Take a runtime as an argument or create one?
  function makeCompileLib(runtime, findModule) {

    var compileLibP = Q.defer();

    function compileWorklist(locator, context) {
      var worklistP = Q.defer();
      // traverse modules from locator and generate worklistP
      return worklistP.promise;
    }

    function compileProgram(worklist) {
      var compiledListP = Q.defer();
      // compile each element in worklist if necessary
      return compiledListP.promise;
    }

    function compileModule(locator, dependencies) {
      var compileResultP = Q.defer();
      // compile stuff and signal compileResultP
      return compileResultP.promise;
    }

    compileLibP.resolve({
      compileWorklist: compileWorklist,
      compileProgram: compileProgram,
      compileModule: compileModule
    });

    return compileLibP.promise;

  }

  return {
    makeCompileLib: makeCompileLib
  };

});

