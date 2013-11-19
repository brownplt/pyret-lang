// This file is for you to edit, and add new predicates or comparisons that are
// specific to your representation of Pyret values.  You can add corresponding
// kinds of TestPredicates in create-tests.arr to correspond to different
// shapes of calls if you want, as well.

function pyretEquals(RUNTIME, pyretVal1, pyretVal2) {
  return RUNTIME.runtime.equal(pyretVal1, pyretVal2);
}

function isNumber(RUNTIME, pyretVal) {
  return RUNTIME.runtime.isNumber(pyretVal.val);
}


function testEquals(name, pyretProg1, pyretProg2) {
  return {
    name: name,
    test: function(RUNTIME) {
      var result1 = pyretProg1(RUNTIME.runtime, RUNTIME.namespace);
      var result2 = pyretProg2(RUNTIME.runtime, RUNTIME.namespace);
      if (result1 instanceof RUNTIME.runtime.NormalResult &&
          result2 instanceof RUNTIME.runtime.NormalResult) {
        expect(result1.val).toBePyretEqual(result2.val, RUNTIME);
      }
    }
  };
}

function testPred(name, pyretProg, pred) {
  return {
    name: name,
    test: function(RUNTIME) {
      expect(pyretProg(RUNTIME.runtime, RUNTIME.namespace)).toSatisfyPyretPred(pred, RUNTIME);
    }
  };
}


function checkOutput(RUNTIME, result, output) {
  if (result instanceof RUNTIME.runtime.NormalResult) {
    var stdout = RUNTIME.runtime.getTestPrintOutput(result.val);
    expect(stdout).toEqual(output["expected-out"]);
    expect("").toEqual(output["expected-err"])
  }
  else if (result instanceof RUNTIME.runtime.FailResult) {
    expect(stdout).toEqual(output.expected);
    expect(result.exn instanceof RUNTIME.runtime.PyretException).toEqual(true, "JS threw a non-Pyret exception: " + jasmine.pp(result.exn));
    expect(output["expected-err"]).not.toEqual("", "JS threw an error but Pyret did not: " + jasmine.pp(result));
  } else {
    fail("Runtime didn't produce a NormalResult or a FailResult");
  }
}

function testPrintNormal(name, pyretProg, output) {
  return {
    name: name,
    test: function(RUNTIME) {
      var result = pyretProg(RUNTIME.runtime, RUNTIME.namespace);
      checkOutput(RUNTIME, result, output);
    }
  }
}

function testPrintCPS(name, pyretProg, output, namespace) {
  return {
    cpsruntime: true,
    name: name,
    test: function(RUNTIME) {
      var namespaceToUse = namespace || RUNTIME.namespace;
      var response = false;
      RUNTIME.start(pyretProg, RUNTIME.runtime, namespace,
          {
            success: function(result) {
              response = true;
              console.log("Returned with result: ", result);
              checkOutput(RUNTIME, result, output);
            },
            failure: function(result) {
              response = true;
              console.log("Returned with failure: ", result);
              checkOutput(RUNTIME, result, output);
            }
          }
        );
      setTimeout(function() { if(!response) { console.error("Never returned"); }}, 500);
    }
  }
}

function testPrint(name, pyretProg, output, cps) {
  if (!cps) {
    return testPrintNormal(name, pyretProg, output);
  }
  else {
    return testPrintCPS(name, pyretProg, output);
  }
}

function testWithLibNormal(name, libProg, pyretProg, output) {
  return {
    name: name,
    test: function(RUNTIME) {
      var libResult = libProg(RUNTIME.runtime, RUNTIME.namespace);
      if (libResult instanceof RUNTIME.runtime.FailResult) {
        console.error("libResult failure on " + name + ":", libResult);
        throw new Error("Library ended in error (see console for failure object): " + name);
      }
      var newNamepsace = RUNTIME.namespace.merge(libResult.namespace);
      var progResult = pyretProg(RUNTIME.runtime, newNamepsace);
      checkOutput(RUNTIME, progResult, output);
    }
  };
}

function testWithLibCPS(name, libProg, pyretProg, output) {
  return {
    cpsruntime: true,
    name: name,
    test: function(RUNTIME) {
      var response = false;
      RUNTIME.start(libProg, RUNTIME.runtime, RUNTIME.namespace,
          {
            success: function(libResult) {
              response = true;
              var newNamespace = RUNTIME.namespace.merge(libResult.namespace);
              testPrintCPS(name, pyretProg, output, newNamespace).test(RUNTIME);
            },
            failure: function(result) {
              response = true;
              console.log("Returned with failure: ", result);
              throw new Error("Library ended in error (see console for failure object): " + name);
            }
          }
        );
      setTimeout(function() { if(!response) { console.error("Never returned"); }}, 500);
    }
  };
}

function testWithLib(name, libProg, pyretProg, output, cps) {
  if(!cps) { return testWithLibNormal(name, libProg, pyretProg, output); }
  else { return testWithLibCPS(name, libProg, pyretProg, output); }
}

// This just hooks things into Jasmine for pretty-printing the results.
// Feel free to add more types of test and hook them in here; equality and
// predicate testing should get you pretty far, however.
beforeEach(function() {
  function wrap(f) {
    return function() {
      return {
        compare: function() {
          return { pass: f.apply(this, arguments) };
        }
      };
    };
  }

  addMatchers({
    toBePyretEqual: wrap(function(actual, expected, RUNTIME) {
      return pyretEquals(RUNTIME, actual, expected);
    }),
    toSatisfyPyretPred: wrap(function(actual, pred, RUNTIME) {
      return pred(RUNTIME, actual);
    })
  });

});
