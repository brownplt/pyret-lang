// This file is for you to edit, and add new predicates or comparisons that are
// specific to your representation of Pyret values.  You can add corresponding
// kinds of TestPredicates in create-tests.arr to correspond to different
// shapes of calls if you want, as well.

function pyretEquals(RUNTIME, pyretVal1, pyretVal2) {
  return RUNTIME.equal(pyretVal1, pyretVal2);
}

function isNumber(RUNTIME, pyretVal) {
  return RUNTIME.isNumber(pyretVal.val);
}


function testEquals(name, pyretProg1, pyretProg2) {
  return {
    name: name,
    test: function(RUNTIME) {
      var result1 = pyretProg1(RUNTIME);
      var result2 = pyretProg2(RUNTIME);
      if (result1 instanceof RUNTIME.NormalResult &&
          result2 instanceof RUNTIME.NormalResult) {
        expect(result1.val).toBePyretEqual(result2.val, RUNTIME);
      }
    }
  };
}

function testPred(name, pyretProg, pred) {
  return {
    name: name,
    test: function(RUNTIME) {
      expect(pyretProg(RUNTIME)).toSatisfyPyretPred(pred, RUNTIME);
    }
  };
}

function testPrint(name, pyretProg, output) {
  return {
    name: name,
    test: function(RUNTIME) {
      var result = pyretProg(RUNTIME);
      if (result instanceof RUNTIME.NormalResult) {
        var stdout = RUNTIME.getTestPrintOutput(result.val);
        expect(stdout).toEqual(output["expected-out"]);
        expect("").toEqual(output["expected-err"])
      }
      else if (result instanceof RUNTIME.FailResult) {
        expect(stdout).toEqual(output.expected);
        expect(RUNTIME.errToJSON(result.exn)).toEqual(output["expected-err"]);
      }
    }
  }
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
