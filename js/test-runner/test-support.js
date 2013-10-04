// This file is for you to edit, and add new predicates or comparisons that are
// specific to your representation of Pyret values.  You can add corresponding
// kinds of TestPredicates in create-tests.arr to correspond to different
// shapes of calls if you want, as well.

// Patterns stolen from https://github.com/pivotal/jasmine/wiki/Matchers

function pyretEquals(RUNTIME, pyretVal1, pyretVal2) {
  return RUNTIME.equal(pyretVal1, pyretVal2);
}

function isNumber(RUNTIME, pyretVal) {
  return RUNTIME.isNumber(pyretVal);
}


function testEquals(name, pyretProg1, pyretProg2) {
  return {
    name: name,
    test: function(RUNTIME) {
      var val1 = pyretProg1(RUNTIME)();
      var val2 = pyretProg2(RUNTIME)();
      expect(val1).toBePyretEqual(val2, RUNTIME);
    }
  };
}

function testPred(name, pyretProg, pred) {
  return {
    name: name,
    test: function(RUNTIME) {
      expect(pyretProg(RUNTIME)()).toSatisfyPyretPred(pred, RUNTIME);
    }
  };
}


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
