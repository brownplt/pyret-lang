var PYRET = (function () {

  function makeRuntime() {
    function PMethod(f) {
      this.method = f;
    }
    function makeMethod(f) { return new PMethod(f); } 
    function isMethod(v) { return v instanceof PMethod; }
    PMethod.prototype = {
      app: function() { throw "Cannot apply method directly."; },
      dict: {}
    };

    function PFunction(f) {
      this.app = f;
    }
    function makeFunction(f) { return new PFunction(f); }
    function isFunction(v) { return v instanceof PFunction; }
    PFunction.prototype = {
      dict: {} 
    };

    var numberDict = {
      _plus: makeMethod(function(left, right) {
        return makeNumber(left.n + right.n);
      })
    };

    function PNumber(n) {
      this.n = n;
    }
    function makeNumber(n) { return new PNumber(n); }
    function isNumber(v) { return v instanceof PNumber; }
    PNumber.prototype = {
      dict : numberDict
    };

    var stringDict = {
      _plus: makeMethod(function(left, right) {
        return makeString(left.s + right.s);
      })
    };

    function PString(s) {
      this.s = s;
    }
    function makeString(s) { return new PString(s); }
    function isString(v) { return v instanceof PString; }
    PString.prototype = {
      dict : stringDict
    };

    function equal(val1, val2) {
      if(isNumber(val1) && isNumber(val2)) {
        return val1.n === val2.n;
      }
      else if (isString(val1) && isString(val2)) {
        return val1.s === val2.s;
      }
      return false;
    }

    function toStringJS(val) {
      if(isNumber(val)) {
        return String(val.n);
      }
      else if (isString(val)) {
        return val.s;
      }
      else if (isFunction(val)) {
        return "fun: end";
      }
      else if (isMethod(val)) {
        return "method: end";
      }
      throw ("toStringJS on an unknown type: " + val);
    }

    function getField(val, str) {
      var fieldVal = val.dict[str];
      if (isMethod(fieldVal)) {
        return makeFunction(function() {
          var argList = Array.prototype.slice.call(arguments);
          return fieldVal.method.apply(null, [val].concat(argList));
        });
      } else {
        return fieldVal;
      }
    }

    var testPrintOutput = "";
    function testPrint(val) {
      var str = toStringJS(val);
      console.log("testPrint: ", val, str);
      testPrintOutput += str + "\n";
      return val;
    }

    return {
      nothing: {},
      makeNumber: makeNumber,
      isNumber: isNumber,
      equal: equal,
      getField: getField,
      "test-print": makeFunction(testPrint),
      getTestPrintOutput: function() { return testPrintOutput; }
    }
  }

  return {
    makeRuntime: makeRuntime
  };
})();

