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

    function equal(val1, val2) {
      if(isNumber(val1) && isNumber(val2)) {
        return val1.n === val2.n;
      }
      return false;
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

    return {
      nothing: {},
      makeNumber: makeNumber,
      isNumber: isNumber,
      equal: equal,
      getField: getField
    }
  }

  return {
    makeRuntime: makeRuntime
  };
})();

