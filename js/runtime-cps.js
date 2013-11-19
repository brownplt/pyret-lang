var PYRET_CPS = (function () {

  function makeRuntime() {
    function PObject(d) {
      this.dict = d;
    }
    function makeObject(d) { return new PObject(d); }
    function isObject(v) { return v instanceof PObject; }
    PObject.prototype = {
      app: function() { throw "Cannot apply object." }
    };

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
      _plus: makeMethod(function(k, f, left, right) {
        return makeNumber(left.n + right.n);
      }),
      _minus: makeMethod(function(left, right) {
        return makeNumber(left.n - right.n);
      }),
      _times: makeMethod(function(left, right) {
        return makeNumber(left.n * right.n);
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

    function toRepr(val) {
      if(isNumber(val)) {
        return makeString(String(val.n));
      }
      else if (isString(val)) {
        return makeString('"' + val.s + '"');
      }
      else if (isFunction(val)) {
        return makeString("fun: end");
      }
      else if (isMethod(val)) {
        return makeString("method: end");
      }
      else if (isObject(val)) {
        return makeString("{in: complete}");
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
    function testPrint(k, val) {
      var str = toRepr(val).s;
      console.log("testPrint: ", val, str);
      testPrintOutput += str + "\n";
      // Should be applyFunc, or whatever your implementation calls it
      k.app(val);
    }

    function NormalResult(val, namespace) {
      this.val = val;
      this.namespace = namespace;
    }
    function makeNormalResult(val, ns) { return new NormalResult(val, ns); }

    function FailResult(exn) {
      this.exn = exn;
    }
    function makeFailResult(exn) { return new FailResult(exn); }

    function PyretException(exnVal) {
      this.exnVal = exnVal;
    }
    function makePyretException(exnVal) {
      return new PyretException(exnVal);
    }

    function errToJSON(exn) {
      return JSON.stringify({exn: String(exn)})
    }

    // TODO(students): Make sure this returns a JavaScript dictionary with
    // the same contents as the Pyret dictionary (your field name may not
    // be dict, or there may be more work to do here, depending on your
    // representation).
    function pyretToJSDict(v) {
      return v.dict;
    }

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
    function start(fun, runtime, namespace, onDone) {
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
      run(function() { fun(runtime, namespace, onDone); });
    }

    function requestPause(k) {
      pauseRequested = true;
      currentPause = k;
    }

    return {
      start: start,
      requestPause: requestPause,
      namespace: Namespace({
        nothing: {},
        "test-print": makeFunction(testPrint),
        brander: makeFunction(function() {
          throw "brander NYI";
        }),
        "check-brand": makeFunction(function() {
          throw "check-brand NYI";
        }),
        Function: makeFunction(function() {
          throw "function NYI";
        }),
        builtins: "Not yet implemented"
      }),
      runtime: {
        makeNumber: makeNumber,
        makeObject: makeObject,
        makeFunction: makeFunction,
        isNumber: isNumber,
        equal: equal,
        getField: getField,
        getTestPrintOutput: function(val) {
          return testPrintOutput + toRepr(val).s;
        },
        NormalResult: NormalResult,
        FailResult: FailResult,
        PyretException: PyretException,
        makeNormalResult: makeNormalResult,
        makeFailResult: makeFailResult,
        makePyretException: makePyretException,
        toReprJS: toRepr,
        errToJSON: errToJSON,
        pyretToJSDict: pyretToJSDict
      }
    };
  }

  return {
    makeRuntime: makeRuntime
  };
})();

