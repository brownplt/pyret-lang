var PYRET_CPS = (function () {

  function makeRuntime() {
    //Base of all objects
    //DEF
    /*************************
    *       Base
    *************************/
    function PBase() {}
    function isPBase(v) { return v instanceof PBase; }
    PBase.prototype = {
      dict: {},
      brands: [],
      app: (function() { throwPyretMessage("Cannot apply this data type");}),
      type : 'base',
      extendWith : extendWith,
    };

    /*
        Extends an object with the new fields in fields
        If all the fields are new, the brands are kept,
        otherwise, the extended object has no brands

        fields: a PObj whose fields will be added to the Pyret base
        If any of the fields exist, they will be overwritten with the new value
    */
    function extendWith(fields) {
        var newObj = this.clone();
        var allNewFields = true;
        for(var field in fields) {
            if(newObj.dict.hasOwnProperty(field)) {
               allNewFields = false;
            }
            newObj.dict[field] = fields[field];
        }
        newObj.brands = [];
        if(allNewFields) {
            newObj.brands = this.brands.slice(0);
        }
        return newObj;
    }

    //DEF
    /*************************
    *       Nothing
    *************************/
    function PNothing(){}
    function makeNothing() {return new PNothing();}
    PNothing.prototype = Object.create(PBase.prototype);
    function isNothing(val) {return val instanceof PNothing;}


    function PMethod(f) {
      this.method = f;
    }
    function makeMethod(f) { return new PMethod(f); } 
    function isMethod(v) { return v instanceof PMethod; }
    PMethod.prototype = {
      app: function() { throw "Cannot apply method directly."; },
      dict: {}
    };


    function PFunction(f,arity) {
      this.app = f;
      this.brands = [];
      this.arity = arity;
    }
    function makeFunction(f,doc) { 
        var fun = new PFunction(f, f.length); 
        fun.dict ={};
        fun.dict._doc = doc;
        fun.dict._method = makeMethod(function(me) {
            return makeMethod(me.app, me.dict._doc);
        });
        return fun;
    }
    function isFunction(v) { return v instanceof PFunction; }
    PFunction.prototype = Object.create(PBase.prototype);
    PFunction.prototype.getType = (function() {return 'function';});
    PFunction.prototype.toString = function() {return 'fun(): end'}
    PFunction.prototype.clone = (function() {
        var newFun = makeFunction(this.app);
        return newFun;
    });

    //DEF
    /**********************************
    * Booleans
    ***********************************/
    function checkBothBool(arg1, arg2, fname) {
        typeCheck(arg1, isBoolean, arg2, isBoolean, fname);
        return;
    }

    var booleanDict = {
        _and : makeMethod(function(left, right) {
            if(!isBoolean(left)) {
                raiseTypeError(left, right, 'and');
            }
            if(left.b) {
                var rightVal = applyFunction(right,[]);
                if(!isBoolean(rightVal)) {
                    raiseTypeError(left, right, 'and');
                }
                return makeBoolean(rightVal.b);
            }
            else {
                return makeBoolean(false);
            }
        }),

        _or : makeMethod(function(left, right) {
            if(!isBoolean(left)) {
                raiseTypeError(left, right, 'or');
            }
            if(left.b) {
                return makeBoolean(true);
            }
            else {
                var rightVal = applyFunction(right,[]);
                if(!isBoolean(rightVal)) {
                    raiseTypeError(left, right, 'or');
                }
                return makeBoolean(rightVal.b);
            }
        }),

      _not: makeMethod(function(me) {
        checkIf(me, isBoolean, 'not');
        return makeBoolean(!(me.b));
      }),

      _tostring: makeMethod(function(me) {
        checkIf(b, isBoolean, 'toString');
       return toRepr(me); 
      }),   
      _torepr: makeMethod(function(me) {
       checkIf(b, isBoolean, 'torepr');
       return makeString(String(me.b)); 
      }),   
      _equals: makeMethod(function(me, other) {
        checkBothBool(me, other);
       return makeBoolean(me.b === other.b); 
      }),   
    };

    //Checks that something is a boolean, returns its boolean value
    function checkBool(b) {
        if(isBoolean(b)) {
            return b.b;
        }
        else {
            throwPyretMessage('check-bool: expected boolean, got ' + b.getType());
        }
    }
        
    function PBoolean(b) {
      this.b = b;
      this.brands = [];
    }
    function makeBoolean(b) { return new PBoolean(b); }
    function isBoolean(v) { return v instanceof PBoolean; }
    PBoolean.prototype = Object.create(PBase.prototype);
    PBoolean.prototype.dict = booleanDict;
    PBoolean.prototype.toString = (function() {return String(this.b);});
    PBoolean.prototype.clone = (function() {
        var newBool = makeBoolean(this.b);
        return newBool;
    });
    PBoolean.prototype.getType = function() {return String(this.b);};

    function equal(val1, val2) {
      if(isNumber(val1) && isNumber(val2)) {
        return val1.n === val2.n;
      }
      else if (isString(val1) && isString(val2)) {
        return val1.s === val2.s;
      }
      else if (isBoolean(val1) && isBoolean(val2)) {
        return val1.b === val2.b;
      }
      return val1 === val2;
    }

    /* DEF
        Pyret errors
    */
    function PyretException(exnVal) {
      this.exnVal = exnVal;
    }
    function makePyretException(exnVal) {
      return new PyretException(exnVal);
    }
    function throwPyretException(exnVal) {
      throw makePyretException(exnVal);
    }
    function throwPyretMessage(msg) {
      var eDict = {message : makeString(msg)};
      throwPyretException(makeObj(eDict));
    }
    
    
    var numberDict = {
      _plus: makeMethod(function(k, f, left, right) {
        applyFunction(k, [(makeNumber(left.n + right.n))]);
      }),
      _minus: makeMethod(function(k, left, right) {
        applyFunction(k, [(makeNumber(left.n - right.n))]);
      }),
      _times: makeMethod(function(k, left, right) {
        applyFunction(k, [(makeNumber(left.n * right.n))]);
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
      else if (isMethod(val)) {
        return makeString("method: end");
      }
      else if (isFunction(val)) {
        return makeString("fun(): end");
      }
      else if (isObj(val)) {
        if(val.dict.hasOwnProperty('_torepr')) {
            return applyFunction(getField(val, '_torepr'), []);
        }

        var fields = [];
        for(f in val.dict) {
            fields.push(f + ": " + toRepr(val.dict[f]).s)//val.dict[f].toString());
        }
        return makeString('{' +fields.join(", ")+ '}');
      }
      else if (isNothing(val)) {
        return makeString("nothing");
      }
      throw ("toStringJS on an unknown type: " + val);
    }

    function getField(val, str) {
      var fieldVal = val.dict[str];
      if (isMethod(fieldVal)) {
        var methFun = makeFunction(function() {
          var argList = Array.prototype.slice.call(arguments);
          var $k = argList[0];
          var $f = argList[1];

            //TODO: Make this CPS'y
            fieldVal.method.apply(null, [$k,$f ,val].concat(argList.slice(2)));
        });
    
        methFun.arity = fieldVal.method.length - 1;
        return methFun;
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
      applyFunction(k, []);
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

    /**
        Applies a function to the given list of arguments
        Performs arity checking (arrity!!) 

        fn: a PFunction to apply
        args: a list of arguments to apply to the function
    */
   function applyFunction(fn, args) {
        if(!isFunction(fn)) {
            throwPyretMessage("Cannot apply non-function: " + toRepr(fn));
        }
        if(args.length != fn.arity) {
            throwPyretMessage("Check arity failed: " + toRepr(fn) + " expected " + fn.arity + " arguments, but given " + args.length);
        }
        return fn.app.apply(this, args);
   }

    //DEF
    /**********************************
    * Objects
    ***********************************/
    function PObj(d) {
      this.dict = d;
      this.brands = [];
      //this.dict['_torepr'] = makeMethod(function(me) {
        //return toRepr(me);
      //});
    }
    function makeObj(b) { return new PObj(b); }
    function isObj(v) { return v instanceof PObj; }
    PObj.prototype = Object.create(PBase.prototype);
    PObj.prototype.clone = (function() {
        //We don't need to do deep cloning, but we *do* need to clone the dict
        var newDict = {};
        for(var key in this.dict) {
            newDict[key] = this.dict[key];
        }
        var newObj = makeObj(newDict);
        return newObj;
    });
    PObj.prototype.getType = function() {return 'object';};
    
    PObj.prototype.updateWith = function(fields) {
        var newObj = this; //Don't clone, this is mutation
        for(var field in fields) {
            if(isMutable(newObj.dict[field])) {
                newObj.dict[field].set(fields[field]);
            }
            else 
                throwPyretMessage("Attempted to update a non-mutable field");
        }
        return newObj;
    }
    PObj.prototype.toString = function() {
        var fields = "";
        for(var f in this.dict) {
            fields += f + ": " + dict[f].toString();
        }
        return '{' +fields+ '}';
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
        makeString: makeString,
        makeFunction: makeFunction,
        makeObj: makeObj,
        makeBoolean: makeBoolean,
        makeNothing: makeNothing,
        checkBool: checkBool,
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
        pyretToJSDict: pyretToJSDict,
        applyFunction : applyFunction
      }
    };
  }

  return {
    makeRuntime: makeRuntime
  };
})();

