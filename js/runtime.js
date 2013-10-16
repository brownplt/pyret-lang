var PYRET = (function () {

  function makeRuntime() {
    function PMethod(f) {
      this.method = f;
    }
    function makeMethod(f) { return new PMethod(f); } 
    function isMethod(v) { return v instanceof PMethod; }
    PMethod.prototype = Object.create(PBase.prototype);
    PMethod.prototype.app = function() { throw "Cannot apply method directly."; };
    PMethod.prototype.getType = function() {return 'method';};
    PMethod.prototype.clone = (function() {
        newMet = makeMethod(this.f);
        return newMet;
return     });
    PMethod.prototype.toString = function() {return 'fun ... end'}

    //Base of all objects
    function PBase() {}
    function isPBase(v) { return v instanceof PBase; }
    PBase.prototype = {
      dict: {},
      brands: [],
      app: (function() {throw "Cannot apply this data type";}),
      type : 'base'
    };

    //Checks to see that an object is a function and returns it, raises error otherwise
    function checkFun(o) {
        if(isFunction(o)) {return o;}
        throw 'check-fun: expected function, got ' + o.getType();
    }

    function PFunction(f) {
      this.app = f;
    }
    function makeFunction(f) { return new PFunction(f); }
    function isFunction(v) { return v instanceof PFunction; }
    PFunction.prototype = Object.create(PBase.prototype);
    PFunction.prototype.getType = (function() {return 'function';});
    PFunction.prototype.toString = function() {return 'fun: end'}
    PFunction.prototype.clone = (function() {
        var newFun = makeFunction(this.f);
        return newFun;
    });
        

    function typeCheck(arg1, type1, arg2, type2, name){
        if (!(type1(arg1) && type2(arg2))) {
            raiseTypeError(arg1,arg2, name);
        }
        return;
    }

    function raiseTypeError(arg1, arg2, name) {
        throw ("Bad args to prim: " + name +" : " + arg1.toString() + ", " + arg2.toString());
    }


    /**********************************
    * Numbers
    ***********************************/
    function checkBothNum(arg1, arg2, fname) {
        typeCheck(arg1, isNumber, arg2, isNumber, fname);
        return;
    }

    var numberDict = {
      _plus: makeMethod(function(left, right) {
        checkBothNum(left, right, 'plus');
        return makeNumber(left.n + right.n);
      }),
      _minus: makeMethod(function(left, right) {
        checkBothNum(left, right, 'minus');
        return makeNumber(left.n - right.n);
      }),
      _divide: makeMethod(function(left, right) {
        checkBothNum(left, right, 'divide');
        return makeNumber(left.n / right.n);
      }),
      _times: makeMethod(function(left, right) {
        checkBothNum(left, right, 'times');
        return makeNumber(left.n * right.n);
      }),
      _lessthan: makeMethod(function(left, right) {
        checkBothNum(left, right, 'lessthan');
        return makeBoolean(left.n < right.n);
      }),
      _greaterthan: makeMethod(function(left, right) {
        checkBothNum(left, right, 'greaterthan');
        return makeBoolean(left.n > right.n);
      }),
      _lessequal: makeMethod(function(left, right) {
        checkBothNum(left, right, 'lessequal');
        return makeBoolean(left.n <= right.n);
      }),
      _greaterequal: makeMethod(function(left, right) {
        checkBothNum(left, right, 'greaterequal');
        return makeBoolean(left.n >= right.n);
      }),
      max: makeMethod(function(left, right) {
        checkBothNum(left, right, 'max');
        return makeBoolean(Math.max(left.n, right.n));
      }),
      min: makeMethod(function(left, right) {
        checkBothNum(left, right, 'min');
        return makeBoolean(Math.min(left.n, right.n));
      }),
      abs: makeMethod(function(left, right) {
        checkBothNum(left, right, 'abs');
        return makeBoolean(Math.abs(left.n, right.n));
      }),
      modulo: makeMethod(function(left, right) {
        checkBothNum(left, right, 'modulo');
        return makeBoolean(left.n % right.n);
      }),
      tostring : makeMethod(function(me) {
        return makeString(String(me.n));
      }),
    };

    function PNumber(n) {
      this.n = n;
    }
    function makeNumber(n) { return new PNumber(n); }
    function isNumber(v) { return v instanceof PNumber; }
     PNumber.prototype = Object.create(PBase.prototype);
     PNumber.prototype.dict=  numberDict;    
     PNumber.prototype.toString = (function() {return String(this.n);});
     PNumber.prototype.clone = (function() {
        var newNum = makeNumber(this.n);
        return newNum;
     });
     PNumber.prototype.getType = function(){return 'number';};


    /**********************************
    * Strings
    ***********************************/

    var stringDict = {
      _plus: makeMethod(function(left, right) {
        return makeString(left.s + right.s);
      }),
      tostring : makeMethod(function(me) {
        return makeString(me.s);
      }),
    };

    function PString(s) {
      this.s = s;
    }
    function makeString(s) { return new PString(s); }
    function isString(v) { return v instanceof PString; }
    PString.prototype = Object.create(PBase.prototype);
    PString.prototype.dict = stringDict;
    PString.prototype.toString = (function() {return this.s;});
    PString.prototype.clone = (function() {
        var newStr = makeString(this.s);
        return newStr;
     });
    PString.prototype.getType = function() {return 'string';};

    /**********************************
    * Booleans
    ***********************************/
    var booleanDict = {
        _and : makeMethod(function(left, right) {
            if(!isBoolean(left)) {
                raiseTypeError(left, right, 'and');
            }
            if(left.b) {
                var rightVal = right.app();
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
                var rightVal = right.app();
                if(!isBoolean(rightVal)) {
                    raiseTypeError(left, right, 'or');
                }
                return makeBoolean(rightVal.b);
            }
        }),
    };

    //Checks that something is a boolean, returns its boolean value
    function checkBool(b) {
        if(isBoolean(b)) {
            return b.b;
        }
        else {
            throw 'check-bool: expected boolean, got ' + o.getType();
        }
    }
        
    function PBoolean(b) {
      this.b = b;
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

    function toRepr(val) {
      if(isNumber(val)) {
        return makeString(String(val.n));
      }
      else if (isString(val)) {
        return makeString('"' + val.s + '"');
      }
      else if(isBoolean(val)) {
        return makeString(String(val.b));
      }
      else if (isFunction(val)) {
        return makeString("fun: end");
      }
      else if (isMethod(val)) {
        return makeString("method: end");
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
      var str = toRepr(val).s;
      console.log("testPrint: ", val, str); testPrintOutput += str + "\n";
      return val;
    }

    function NormalResult(val) {
      this.val = val;
    }
    function makeNormalResult(val) { return new NormalResult(val); }

    function FailResult(exn) {
      this.exn = exn;
    }
    function makeFailResult(exn) { return new FailResult(exn); }

    function errToJSON(exn) {return exn;}

    /**********************************
    * Objects
    ***********************************/
    function PObj(d) {
      this.dict = d;
    }
    function makeObj(b) { return new PObj(b); }
    function isObj(v) { return v instanceof PObj; }
    PObj.prototype = Object.create(PBase.prototype);
    PObj.prototype.clone = (function() {
        var newObj = makeObj(this.dict);
        //Deep Clone, clone each field
        for(var f in newObj.dict) {
            newObj[f] = newObj.dict[f].clone();
        }
        return newObj;
    });
    PObj.prototype.getType = function() {return 'object';};
    PObj.prototype.extendWith = function(fields) {
        var newObj = this.clone();
        for(var field in fields) {
            newObj.dict[field] = fields[field];
        }
        return newObj;
    }
    PObj.prototype.toString = function() {return '{object}'}

    /**********************************
    * Builtins
    ***********************************/
    //Brander
    var brandCount = 0;
    brander = makeFunction(function() {
    var myBrand = brandCount++; 
    branderDict = {
        brand: makeFunction(function(toBrand) {
            var newO = toBrand.clone();
            newO.brands = toBrand.brands.slice(0);
            newO.brands.push(myBrand);
            return newO;
        }),
        test: makeFunction(function(o) {
            if(o.brands.indexOf(myBrand) != -1) {
                return makeBoolean(true);
            }
            else {return makeBoolean(false);}
        }),
    }
    return makeObj(branderDict);
    });

    //Raise
    raise = makeFunction(function(expr) {
        throw expr.toString();
    });

    return {
      nothing: {},
      makeNumber: makeNumber,
      makeString: makeString,
      makeBoolean: makeBoolean,
      isNumber: isNumber,
      isString: isString,
      isBoolean: isBoolean,
      checkBool: checkBool,
      
      makeFunction: makeFunction,
      isFunction: isFunction,
      checkFun: checkFun,

      makeMethod: makeMethod,
      isMethod: isMethod,

      makeObj: makeObj,
      isObj: isObj,
     
      //Builtins
      brander:brander,
      raise:raise,

      equal: equal,
      getField: getField,
      getTestPrintOutput: function(val) {
        return testPrintOutput + toRepr(val).s;
      },
      NormalResult: NormalResult,
      FailResult: FailResult,
      makeNormalResult: makeNormalResult,
      makeFailResult: makeFailResult,
      toReprJS: toRepr,
      errToJSON: errToJSON,

      ids:{},

      "test-print": makeFunction(testPrint),

      "is-number": makeFunction(function(x){return makeBoolean(isNumber(x));}),
      "is-string": makeFunction(function(x){return makeBoolean(isString(x));}),
      "is-bool": makeFunction(function(x){return makeBoolean(isBoolean(x));}),
      "is-function": makeFunction(function(x){return makeBoolean(isFunction(x));}),
      "is-method": makeFunction(function(x){return makeBoolean(isMethod(x));}),
    }
  }

  return {
    makeRuntime: makeRuntime
  };
})();

