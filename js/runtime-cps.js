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
    function checkBothBool(f, arg1, arg2, fname) {
        return typeCheck(f, arg1, isBoolean, arg2, isBoolean, fname);
    }

    var booleanDict = {
        _and : makeMethod(function(k, f,left, right) {
            if(!isBoolean(left)) {
                raiseTypeError(left, right, 'and');
                return;
            }
            if(left.b) {
                var rK = makeFunction(function(rightVal) {
                if(!isBoolean(rightVal)) {
                    raiseTypeError(f, left, right, 'and');
                    return;
                }
                    applyFunction(k, [makeBoolean(rightVal.b)]);
                });
                applyFunction(right, [rK, f]);
            }
            else {
               applyFunction(k,[ makeBoolean(false)]);
            }
        }),

        _or : makeMethod(function(k, f, left, right) {
            if(!isBoolean(left)) {
                raiseTypeError(left, right, 'or');
            }
            if(left.b) {
                applyFunction(k, [makeBoolean(true)]);
            }
            else {
                var rK = makeFunction(function(rightVal) {
                if(!isBoolean(rightVal)) {
                    raiseTypeError(f, left, right, 'and');
                    return;
                }
                    applyFunction(k, [makeBoolean(rightVal.b)]);
                });
                applyFunction(right, [rK, f]);
            }
        }),

      _not: makeMethod(function(k, f, me) {
        if(checkIf(f, me, isBoolean, 'not')) {
            applyFunction(k, [ makeBoolean(!(me.b))]);
        }
      }),

      _tostring: makeMethod(function(k, f, me) {
        if(checkIf(f, b, isBoolean, 'toString')){ 
       applyFunction(k, [ toRepr(me)]);
        }
      }),   
      _torepr: makeMethod(function(k, f, me) {
       if(checkIf(f, b, isBoolean, 'torepr')){
       applyFunction(k, [ makeString(String(me.b))]);
       }
      }),   
      _equals: makeMethod(function(k, f, me, other) {
        if(checkBothBool(f, me, other)) {
       applyFunction(k, [ makeBoolean(me.b === other.b)]);
        }
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
    function raisePyretException(f, exnVal) {
      applyFunction(f, [makePyretException(exnVal)]);
    }
    function raisePyretMessage(f, msg) {
      var eDict = {message : makeString(msg)};
      raisePyretException(f, makeObj(eDict));
    }
    function throwPyretMessage(msg) {
      var eDict = {message : makeString(msg)};
      throwPyretException(makeObj(eDict));
    }

    //Raise
    var raise = makeFunction(function(k, f, eVal) {
        applyFunction(f, [makePyretException(eVal)])
    });
    
    //Error
    errorDict = {
        'make-error' : makeFunction(function(k, f, s) {
            //Assuming s is a Pyret Exception
            applyFunction(k,[s.exnVal]);
        })
    };
    error = makeObj(errorDict);

    //Equiv
    /**
        equiv(obj1, obj2)

        Tests if two objects are equivalent.
        Uses obj1's _equals method if one exists
        Otherwise, recursively checks each field in the objects' dictionaries
    **/
    function equiv(k, f, obj1, obj2) {
        if(obj1.dict.hasOwnProperty("_equals")) {
             applyFunction(getField(obj1, "_equals"),[k, f, obj2]);
        }
        else { applyFunction(k, [makeBoolean(isAllSame(obj1, obj2))]);}
    }

    /**
      isAllSame(obj1, obj2)

      Checks that the objects have the same fields 
      Internal only, returns a JS Boolean
    **/
    function isAllSame(obj1, obj2) {
        if(isMethod(obj1) || isFunction(obj1)) {
            return false;
        }
        else if(Object.keys(obj1.dict).length !== Object.keys(obj2.dict).length) {return makeBoolean(false);}
       
        for(key in obj1.dict){
            if(obj2.dict.hasOwnProperty(key)) {
                if(!(equiv(obj1.dict[key], obj2.dict[key]).b)) {
                    return false;
                }
            }
            else {
                return false;
            }
        }

        return true;
    }  


    /**********************************
    * Type Checking
    ***********************************/
    //TODO: Make it so it throws to f
    function typeCheck (f, arg1, type1, arg2, type2, name){
        if (!(type1(arg1) && type2(arg2))) {
            raiseTypeError(f, arg1,arg2, name);
            return false;
        }
        return true;
    }

    function raiseTypeError(f, arg1, arg2, name) {
        raisePyretMessage(f, "Bad args to prim: " + name +" : " + arg1.toString() + ", " + arg2.toString());
    }

    function checkIf(f, arg1, type1, name) {
        if(!type1(arg1)) {
            raisePyretMessage(f, "Incorrect type of value for method \""+name+"\": " + arg1.toString());
            return false;
        }
        return true;
    }

    //DEF
    /**********************************
    * Numbers
    ***********************************/
    function checkBothNum(f, arg1, arg2, fname) {
        return typeCheck(f, arg1, isNumber, arg2, isNumber, fname);
    }
    var numberDict = {
      _add: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'plus')){
            applyFunction(k,[ makeNumber(left.n + right.n)]);
        }
      }),
      _plus: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'plus')){
            applyFunction(k,[ makeNumber(left.n + right.n)]);
        }
      }),
      _minus: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'minus')){
        applyFunction(k,[ makeNumber(left.n - right.n)]);
        }
      }),
      _divide: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'divide')){
        if(right.n === 0) {throwPyretMessage('Division by zero');}
        applyFunction(k,[ makeNumber(left.n / right.n)]);
        }
      }),
      _times: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'times')){
        applyFunction(k,[ makeNumber(left.n * right.n)]);
        }
      }),
      _lessthan: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'lessthan')){
        applyFunction(k,[makeBoolean(left.n < right.n)]);
        }
      }),
      _greaterthan: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'greaterthan')){
        applyFunction(k,[ makeBoolean(left.n > right.n)]);
        }
      }),
      _lessequal: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'lessequal')){
        applyFunction(k,[ makeBoolean(left.n <= right.n)]);
        }
      }),
      _greaterequal: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'greaterequal')){
        applyFunction(k,[ makeBoolean(left.n >= right.n)]);
        }
      }),
      max: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'max')){
        applyFunction(k,[ makeBoolean(Math.max(left.n, right.n))]);
        }
      }),
      min: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'min')){
        applyFunction(k,[ makeBoolean(Math.min(left.n, right.n))]);
        }
      }),
      abs: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'abs')){
        applyFunction(k,[ makeBoolean(Math.abs(left.n, right.n))]);
        }
      }),
      modulo: makeMethod(function(k, f, left, right) {
        if(checkBothNum(f, left, right, 'modulo')){
        applyFunction(k,[ makeBoolean(left.n % right.n)]);
        }
      }),
      tostring : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'tostring')){
        applyFunction(k,[ makeString(String(me.n))]);
        }
      }),
      _torepr : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'torepr')){
        applyFunction(k,[ makeString(String(me.n))]);
        }
      }),
      floor : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'floor')){
        applyFunction(k,[ makeNumber(Math.floor(me.n).toFixed(1))]);
        }
      }),
      ceiling : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'ceiling')){
        applyFunction(k,[ makeNumber(Math.ceil(me.n).toFixed(1))]);
        }
      }),
      exp: makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'exp')){
        applyFunction(k,[ makeNumber(Math.exp(me.n))]);
        }
      }),
      expt: makeMethod(function(k, f, me, pow) {
        if(checkBothNum(f, me, pow, 'expt')){
        applyFunction(k,[ makeNumber(Math.pow(me.n, pow.n))]);
        }
      }),
      _equals: makeMethod(function(k, f, me, other) {
        if(checkBothNum(f, me, other, 'equals')){
        applyFunction(k,[ makeBoolean(me.n === other.n)]);
        }
      }),
      sin : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'sin')){
        applyFunction(k,[ makeNumber(Math.sin(me.n))]);
        }
      }),
      cos : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'cos')){
        applyFunction(k,[ makeNumber(Math.cos(me.n))]);
        }
      }),
      tan : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'tan')){
        applyFunction(k,[ makeNumber(Math.tan(me.n))]);
        }
      }),
      asin : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'asin')){
        applyFunction(k,[ makeNumber(Math.asin(me.n))]);
        }
      }),
      acos : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'acos')){
        applyFunction(k,[ makeNumber(Math.acos(me.n))]);
        }
      }),
      atan : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'atan')){
        applyFunction(k,[ makeNumber(Math.atan(me.n))]);
        }
      }),
      sqr : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'sqr')){
        applyFunction(k,[ makeNumber(Math.pow(me.n,2))]);
        }
      }),
      sqrt : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'sqrt')){
        applyFunction(k,[ makeNumber(Math.sqrt(me.n))]);
        }
      }),
      truncate : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'truncate')){
        applyFunction(k,[ makeNumber(Math.round(me.n))]);
        }
      }),
      exact : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'exact')){
        applyFunction(k,[ makeNumber(me.n)]);
        }
      }),
      log : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isNumber, 'log')){
        applyFunction(k,[ makeNumber(Math.log(me.n))]);
        }
      }),
    };

    function PNumber(n) {
      this.n = n;
    }
    function makeNumber(n) { return new PNumber(n); }
    function isNumber(v) { return v instanceof PNumber; }
    PNumber.prototype = {
      dict : numberDict
    };

    //DEF
    /**********************************
    * Strings
    ***********************************/

    function checkBothStr(f, arg1, arg2, fname) {
        return typeCheck(f, arg1, isString, arg2, isString, fname);
    }

    var stringDict = {
      _plus: makeMethod(function(k, f,left, right) {
        if(checkBothStr(f, left, right, 'string-plus')){
        applyFunction(k, [ makeString(left.s + right.s)]);
        }
      }),
      tostring : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isString, 'tostring')){
        applyFunction(k, [ makeString(me.s)]);
      }
      }),
      contains: makeMethod(function(k, f, me, sub){
        if(checkBothStr(f, me, sub, 'contains')){
        applyFunction(k, [ makeBoolean(me.s.indexOf(sub.s) != -1)]);
        }
      }),
      'char-at': makeMethod(function(k, f, me, n) {
        if(typeCheck(f, me, isString, n, isNumber, 'char-at')){
        applyFunction(k, [ makeString(String(me.s.charAt(n.n)))]);
        }
      }),
      replace: makeMethod(function(k, f, me, forStr, withStr) {
          if(checkIf(f, me, isString, 'tostring') &&
          checkIf(f, forStr, isString, 'tostring') && 
          checkIf(f, withStr, isString, 'tostring')) { 
          applyFunction(k, [ makeString(me.s.replace(new RegExp(forStr.s ,"g"), withStr.s))]);
          }
      }),
      tonumber: makeMethod(function(k, f, me) {
          checkIf(f, me, isString, 'tonumber')
          toNum = Number(me.s);
          if(!isNaN(toNum)) {
            applyFunction(k, [ makeNumber(Number(me.s))]);
          }
          else {
              applyFunction(k, [ makeNothing()]);
          }
      }),
      substring: makeMethod(function(k, f, me, start, stop) {
          if(checkIf(f, me, isString, 'substring') && 
          checkIf(f, start, isNumber, 'substring') &&
          checkIf(f, stop, isNumber, 'substring')) {
        applyFunction(k, [ makeString(me.s.substring(start.n,stop.n))]);
          }
      }),
      append : makeMethod(function(k, f, me, o) {
        if(checkBothStr(f, me,o, 'append')){
        applyFunction(k, [ makeString(me.s + o.s)]);
        }
      }),
      length : makeMethod(function(k, f, me) {
        if(checkIf(f, me, isString, 'length')){
        applyFunction(k, [ makeNumber(me.s.length)]);
        }
      }),
      repeat : makeMethod(function(k, f, me, n) {
        if(typeCheck(f, me, isString, n, isNumber, 'repeat')){
        var result = "";i
        for(var x = n.n; x>0; n.n--){
           result = result + me.s;
        }
        applyFunction(k, [ makeString(result)]);
        }
      }),

      _greatereqaul : makeMethod(function(k, f, me, n) {
        if(typeCheck(f, me, isString, n, isString, 'greaterequal')){
        applyFunction(k, [ makeBoolean(me.s >= n.s)]);
        }
      }),
      _greaterthan : makeMethod(function(k, f, me, n) {
        if(typeCheck(f, me, isString, n, isString, 'greaterthan')){
        applyFunction(k, [ makeBoolean(me.s > n.s)]);
        }
      }),
      _lessequal : makeMethod(function(k, f, me, n) {
        if(typeCheck(f, me, isString, n, isString, 'lessequal')){
        applyFunction(k, [ makeBoolean(me.s <= n.s)]);
        }
      }),
      _lessthan : makeMethod(function(k, f, me, n) {
        if(typeCheck(f, me, isString, n, isString, 'lessthan')){
        applyFunction(k, [ makeBoolean(me.s < n.s)]);
        }
      }),
      _equals : makeMethod(function(k, f, me, x) {
        if(typeCheck(f, me, isString, x, isString, 'equals')){
        applyFunction(k, [ makeBoolean(me.s === x.s)]);
        }
      }),
      _torepr : makeMethod(function(k, f, me) {
          if(checkIf(f, me, isString, 'torepr')){
          applyFunction(k, [ '"'+me.s+'"']);
      }
      }),
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
      else if (isBoolean(val)) {
        return makeString(String(val.b));
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
      else if (isMutable(val)) {
        return makeString("mutable field");
      }
      else if (isPlaceholder(val)) {
        return makeString("mutable field");
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

    /**
        getColonField(val, str)

        Gets the field of the given name from the pyret value
        Does not retrieve the value from placeholders, mutables or methods
        Instead it returns them "raw"

        Val: A pyret value
        Str: The name of the field to retrieve
    **/
    function getColonField(val, str, conts) {
      var fieldVal = val.dict[str];
        if(fieldVal === undefined) {
            raisePyretMessage(conts.dict['$f'], str + " was not found on " + toRepr(val).s);
        }
        else{
        return fieldVal;
        }
      }


    /**
        getMutField(val, str)

        Gets the field of the given name from the pyret value
        The field must be a mutable field, else error

        Val: A pyret value
        Str: The name of the field to retrieve
    **/
    function getMutField(val, str) {
      var fieldVal = val.dict[str];
      if(fieldVal === undefined) {
            throwPyretMessage(str + " was not found on " + toRepr(val).s);
      }
      else if(isMutable(fieldVal)) {
        return fieldVal;
      }
      else {
            throwPyretMessage(str + " is not a mutable field.");
      }
    }


    var testPrintOutput = "";
    function testPrint(k, f, val) {
      var str = toRepr(val).s;
      console.log("testPrint: ", val, str);
      testPrintOutput += str + "\n";
      // Should be applyFunc, or whatever your implementation calls it
      applyFunction(k, [val]);
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

    //DEF
    /**********************************
    * Placeholder
    ***********************************/
    function PPlaceholder(d) {
      this.dict = d;
      this.brands = [];
      //this.dict['_torepr'] = makeMethod(function(me) {
        //return toRepr(me);
      //});
    }

    
    function getPlaceholderDict(){  
        var isSet = false;
        var value = undefined;
        var guards = [];
        
        return {
        get : makeMethod(function(k, f, me) { 
           if(isSet){
               applyFunction(k, [value])
           }
           else {
              raisePyretMessage(f, "Tried to get value from uninitialized placeholder");
              return;
           }
        }),

        guard : makeMethod(function(k, f, me, guard) {
           if(isSet) {
               raisePyretMessage(f, "Tried to add guard on an already-initialized placeholder");
               return;
            }
           else {
                guards.push(guard);
           }
           applyFunction(k, [makeNothing()]);
        }),

        set : makeMethod(function(k, f, me, val) {
            if(isSet) {
                raisePyretMessage(f, "Tried to set value in already-initialized placeholder");
                return;
            }
            for(var g in guards) {
                var newK = makeFunction(function(newVal) {
                    val = newVal;
                });
                applyFunction(guards[g],[newK, f, val]);
            }
            value = val;
            isSet = true;
            applyFunction(k, [value]);
        }),

       tostring : makeMethod(function(k, f, me) {
        applyFunction(k, [ makeString("cyclic-field")]);
       }),

       _torepr : makeMethod(function(k, f, me) {
            applyFunction(k, [ makeString("cyclic-field")]);
        }),
        
       _equals : makeMethod(function(k, f, me,other) {
            applyFunction(k, [ makeBoolean(me === other)]);
       }),
     };
    }

    function isPlaceholder(v) { return v instanceof PPlaceholder; }

    PPlaceholder.prototype = Object.create(PBase.prototype);
    PPlaceholder.prototype.clone = (function() {
        //We don't need to do deep cloning, but we *do* need to clone the dict
        var newDict = {};
        for(var key in this.dict) {
            newDict[key] = this.dict[key];
        }
        var newPlac = makePlaceholder(newDict);
        return newPlac;
    });
    PObj.prototype.getType = function() {return 'placeholder';};
    //Placeholder

    function makePlaceholder(k, f) {
        var plac = new PPlaceholder(getPlaceholderDict());
        applyFunction(k,  [plac]);
    }

    


    //DEF
    /************************
            Mutables
      **********************/
    function PMutable(d) {
      this.dict = d;
      this.brands = [];
      //this.dict['_torepr'] = makeMethod(function(me) {
        //return toRepr(me);
      //});
    }
    PMutable.prototype = Object.create(PBase.prototype);

    function mutClone(val, r, w) {
        return function clone() {
            //We don't need to do deep cloning, but we *do* need to clone the dict
            var newDict = {};
            for(var key in this.dict) {
                newDict[key] = this.dict[key];
            }
            var newObj = makeObj(newDict);
            
           newObj.set = (function(newVal) { 
                newVal = applyFunction(w,[newVal]);
                return (a = newVal);
            });
            
           newObj.r = r;
           newObj.w = w;
           newObj.a = val;

           newObj.clone = clone;
           return newObj;
          }
    }

    //Mutable
    function makeMutable(k, f, val, r, w) {
        var a = val;

        if(!isFunction(r)) {
            raisePyretMessage(f,'typecheck failed; expected Function and got\n' + toRepr(r).s);
            return;
        }
        if(!isFunction(w)) {
            raisePyretMessage(f,'typecheck failed; expected Function and got\n' + toRepr(w).s);
            return;
        }

        var mut = new PMutable({
            tostring: makeMethod(function(k, f, me) {
                applyFunction(k, [makeString("mutable-field")]);
            }),

            'get': makeMethod(function(k, f, me) {
                var newk = makeFunction(function(a_val) {
                    a = a_val;
                    applyFunction(k, [a])
                });
                applyFunction(r,[newk,f, a]);
            }),

            '_equals' : makeMethod(function(k, f, me, other) {
                applyFunction(k, [makeBoolean(me === other)]);
            }),
        });

        mut.set = (function(newVal) { 
                var newk = makeFunction(function(a_val) {
                    a = a_val;
                    applyFunction(k, [a])
                });
                applyFunction(w ,[newk,f, newVal]);
            });

        
        mut.clone = mutClone(val, r,w);
        applyFunction(k, [mut]);
    };


    function isMutable(val) {
        return val instanceof PMutable;
    }

    var identity = makeFunction(function(k, f, x) {applyFunction(k, [x]);});
    function makeSimpleMutable(k, f, val) { makeMutable(k, f, val,identity,identity );}


    /**********************************
    * Builtins
    ***********************************/
    //Brander
    var brandCount = 0;
    brander = makeFunction(function(k,f) {
    var myBrand = brandCount++; 
    var branderDict = {
        brand: makeFunction(function(k, f, toBrand) {
            var newO = toBrand.clone();
            newO.brands = toBrand.brands.slice(0);
            newO.brands.push(myBrand);
            applyFunction(k, [newO]);
        }),
        test: makeFunction(function(k,f ,o) {
            applyFunction(k, [makeBoolean(o.brands.indexOf(myBrand) != -1)])
        }),
    };
    applyFunction(k, [makeObj(branderDict)]);
    });

    //check-brand
    checkBrand = makeFunction(function(k, f, test, obj, msg){
        if(isFunction(test)){
            applyFunction(test,[k, f, obj, msg]);
        }
        else {
            raisePyretMessage(f, "Check brand with non-function");
        }
    });

    /**
      dataToRepr(val, name, fields)

      Creates a string representation of the data
      using the value, name of object type and field names
    **/
     function dataToRepr(k, f, val, name, fields){
            var fieldsEmpty = true;
            var repr = name.s + "(";
            while(true){
              try{
                  var first = getField(fields, "first");
                  var rest = getField(fields, "rest");
                  fields = rest;

                  if(! isString(first)) {throwPyretMessage("Key was not a string: " + toRepr(first));}

                  repr = repr + toRepr(getField(val, first.s)).s + ", ";
                  fieldsEmpty = false;
              }
              catch(e){
                break;
              }
            }
            if(fieldsEmpty) {
                applyFunction(k,[ makeString(name.s + "()")]);
            }
            else {
              applyFunction(k,[ makeString(repr.substring(0, repr.length-2) + ")")]);
            }

            }   

    /**
      dataEquals(me, other, brand, fields)

      Checks if the data objects are equal
      Uses the list of fields to check for equality
    **/
    function dataEquals(k, f, me, other, brand, fields) {
        var b = applyFunction(brand, [other]).b;
        
        var acc = true;
        while(true){
          try{
              var first = getField(fields, "first");
              var rest = getField(fields, "rest");
              fields = rest;

              myVal = getField(me, first.s);
              otherVal = getField(other, first.s);
            
              acc = acc && (equiv(myVal, otherVal).b)
          }
          catch(e) {
            break;
          }
        }
        //var sameBrands = checkSameBrands(me.brands, other.brands);
        applyFunction(k, [makeBoolean(b && acc)]);
    }
 //-------------------

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
        Function: makeFunction(function() {
          throw "function NYI";
        }),
        builtins: makeObj({
            'data-to-repr': makeFunction(dataToRepr),

            'data-equals': makeFunction(dataEquals),

            "has-field" : makeFunction(function(prim, field) {
              applyFunction(k,[makeBoolean(prim.dict.hasOwnProperty(field))]);
              }),
            equiv : makeFunction(equiv)
        }),

        "mk-placeholder": makeFunction(makePlaceholder),
        "mk-simple-mutable" : makeFunction(makeSimpleMutable),
        "mk-mutable" : makeFunction(makeMutable),
        brander : brander,
        "check-brand": checkBrand,

        //TODO: These aren't neccessarily right, they should probably raise errors
        'Function': makeFunction(function(obj) {applyFunction(k, [ makeBoolean(isFunction(obj))]);}),
        'Number': makeFunction(function(k, f, x, m){applyFunction(k, [ makeBoolean(isNumber(x))]);}),
        'Method': makeFunction(function(k, f, x, m){applyFunction(k, [ makeBoolean(isMethod(x))]);}),
        'Placeholder': makeFunction(function(k, f, x, m){applyFunction(k, [ makeBoolean(isPlaceholder(x))]);}),
        'Mutable': makeFunction(function(k, f, x, m){applyFunction(k, [ makeBoolean(isMutable(x))]);}),
        'Nothing': makeFunction(function(k, f, x, m){applyFunction(k, [ makeBoolean(isNothing(x))]);}),
        'String': makeFunction(function(k, f, x, m) {applyFunction(k, [ makeBoolean(isString(x))]);}),
        'Any': makeFunction(function(k, f, x,m ){applyFunction(k, [ makeBoolean(isPBase(x))]);}),
        'Bool': makeFunction(function(k, f, x,m){applyFunction(k, [ makeBoolean(isBoolean(x))]);}),
        'Object': makeFunction(function(k, f, x,m){applyFunction(k, [ makeBoolean(isObj(x))]);}),

        'is-function': makeFunction(function(obj) {applyFunction(k, [ makeBoolean(isFunction(obj))]);}),
        'is-number': makeFunction(function(k, f, x){applyFunction(k, [ makeBoolean(isNumber(x))]);}),
        'is-method': makeFunction(function(k, f, x){applyFunction(k, [ makeBoolean(isMethod(x))]);}),
        'is-placeholder': makeFunction(function(k, f, x){applyFunction(k, [ makeBoolean(isPlaceholder(x))]);}),
        'is-mutable': makeFunction(function(k, f, x){applyFunction(k, [ makeBoolean(isMutable(x))]);}),
        'is-nothing': makeFunction(function(k, f, x){applyFunction(k, [ makeBoolean(isNothing(x))]);}),
        'is-string': makeFunction(function(k, f, x) {applyFunction(k, [ makeBoolean(isString(x))]);}),
        'is-any': makeFunction(function(k, f, x){applyFunction(k, [ makeBoolean(isPBase(x))]);}),
        'is-bool': makeFunction(function(k, f, x){applyFunction(k, [ makeBoolean(isBoolean(x))]);}),
        'is-object': makeFunction(function(k, f, x){applyFunction(k, [ makeBoolean(isObj(x))]);}),

          "prim-num-keys" : makeFunction(function(k,f ,prim) {
              if(isNothing(prim)) {return makeNumber(0);}
            applyFunction(k, [makeNumber(Object.keys(prim.dict).length)]);
          }),
        "prim-keys" : makeFunction(function(k, f, prim) {
            var myKeys = makeEmpty();
            for(key in prim.dict) {
                myKeys = makeLink(makeString(String(key)), myKeys);
            }
            applyFunction(k,[ myKeys]);
         }),
          "prim-has-field" : makeFunction(function(k, f, prim, field) {
            applyFunction(k, [ makeBoolean(prim.dict.hasOwnProperty(field))]);
          }),

        raise : raise,
        error : error
      }),
      runtime: {
        makeNumber: makeNumber,
        makeString: makeString,
        makeFunction: makeFunction,
        makeMethod: makeMethod,
        makeObj: makeObj,
        makeBoolean: makeBoolean,
        makeNothing: makeNothing,
        checkBool: checkBool,
        isNumber: isNumber,
        equal: equal,
        getField: getField,
        getMutField: getMutField,
        getColonField: getColonField,
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

