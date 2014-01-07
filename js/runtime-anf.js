/***
This is the runtime for the ANF'd version of pyret
*/
"use strict";
var SafeDict = (function() {
    var noProto = {};
    function SafeDict(initialBindings) {
      if (typeof initialBindings !== "object") {
            throw new Error("Non-object " + initialBindings + " given to SafeDict constructor");
      }
      this.bindings = initialBindings;
      this.proto = noProto;
    }
    SafeDict.prototype = {
      merge: function(other) {
          var combined = Object.create(this.bindings);
          for (var k in other.bindings) {
            combined[k] = other.bindings[k];
          }
          var newSafeDict = new SafeDict(combined);
          if (other.proto !== noProto) {
            newSafeDict.proto = other.proto;
          }
          else {
            newSafeDict.proto = this.proto;
          }
          return newSafeDict;
      },
      get: function(key) {
          if (key === "__proto__") {
            if (this.proto === noProto) {
                throw new Error("Looked up __proto__, not bound in safeDict");
            }
            return this.proto;
          }
          else {
            if (!(key in this.bindings)) {
                throw new Error("Looked up " + key + ", not bound in safeDict");
            }
            return this.bindings[key];
          }
      },
      set: function(key, value) {
          if (key === "__proto__") {
            var newSafeDict = new SafeDict(this.bindings);
            newSafeDict.proto = value;
            return newSafeDict;
          } 
          else {
            var o = Object.create(null);
            o[key] = value;
            return this.merge(new SafeDict(o));
          }
      },
      hasBinding: function(key) {
          if (key === "__proto__") {
            return this.proto !== noProto;
          }
          else {
            return key in this.bindings;
          }
      },
      getNames: function() {
        var keys = [];
        if (this.proto !== noProto) { keys.push("__proto__"); }
        for (var key in this.bindings) {
          keys.push(key);
        }
        return keys;
      }
    };
    var makeSafeDict = function(bindingsObj) {
      var bindings = Object.create(null);
      Object.keys(bindingsObj).forEach(function(k) {
          bindings[k] = bindingsObj[k];
      });
      return new SafeDict(bindings);
    }
    
    return makeSafeDict;
})();

var PYRET_ANF = (function() {

function makeRuntime() {
    /**
      The base of all pyret values
      @constructor
      @abstract
    */
    function PBase() {
        this.brands = [];
        this.dict   = {};
    }

    PBase.prototype = {
        dict   : {},
        brands : [],
        extendWith : extendWith,
        clone : function() {return new PBase();}
    };

    /**
        Extends an object with the new fields in fields
        If all the fields are new, the brands are kept,
        otherwise, the extended object has no brands

        The original object is not mutated, instead it is cloned and the clone is mutated

        @param {Object.<string, PBase>} fields: a PObj whose fields will be added to the Pyret base
        If any of the fields exist, they will be overwritten with the new value

        @return {PBase} the extended object 
    */
    function extendWith(fields) {
        var newObj = this.clone();
        var allNewFields = true;

        for(var field in fields) {
            if(allNewFields && newObj.dict.hasOwnProperty(field)) {
                allNewFields = false;
            }

            newObj.dict[field] = fields[field];
        } 
            
            newObj.brands = (allNewFields ? this.brands.slice(0) : []);

            return newObj;
    }


    /**
      Makes a copy of a dictionary
      Use this when cloning an object
      Not a deep copy, field values are merely references and are shared between the copies

      @param {Object.<string, PBase>} dict the dictionary to clone
      @return {Object.<string, PBase>} a copy of the dict such that changes to the copy are *not* reflected in the original
    */
    function copyDict(dict) {
        var newDict = {};
        for(var field in dict) {
            newDict[field] = dict[field];
        }

        return newDict;
    }

    /**Tests whether an object is a PBase
        @param {Object} obj the item to test
        @return {boolean} true if object is a PBase
    */
    function isBase(obj) { return obj instanceof PBase; }

    /*********************
            Nothing
    **********************/
    /**
      Pyret Nothing
      Represents the 'nothing' value in pyret
      @constructor
      @extends {PBase}
    **/
    function PNothing() {
        /**@type {Object.<string, PBase>}*/
        this.dict   = {};
        /**@type {Array.<number>}*/
        this.brands = [];
    }
    PNothing.prototype = Object.create(PBase.prototype);
    
    /**Clones the number
      @return {PNothing} With same dict
    */
    PNothing.prototype.clone = function() { 
        var newNoth = makeNothing(); 
        newNoth.dict = copyDict(this.dict);
        return newNoth;
    };
    /**Tests whether an object is a PNothing
        @param {Object} obj the item to test
        @return {boolean} true if object is a PNothing
    */
    function isNothing(obj) { return obj instanceof PNothing; }

    /**Makes a nothing
       @return PNothing
    */
    function makeNothing() {return new PNothing();}

    /*********************
            Number
    **********************/
    /**The representation of all numerical values
        @constructor
        @param {number} n the number to store in this pyret number
        @extends {PBase}
    */
    function PNumber(n) { 
        /**@type {number}*/
        this.n    = n;

        /**@type {Object.<string, PBase>}*/
        this.dict = createNumberDict(); 
    }
    PNumber.prototype = Object.create(PBase.prototype); 

    /**Clones the number
      @return {PNumber} With same n and dict
    */
    PNumber.prototype.clone = function() { 
        var newNum = makeNumber(this.n); 
        newNum.dict = copyDict(this.dict);
        return newNum;
    };


    /**Tests whether an object is a PNumber
        @param {Object} obj the item to test
        @return {boolean} true if object is a PNumber
    */
    function isNumber(obj) { return obj instanceof PNumber; }

    /**Creates a copy of the common dictionary all objects have
      @return {Object} the dictionary for a number
    */
    function createNumberDict() {
        return { };
    }

    /**Makes a PNumber using the given n

      @param {number} n the number the PNumber will contain
      @return {PNumber} with value n
    */
    function makeNumber(n) {
       return new PNumber(n); 
    }

    /*********************
            String
    **********************/
    /**The representation of all string pyret values
        @constructor
        @param {string} s the string to store in this pyret number
        @extends {PBase}
    */
    function PString(s) { 
        /**@type {string}*/
        this.s    = s;

        /**@type {Object.<string, PBase>}*/
        this.dict = createStringDict(); 
    }
    PString.prototype = Object.create(PBase.prototype); 

    /**Clones the number
      @return {PNumber} With same n and dict
    */
    PString.prototype.clone = function() { 
        var newStr = makeString(this.s); 
        newStr.dict = copyDict(this.dict);
        return newStr;
    };


    /**Tests whether an object is a PString
        @param {Object} obj the item to test
        @return {boolean} true if object is a PString
    */
    function isString(obj) { return obj instanceof PString; }

    /**Creates a copy of the common dictionary all objects have
      @return {Object} the dictionary for a number
    */
    function createStringDict() {
        return { };
    }

    /**Makes a PString using the given s

      @param {string} s the string the PString will contain
      @return  PString} with value s
    */
    function makeString(s) {
       return new PString(s); 
    }

    /*********************
           Boolean 
    **********************/
    /**The representation of all boolean pyret values
        @constructor
        @param {boolean} b the boolean to store in this pyret number
        @extends {PBase}
    */
    function PBoolean(b) { 
        /**@type {boolean}*/
        this.b    = b;

        /**@type {Object.<string, PBase>}*/
        this.dict = createBooleanDict(); 
    }
    PBoolean.prototype = Object.create(PBase.prototype); 

    /**Clones the Boolean
      @return {PBoolean} With same b and dict
    */
    PBoolean.prototype.clone = function() { 
        var newBool = makeBoolean(this.b); 
        newBool.dict = copyDict(this.dict);
        return newBool;
    };


    /**Creates a copy of the common dictionary all boolean have
      @return {Object} the dictionary for a boolean
    */
    function createBooleanDict() {
        return { };
    }

    /**Tests whether an object is a PBoolean
        @param {Object} obj the item to test
        @return {boolean} true if object is a PBoolean
    */
    function isBoolean(obj) { return obj instanceof PBoolean; }

    /**Creates a copy of the common dictionary all objects have
      @return {Object} the dictionary for a number
    */
    function createBoolean() {
        return { };
    }

    /**Makes a PBoolean using the given s

      @param {boolean} b the Boolean the PBoolean will contain
      @return  PBoolean} with value b
    */
    function makeBoolean(b) {
       return new PBoolean(b); 
    }

    /*********************
            Function
    **********************/

    /**The representation of a function
        @constructor
        @param {function(...[PBase]) :  PBase} fun 
        @extends {PBase}
    */
    function PFunction(fun) { 
        /**@type {function(...[PBase]) : PBase}*/
        this.app   = fun;
        /**@type {number}*/
        this.arity = fun.length;

        /**@type {Object.<string, PBase>}*/
        this.dict = createFunctionDict(); 
    }
    PFunction.prototype = Object.create(PBase.prototype); 

    /**Clones the function
      @return {PFunction} With same app and dict
    */
    PFunction.prototype.clone = function() { 
        var newFun = makeFunction(this.app); 
        newFun.dict = copyDict(this.dict);
        return newFun;
    };

    /**Tests whether an object is a PFunction
        @param {Object} obj the item to test
        @return {boolean} true if object is a PFunction
    */
    function isFunction(obj) { return obj instanceof PFunction; }

    /**Creates a copy of the common dictionary all function have
      @return {Object} the dictionary for a function
    */
    function createFunctionDict() {
        return { };
    }

    /**Makes a PFunction using the given n

      @param {function(...[PBase]) : PBase} fun The JS function that represents the body of the function, must contain at least one arg, which represents self
      @return {PFunction} with app of fun
    */
    function makeFunction(fun) {
       return new PFunction(fun); 
    }


    /*********************
            Method
    **********************/

    /**The representation of a method
        @constructor
        @param {function(PBase, ...[PBase]) :  PBase} meth 
        @extends {PBase}
    */
    function PMethod(meth) { 
        /**@type {function(PBase, ...[PBase]) : PBase}*/
        this.meth   = meth;
        /**@type {number}*/
        this.arity = meth.length;

        /**@type {Object.<string, PBase>}*/
        this.dict = createMethodDict(); 
    }
    PMethod.prototype = Object.create(PBase.prototype); 

    /**Clones the method
      @return {PMethod} With same meth and dict
    */
    PMethod.prototype.clone = function() { 
        var newMeth = makeMethod(this.meth); 
        newMeth.dict = copyDict(this.dict);
        return newMeth;
    };

    /**Tests whether an object is a PMethod
        @param {Object} obj the item to test
        @return {boolean} true if object is a PMethod
    */
    function isMethod(obj) { return obj instanceof PMethod; }

    /**Creates a copy of the common dictionary all function have
      @return {Object} the dictionary for a method
    */
    function createMethodDict() {
        return { };
    }

    /**Makes a PMethod using the given function
      The function first argument should be self

      @param {function(PBase, ...[PBase]) : PBase} meth The JS function that represents the body of the method
      @return {PMethod} with app of fun
    */
    function makeMethod(meth) {
       return new PMethod(meth); 
    }
    //Export the runtime
    //String keys should be used to prevent renaming
    return {
        'isBase'      : isBase,
        'isNothing'   : isNothing,
        'isNumber'    : isNumber,
        'isString'    : isString,
        'isBoolean'   : isBoolean,
        'isFunction'  : isFunction,
        'isMethod'    : isMethod,

        'makeNothing'  : makeNothing,
        'makeNumber'   : makeNumber,
        'makeBoolean'  : makeBoolean,
        'makeString'   : makeString,
        'makeFunction' : makeFunction,
        'makeMethod'   : makeMethod
    };
}

return  {'makeRuntime' : makeRuntime};
})();

if (typeof exports !== 'undefined') {
  exports['PYRET_ANF'] = PYRET_ANF;
}

