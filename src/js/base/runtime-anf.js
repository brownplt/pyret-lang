/***
This is the runtime for the ANF'd version of pyret
*/
"use strict";
/** @typedef {!Object} */
var Bignum;


define(["js/namespace", "js/js-numbers"],
       function (Namespace, jsnumsIn) {
  if(requirejs.isBrowser) {
    var require = requirejs;
  }
  else {
    var require = requirejs.nodeRequire("requirejs");
  }



  //var Namespace = require('./namespace.js').Namespace;

  /**
    @type {{
        fromFixnum : function(number) : Bignum,
        fromString : function(string) : (Bignum|boolean),
        toFixnum : function() : number,

        isSchemeNumber : function(Object) : boolean,

        equals : function(Bignum, Bignum) : boolean,
        lessThan : function(Bignum, Bignum) : boolean,
        greaterThan : function(Bignum, Bignum) : boolean,
        lessThanOrEqual : function(Bignum, Bignum) : boolean,
        greaterThanOrEqual : function(Bignum, Bignum) : boolean,

        add : function(Bignum, Bignum) : Bignum,
        subtract : function(Bignum, Bignum) : Bignum,
        multiply : function(Bignum, Bignum) : Bignum,
        divide : function(Bignum, Bignum) : Bignum,

        sin : function(Bignum) : Bignum,
        cos : function(Bignum) : Bignum,
        tan : function(Bignum) : Bignum,
        asin : function(Bignum) : Bignum,
        acos : function(Bignum) : Bignum,
        atan : function(Bignum) : Bignum
          }}
   */
  var jsnums = jsnumsIn;



/**
Creates a Pyret runtime
@param {{stdout : function(string), initialGas : number}} theOutsideWorld contains the hooks
into the environment

@return {Object} that contains all the necessary components of a runtime
*/
function makeRuntime(theOutsideWorld) {
/**
    Extends an object with the new fields in fields
    If all the fields are new, the brands are kept,
    otherwise, the extended object has no brands

    The original object is not mutated, instead it is cloned and the clone
    is mutated

    @param {!Object.<string, !PBase>} fields: a PObj whose fields will be added to the Pyret base
    If any of the fields exist, they will be overwritten with the new value

    @return {!PBase} the extended object 
*/
function extendWith(fields) {
    /**@type {!Object}*/
    var newDict = Object.create(this.dict);
    /**@type {!boolean}*/
    var allNewFields = true;

    for(var field in fields) {
        if(allNewFields && hasProperty(this.dict, field)) {
            allNewFields = false;
        }

        newDict[field] = fields[field];
    } 

    var newObj = this.updateDict(newDict, allNewFields);

    return newObj;
}

    function brandClone(newObj, obj, b) {
      newObj.dict = obj.dict;
      if (b in obj.brands) {
        newObj.brands = obj.brands;
      }
      else {
        newObj.brands = Object.create(obj.brands);
        newObj.brands[b] = true;
        newObj.brands.brandCount++;
      }
      return newObj;
    }

    var noBrands = { brandCount: 0 };

    /**
      The base of all pyret values
      @constructor
    */
    function PBase() {
        /**@type {!Object.<string, Boolean>}*/
        this.brands = noBrands;
        /**@type {!Object.<string, !PBase>}*/
        this.dict   = emptyDict;
    }

    /**@type {!Object.<string, !PBase>}*/
    PBase.prototype.dict = emptyDict;
    /**@type {!function(!Object.<string, !PBase>) : !PBase}*/
    PBase.prototype.extendWith = extendWith;

/**
  Sets up Inheritance with a function call
  This needs to be done socompiler recognizes it

  @param {Function} sub the class that will become a subclass
  @param {Function} from the class that sub will subclass
  */
function inherits(sub, from) {
    sub.prototype = new from();
    //sub.prototype = Object.create(from.prototype);
}

//Set up heirarchy
//We need to set it up before all the other classes
inherits(PNothing, PBase);
inherits(PObject, PBase);
inherits(PFunction, PBase);
inherits(PMethod, PBase);
inherits(POpaque, PBase);

/**
    Tests whether a JS Object has a property

    @param {!Object} obj the object to test
    @param {!string} p the property to look for
    @return {boolean} true if obj has property p, false otherwise
*/
function hasProperty(obj, p) {
    return p in obj;
}

/**
    Tests whether a JS Object has a property, but not on 
    any of its prototypes.
    Useful for objects that lack the .hasOwnProperty method

    @param {!Object} obj the object to test
    @param {!string} p the property to look for
    @return {boolean} true if obj has property p, false otherwise
*/
function hasOwnProperty(obj, p) {
    return Object.prototype.hasOwnProperty.call(obj, p);
}

var parameters = Object.create(null);

function getParam(param) {
  if(hasOwnProperty(parameters, param)) {
    return parameters[param];
  }
  else {
    throw new Error("Parameter " + param + " not defined");
  }
}
function setParam(param, val) {
  parameters[param] = val;
}
function hasParam(param) {
  return param in parameters;
}

/**
    Get the brands on an object

    @param {!PBase} obj the object to get the brands of
    @return {Object.<string,!Boolean>}
*/
function getBrands(obj) {
  return obj.brands;
}

var getProto = Object.getPrototypeOf;

/**
    Get the fields in an object.

    @param {!PBase} obj the object to get the fields of
    @return {Array.<string>}

*/
function getFields(obj) {
  var fields = [];
  var currentProto = obj.dict;
  while(currentProto !== null) {
    fields = fields.concat(Object.keys(currentProto));
    currentProto = getProto(currentProto);
  }
  return fields;
}

/**
    Fold a function over the fields of an object (key and value)

    @param {!PBase} obj the object to fold over
    @param {Function} f the folding function takes the accumulator, then the
                        field name, then the value
    @param {!Object} init the initial value to fold over
*/
function foldFields(obj, f, init) {
  var fields = getFields(obj);
  var acc = init;
  fields.forEach(function(fld) {
      acc = f(acc, fld, obj.dict[fld]);
    });
  return acc;
}


/**
  Makes a copy of a dictionary
  Use this when cloning an object
  Not a deep copy, field values are merely references and are shared between the copies

  @param {!Object.<string, !PBase>} dict the dictionary to clone
  @return {!Object.<string, !PBase>} a copy of the dict such that changes to the copy are *not* reflected in the original
*/
function copyDict(dict) {
    return Object.create(dict);
}

/**
  @param {Array.<number>} brands
  @return Array.<number>
*/
function copyBrands(brands) {
  return brands;
}

var emptyDict = Object.create(null);

/** Creates a truly empty dictonary, with no inherit fields 
    @return {!Object} an empty object
 **/
function makeEmptyDict() {
    return Object.create(null);
}

/**Tests whether an object is a PBase
    @param {Object} obj the item to test
    @return {boolean} true if object is a PBase
*/
function isBase(obj) { return obj instanceof PBase; }

/********************   
    Getting Fields
********************/
/**
  Gets the field from an object of the given name
    -If field is a method, it binds self correctly and returns a function
    -If field is a placeholder, it calls get on the placeholder
    -If field is a mutable -> error
    -If field undefined -> error
    -Otherwise, returns field value

  @param {PBase} val
  @param {string} field

  @return {!PBase}
**/
function getFieldLoc(val, field, loc) {
    if(val === undefined) { ffi.throwInternalError("Field lookup on undefined ", ffi.makeList([field])); }
    if(!isObject(val)) { ffi.throwLookupNonObject(makeSrcloc(loc), val, field); }
    var fieldVal = val.dict[field];
    if(fieldVal === undefined) {
        //TODO: Throw field not found error
        //NOTE: When we change JSON.stringify to toReprJS, we'll need to support
        //reentrant errors (see commit 24ff13d9e9)
        throw ffi.throwFieldNotFound(makeSrcloc(loc), val, field);
    }
    /*else if(isMutable(fieldVal)){
        //TODO: Implement mutables then throw an error here
    }*/
    /*else if(isPlaceholder(fieldVal)){
        //TODO: Implement placeholders then call get here
        //Be wary of guards blowing up stack
    }*/
    else if(isMethod(fieldVal)){
        var curried = fieldVal['meth'](val);
        return makeFunctionArity(curried, fieldVal.arity - 1);
    }
    else {
        return fieldVal;
    }
}

function getField(obj, field) {
  return thisRuntime.getFieldLoc(obj, field, ["runtime"]);
}


/**
  Gets the field from an object of the given name
  -Returns the raw field value

  @param {!PBase} val
  @param {string} field

  @return {!PBase}
**/
function getColonField(val, field) {
  if(val === undefined) { ffi.throwInternalError("Field lookup on undefined ", [field]); }
  if(!isObject(val)) { ffi.throwLookupNonObject(makeSrcloc(["runtime"]), val, field); }
  var fieldVal = val.dict[field];
  if(fieldVal === undefined) {
    ffi.throwFieldNotFound(makeSrcloc(["runtime"]), val, field);
  }
  else {
    return fieldVal;
  }
}

/**

  @constructor
*/
function POpaque(val, equals) {
  this.val = val;
  this.equals = equals;
  /**@type {!Object.<string, Boolean>}*/
  this.brands = noBrands;
}
POpaque.prototype = Object.create(PBase.prototype);

POpaque.prototype.extendWith = function() {
  ffi.throwInternalError("Cannot extend opaque values", ffi.makeList([this]));
};
POpaque.prototype.updateDict = function(dict, keepBrands) {
  ffi.throwInternalError("Cannot clone opaque values", ffi.makeList([this]));
};

function makeOpaque(val, equals) { return new POpaque(val, equals); }
function isOpaque(val) { return val instanceof POpaque; }

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
    /**@type {!Object.<string, !PBase>}*/
    this.dict   = emptyDict;
    /**@type {!Object.<string, Boolean>}*/
    this.brands = noBrands;
}
PNothing.prototype = Object.create(PBase.prototype);

/**Clones the nothing
  @return {!PNothing} With same dict
*/
PNothing.prototype.updateDict = function(dict, keepBrands) { 
    var newNoth = makeNothing(); 
    newNoth.dict = dict;
    newNoth.brands = keepBrands ? this.brands : noBrands;
    return newNoth;
};

/**Clones the nothing
  @param {!String} b The brand
  @return {!PNothing} With same dict
*/
PNothing.prototype.brand = function(b) { 
    var newNoth = makeNothing(); 
    return brandClone(newNoth, this, b);
};
/**Tests whether an object is a PNothing
    @param {Object} obj the item to test
    @return {boolean} true if object is a PNothing
*/
function isNothing(obj) { return obj instanceof PNothing; }

/**Makes a nothing
   @return {!PNothing}
*/
function makeNothing() {return new PNothing();}
var nothing = makeNothing();

/*********************
        Number
**********************/
/**Tests whether an object is a PNumber
    @param {Object} obj the item to test
    @return {boolean} true if object is a PNumber
*/
function isNumber(obj) { 
  return jsnums.isSchemeNumber(obj);
}

/**Makes a PNumber using the given bignum

  @param {Bignum} n the number the PNumber will contain
  @return {!PNumber} with value n
*/
function makeNumberBig(n) {
  return n;
}

/**Makes a PNumber using the given JSNum

  @param {number} n the number the PNumber will contain
  @return {!PNumber} with value n
*/
function makeNumber(n) {
  return jsnums.fromFixnum(n);
}
//TODO: for BIG numbers, we'll need to compile them in as strings and use jsnums.fromString(_) to get the value
/**Makes a PNumber using the given string

  @param {string} s 
  @return {!PNumber} with value n
*/
function makeNumberFromString(s) {
  var result = jsnums.fromString(s);
  if(result === false) {
    ffi.throwMessageException("Could not create number from: " + s);
  }
  return result;
}

/*********************
        String
**********************/
/**Tests whether an object is a PString
    @param {Object} obj the item to test
    @return {boolean} true if object is a PString
*/
function isString(obj) { 
  return typeof obj === 'string';
}

/**Makes a PString using the given s

  @param {string} s the string the PString will contain
  @return {!PString} with value s
*/
function makeString(s) {
  if(typeof s !== "string") { throw Error("Non-string given to makeString " + JSON.stringify(s)); }
  return s;
}

/*********************
       Boolean 
**********************/
//Boolean Singletons
var pyretTrue =  true;
var pyretFalse = false;

/**Makes a PBoolean using the given s

  @param {boolean} b the Boolean the PBoolean will contain
  @return {!PBoolean} with value b
*/
function makeBoolean(b) {
  return b;
}

function isBoolean(b) {
  return b === !!b;
}

/**Tests whether the boolean is equal to the singleton true value

  @param {PBoolean} b
  @return {boolean}
*/
function isPyretTrue(b) {
    return b === pyretTrue;
}
function isPyretFalse(b) {
    return b === pyretFalse;
}

/*********************
        Function
**********************/

/**The representation of a function
    @constructor
    @extends {PBase}

    @param {Function} fun the function body
*/
function PFunction(fun, arity) { 
    /**@type {Function}*/
    this.app   = fun;

    /**@type {number}*/
    this.arity = arity || fun.length;

    /**@type {!Object.<string, !PBase>}*/
    this.dict = createFunctionDict(); 

    /**@type {!Object.<string, Boolean>}*/
    this.brands = noBrands;
}
//PFunction.prototype = Object.create(PBase.prototype); 

/**Clones the function
  @return {!PFunction} With same app and dict
*/
PFunction.prototype.updateDict = function(dict, keepBrands) { 
    var newFun = makeFunction(this.app); 
    newFun.dict = dict;
    newFun.brands = keepBrands ? this.brands : noBrands;
    return newFun;
};

/**Clones the function
  @param {!string} b The brand to add
  @return {!PFunction} With same app and dict
*/
PFunction.prototype.brand = function(b) { 
    var newFun = makeFunction(this.app); 
    return brandClone(newFun, this, b);
};

/**Tests whether an object is a PFunction
    @param {Object} obj the item to test
    @return {boolean} true if object is a PFunction
*/
function isFunction(obj) {return obj instanceof PFunction; }

/**Creates a copy of the common dictionary all function have
  @return {!Object.<string, !PBase>} the dictionary for a function
*/
function createFunctionDict() {
    return emptyDict;
}

/**Makes a PFunction using the given n

  @param {Function} fun The JS function that represents the body of the function, must contain at least one arg, which represents self
  @return {!PFunction} with app of fun
*/
function makeFunction(fun) {
   return new PFunction(fun, fun.length); 
}
function makeFunctionArity(fun, arity) {
   return new PFunction(fun, arity); 
}


/*********************
        Method
**********************/

/**The representation of a method
    @constructor
    @param {Function} meth 
    @param {Function} full_meth 
    @extends {PBase}
*/
function PMethod(meth, full_meth) { 
    /**@type {Function}*/
    this['meth']   = meth;

    /**@type {Function}*/
    this['full_meth']   = full_meth;

    /**@type {number}*/
    this.arity = full_meth.length;

    /**@type {!Object.<string, !PBase>}*/
    this.dict = createMethodDict(); 

    /**@type {!Object.<string, Boolean>}*/
    this.brands = noBrands;

}
//PMethod.prototype = Object.create(PBase.prototype); 

/**Clones the method
  @return {!PMethod} With same meth and dict
*/
PMethod.prototype.updateDict = function(dict, keepBrands) { 
    var newMeth = makeMethod(this['meth'], this['full_meth']); 
    newMeth.dict = dict;
    newMeth.brands = keepBrands ? this.brands : noBrands;
    return newMeth;
};

/**Clones the method
  @param {!string} b The brand to add
  @return {!PMethod} With same meth and dict
*/
PMethod.prototype.brand = function(b) { 
    var newMeth = makeMethod(this['meth'], this['full_meth']); 
    return brandClone(newMeth, this, b);
};

/**Tests whether an object is a PMethod
    @param {Object} obj the item to test
    @return {boolean} true if object is a PMethod
*/
function isMethod(obj) { return obj instanceof PMethod; }

/**Creates a copy of the common dictionary all function have
  @return {!Object.<string, !PBase>} the dictionary for a method
*/
function createMethodDict() {
    return emptyDict;
}

/**Makes a PMethod using the given function
  The function first argument should be self

  @param {Function} meth The Curried JS function that represents the body of the method
  @param {Function} full_meth The Full JS function that represents the body of the method
      @return {!PMethod} with app of fun
    */
    function makeMethod(meth, full_meth) {
      return new PMethod(meth, full_meth); 
    }

    function makeMethodFromFun(meth) {
      return new PMethod(function(obj) {
          return function() {
              var argList = Array.prototype.slice.call(arguments);
              return meth.apply(null, [obj].concat(argList));
            };
        }, meth);
    }

    /*********************
            Object
    **********************/
    /**The representation of an object
        @constructor
        @param {!Object.<string, !PBase>} dict
        @extends {PBase}
    */
    function PObject(dict, brands) { 
        /**@type {!Object.<string, !PBase>}*/
        this.dict = dict;

        /**@type {!Object.<string, Boolean>}*/
        this.brands = brands;
    }
    //PObject.prototype = Object.create(PBase.prototype); 

    PObject.prototype.updateDict = function(dict, keepBrands) {
      var newObj = new PObject(dict, keepBrands ? this.brands : noBrands);
      return newObj;
    }

    /**Clones the object
      @return {!PObject} With same dict
    */
    PObject.prototype.brand = function(b) { 
        var newObj = makeObject(this.dict); 
        return brandClone(newObj, this, b);
    };

    /**Tests whether an object is a PObject
        @param {Object} obj the item to test
        @return {!boolean} true if object is a PObject
    */
    function isObject(obj) { return obj instanceof PObject; }

    /**Makes a PObject using the given dict

      @param {!Object.<string, !PBase>} dict
      @return {!PObject} with given dict
    */
    function makeObject(dict) {
       return new PObject(dict, noBrands); 
    }

    function makeBrandedObject(dict, brands) {
        return new PObject(dict, brands);
    }

    /**The representation of an array
       A PArray is simply a JavaScript array
    */
    function isArray(val) {
      return val instanceof Array;
    }
    function makeArray(arr) {
      return arr;
    }

    PObject.prototype.updateDict = function(dict, keepBrands) {
      var newObj = new PObject(dict, keepBrands ? this.brands : noBrands);
      return newObj;
    }

    /**Clones the object
      @return {!PObject} With same dict
    */
    PObject.prototype.brand = function(b) { 
        var newObj = makeObject(this.dict); 
        return brandClone(newObj, this, b);
    };

    /**Tests whether an object is a PObject
        @param {Object} obj the item to test
        @return {!boolean} true if object is a PObject
    */
    function isObject(obj) { return obj instanceof PObject; }

    /**Makes a PObject using the given dict

      @param {!Object.<string, !PBase>} dict
      @return {!PObject} with given dict
    */
    function makeObject(dict) {
       return new PObject(dict, noBrands); 
    }

    function makeBrandedObject(dict, brands) {
        return new PObject(dict, brands);
    }
    
    /************************
          Type Checking
    ************************/
    /**
      Checks if value is ___
      @param {!PBase} val the value to test
      @param {!function(!PBase) : boolean} test

      @return {!boolean} true if val passes test
    */
    function checkIf(val, test) {
        if(!test(val)) {
            throw makeMessageException("Pyret Type Error: " + test + ": " + JSON.stringify(val))
        }
        return true;
    }

    function checkType(val, test, typeName) {
      if(!test(val)) { ffi.throwTypeMismatch(val, typeName) }
      return true;
    }

    function isPyretVal(val) {
      if (typeof val === "string" || typeof val === "boolean" || val instanceof Array) {
        return true;
      }
      else if (jsnums.isSchemeNumber(val)) {
        return true;
      }
      else if (isObject(val) ||
               isFunction(val) ||
               isMethod(val) ||
               isOpaque(val) ||
               isNothing(val)) {
        return true
      }
    }

    var checkArity = function(expected, args, source) {
      if (expected !== args.length) {
        throw ffi.throwArityErrorC([source], expected, args);
      }
    }


    var makeCheckType = function(test, typeName) {
      if (arguments.length !== 2) {
        // can't use checkArity yet because ffi isn't initialized
        throw("MakeCheckType was called with the wrong number of arguments: expected 2, got " + arguments.length);
      }
      return function(val) { 
        checkArity(1, arguments, "runtime");
        return checkType(val, test, typeName); 
      };
    }
    var checkString = makeCheckType(isString, "String");
    var checkNumber = makeCheckType(isNumber, "Number");
    var checkArray = makeCheckType(isArray, "Array");
    var checkBoolean = makeCheckType(isBoolean, "Boolean");
    var checkObject = makeCheckType(isObject, "Object");
    var checkFunction = makeCheckType(isFunction, "Function");
    var checkMethod = makeCheckType(isMethod, "Method");
    var checkOpaque = makeCheckType(isOpaque, "Opaque");
    var checkPyretVal = makeCheckType(isPyretVal, "Pyret Value");

    function confirm(val, test) {
      checkArity(2, arguments, "runtime");
      if(!test(val)) {
          throw makeMessageException("Pyret Type Error: " + test + ": " + JSON.stringify(val))
      }
      return thisRuntime.unwrap(val);
    }


    /************************
       Builtin Functions
    ************************/

    function hasBrand(obj, brand) {
      return Boolean(obj.brands && obj.brands[brand] === true);
    }

    var brandCounter = 0;
    function mkBrandName() {
      var thisBrandStr = "$brand" + String(++brandCounter);
      return thisBrandStr;
    }
    /**@type {PFunction} */
    var brander = makeFunction(
    /**
      @return {!PBase}
    */
    function() {
      var thisBrandStr = mkBrandName();
      return makeObject({
          'test': makeFunction(function(obj) {
              return makeBoolean(hasBrand(obj, thisBrandStr));
            }),
          'brand': makeFunction(function(obj) {
              return obj.brand(thisBrandStr);
            })
        });
    }
    );

    // Stolen from https://github.com/dyoo/whalesong/blob/master\
    // /whalesong/js-assembler/runtime-src/baselib-strings.js
    var replaceUnprintableStringChars = function (s) {
      var ret = [], i;
      for (i = 0; i < s.length; i++) {
        var val = s.charCodeAt(i);
        switch(val) {
          case 7: ret.push('\\a'); break;
          case 8: ret.push('\\b'); break;
          case 9: ret.push('\\t'); break;
          case 10: ret.push('\\n'); break;
          case 11: ret.push('\\v'); break;
          case 12: ret.push('\\f'); break;
          case 13: ret.push('\\r'); break;
          case 34: ret.push('\\"'); break;
          case 92: ret.push('\\\\'); break;
          default: 
            if (val >= 32 && val <= 126) {
              ret.push( s.charAt(i) );
            }
            else {
              var numStr = val.toString(16).toUpperCase();
              while (numStr.length < 4) {
                numStr = '0' + numStr;
              }
              ret.push('\\u' + numStr);
            }
            break;
        }
      }
      return ret.join('');
    };

    var escapeString = function (s) {
        return '"' + replaceUnprintableStringChars(s) + '"';
    };

    function toReprLoop(val, method) {
      var stack = [{todo: [val], done: []}];
      function toReprHelp() {
        while (stack.length > 0 && stack[0].todo.length > 0) {
          var top = stack[stack.length - 1];
          if (top.todo.length > 0) {
            var next = top.todo[top.todo.length - 1];
            if (isNumber(next)) {
              top.todo.pop();
              top.done.push(String(/**@type {!PNumber}*/ (next)));
            } else if (isBoolean(next)) {
              top.todo.pop();
              top.done.push(String(/**@type {!PBoolean}*/ (next)));
            } else if (isString(next)) {
              top.todo.pop();
              if (method === "_torepr") {
                top.done.push('"' + replaceUnprintableStringChars(String(/**@type {!PString}*/ (next))) + '"');
              } else {
                top.done.push(String(/**@type {!PString}*/ (next)));
              }
            } else if (isNothing(next)) {
              top.todo.pop();
              top.done.push("nothing");
            } else if (isObject(next)) {
              if (next.dict[method]) {
                // If this call fails
                var s = getField(next, method).app();
                // the continuation stacklet will get the result value, and do the next two steps manually
                top.todo.pop();
                top.done.push(thisRuntime.unwrap(s));
              } else { // Push the fields of this nested object onto the work stack
                var keys = [];
                var vals = [];
                for (var field in next.dict) {
                  keys.push(field); // NOTE: this is reversed order from the values,
                  vals.unshift(next.dict[field]); // because processing will reverse them back
                }
                stack.push({todo: vals, done: [], keys: keys});
              }
            } else if (isFunction(next)) {
              top.todo.pop();
              top.done.push("<function>");
            } else if (isMethod(next)) {
              top.todo.pop();
              top.done.push("<method>");
            } else {
              top.todo.pop();
              top.done.push(String(next));
            }
          } else { // All fields of a nested object have been stringified; collapse
            stack.pop();
            var prev = stack[stack.length - 1];
            prev.todo.pop();
            var s = "{";
            for (var i = 0; i < top.keys.length; i++) {
              if (i > 0) { s += ", "; }
              s += top.keys[i] + ": " + top.done[i];
            }
            s += "}";
            prev.done.push(s);
          }
        }
        return stack[0].done[0];
      }
      function toReprFun($ar) {
        var $step = 0;
        var $ans = undefined;
        try {
          if (thisRuntime.isActivationRecord($ar)) {
            $step = $ar.step;
            $ans = $ar.ans;
//            console.log("Resuming toReprFun with ans = " + JSON.stringify($ans));
          }
          while(true) {
            switch($step) {
            case 0:
              $step = 1;
              return toReprHelp();
            case 1:
              if (stack.length === 0) {
                ffi.throwInternalError("Somehow we've drained the toRepr worklist, but have results coming back");
              }
              var top = stack[stack.length - 1];
              top.todo.pop();
              top.done.push(thisRuntime.unwrap($ans));
              $step = 0;
              break;
            }
          }
        } catch($e) {
          if (thisRuntime.isCont($e)) {
            $e.stack[thisRuntime.EXN_STACKHEIGHT++] = thisRuntime.makeActivationRecord(
              ["runtime torepr"],
              toReprFun,
              $step,
              undefined, // This answer will be filled in with the nested answer of toReprHelp
              [],
              []);
          }
          if (thisRuntime.isPyretException($e)) {
            $e.pyretStack.push(["runtime torepr"]);
          }
          throw $e;
        }
      }
      return toReprFun();
    }
      
    /**
      Creates the js string representation for the value
      @param {!PBase} val

      @return {!string} the value given in
    */
    function toReprJS(val, method) {
      if (isNumber(val)) { return String(val); }
      else if (isBoolean(val)) { return String(val); }
      else if (isString(val)) {
        if (method === "_torepr") {
          return ('"' + replaceUnprintableStringChars(String(/**@type {!PString}*/ (val))) + '"');
        } else {
          return String(/**@type {!PString}*/ (val));
        }
      }
      return toReprLoop(val, method);
    }

    /**@type {PFunction} */
    var torepr = makeFunction(function(val) {return makeString(toReprJS(val, "_torepr"));});
    var tostring = makeFunction(function(val) {
        if(isString(val)) {
          return makeString(val);
        }
        else {
          return makeString(toReprJS(val, "tostring"));
        }
      });

    var print = makeFunction(
    /**
      Prints the value to the world by passing the repr to stdout
      @param {!PBase} val

      @return {!PBase} the value given in
    */
       function(val){
        display.app(val);
        theOutsideWorld.stdout("\n");
        return val;
    });

    var display = makeFunction(
    /**
      Prints the value to the world by passing the repr to stdout
      @param {!PBase} val

      @return {!PBase} the value given in
    */
       function(val){
        if (isString(val)) {
          var repr = val;
        }
        else {
          var repr = toReprJS(val, "tostring");
        }
        theOutsideWorld.stdout(repr);
        return val;
    });

    var print_error = makeFunction(
    /**
      Prints the value to the world by passing the repr to stderr
      @param {!PBase} val

      @return {!PBase} the value given in
    */
       function(val){
        display_error.app(val);
        theOutsideWorld.stderr("\n");
        return val;
    });

    var display_error = makeFunction(
    /**
      Prints the value to the world by passing the repr to stderr
      @param {!PBase} val

      @return {!PBase} the value given in
    */
       function(val){
        if (isString(val)) {
          var repr = val;
        }
        else {
          var repr = toReprJS(val, "tostring");
        }
        theOutsideWorld.stderr(repr);
        return val;
    });

    /********************
         Exceptions
     *******************/

    /**
      An Exception that represents a pyret exception

      @param {!PBase} e the value to raise
      @constructor
    */
    function PyretFailException(e) {
      this.exn = e;
      this.pyretStack = [];
      this.stack = (new Error).stack;
    }

    /**
      Tests if result is a PyretException
      @param {Object} val the value to test
      @return {boolean} true if it is a FailueResult
    */
    function isPyretException(val) { return val instanceof PyretFailException; }
    PyretFailException.prototype.toString = function() {
      var stackStr = this.pyretStack && this.pyretStack.length > 0 ? 
        this.getStack().map(function(s) {
            var g = getField;
            return s && hasField.app(s, "source") ? g(s, "source") +
                   " at " +
                   g(s, "start-line") +
                   ":" +
                   g(s, "start-column")
              : "<builtin>";
          }).join("\n") :
        "<no stack trace>";
      return toReprJS(this.exn, "tostring") + "\n" + stackStr;
    };
    PyretFailException.prototype.getStack = function() {
      return this.pyretStack.map(makeSrcloc);
    };

    function makeSrcloc(arr) {
      if (typeof arr === "object" && arr.length === 1) {
        checkString(arr[0]);
        return getField(srcloc, "builtin").app(arr[0])
      }
      else if (typeof arr === "object" && arr.length === 7) {
        return getField(srcloc, "srcloc").app(
            arr[0], arr[1], arr[2], arr[3], arr[4], arr[5], arr[6]
          )
      }
    }

    function makePyretFailException(exn) {
      return new PyretFailException(exn);
    }

    /**
      Raises a PyretFailException with the given string
      @param {!string} str
      @return {!PyretFailException}
    */
    function makeMessageException(str) {
      ffi.throwMessageException(str);
    }

    var raiseJSJS = 
      /**
        Raises any Pyret value as an exception
        @param {!PBase} val the value to raise
      */
      function(val) { 
        throw new PyretFailException(val); 
      };
    /** type {!PFunction} */
    var raisePyPy = makeFunction(raiseJSJS);

    /** type {!PFunction} */
    var hasField = makeFunction(
        /**
          Checks if an object has a given field
          @param {!PBase} obj The object to test
          @param {!PBase} str The field to test for, signals error if non-string
          @return {!PBase} 
        */
        function(obj, str) {
          checkString(str);
          return makeBoolean(hasProperty(obj.dict, str));
        }
      );

    function sameBrands(brands1, brands2) {
      if (brands1.brandCount !== brands2.brandCount) { return false; }
      for(var i in brands1) {
        if(brands1[i] !== brands2[i]) { return false; }
      }
      return true;
    }
    // JS function from Pyret values to JS booleans
    // Needs to be a worklist algorithm to avoid blowing the stack
    function same(left, right) {
      if (left === right) { return true; }

      var toCompare = [{left: left, right: right}];
      var current, curLeft, curRight;
      // Hunts for differences in the worklist, returning false when it finds them.
      // "continue" is equivalent to a recursive call returning "true", false
      // is returned directly when it is encountered
      while(toCompare.length > 0) {
        current = toCompare.pop();
        left = current.left;
        right = current.right;
        if (left === right) { continue; }
        if (isNumber(left) && isNumber(right) && jsnums.equals(left, right)) {
          continue;
        }
        // redundant becase it's just === now
        // else if (isString(left) && isString(right) && left.s === right.s) {
        //   continue;
        // }
        // else if (isBoolean(left) && isBoolean(right) && left.b === right.b) {
        //   continue;
        // }
        else if (isFunction(left) && isFunction(right) && left === right) {
          continue;
        }
        else if (isMethod(left) && isMethod(right) && left === right) {
          continue;
        }
        else if (isOpaque(left) && isOpaque(right) && left.equals(left.val, right.val)) {
          continue;
        }
        else if (isArray(left) && isArray(right)) {
          if (left.length !== right.length) { return false; }
          for (var i = 0; i < left.length; i++) {
            toCompare.push({
              left: left[i],
              right: right[i]
            });
          }
        }
        else if (isObject(left) && isObject(right)) {
          var dictLeft = left.dict;
          var dictRight = right.dict;
          var fieldsLeft;
          var fieldsRight;
          // Fast case, for objects that get extended with similar patterns
          // (e.g. variants of data have same proto), just check own props
          if(getProto(dictLeft) === getProto(dictRight)) {
            fieldsLeft = Object.keys(dictLeft);
            fieldsRight = Object.keys(dictRight);
            if(fieldsLeft.length !== fieldsRight.length) { return false; }
            for(var k = 0; k < fieldsLeft.length; k++) {
              toCompare.push({
                  left: left.dict[fieldsLeft[k]],
                  right: right.dict[fieldsLeft[k]]
                });
            }
          }
          // Slower case, just iterate all fields, all the way down to the bottom
          else {
            fieldsLeft = getFields(left);
            fieldsRight = getFields(right);
            if(fieldsLeft.length !== fieldsRight.length) { return false; }
            for(var k = 0; k < fieldsLeft.length; k++) {
              toCompare.push({
                  left: left.dict[fieldsLeft[k]],
                  right: right.dict[fieldsLeft[k]]
                });
            }
          }
          if(!sameBrands(getBrands(left), getBrands(right))) {
            return false;
          }
          // continue would be inappropriate (but not incorrect)
          // here, because we have enqueued things
        }
        else {
          // In all other cases, things are not equal
          return false;
        }
      }

      return true;

    };
    // Pyret function from Pyret values to Pyret booleans
    var samePyPy = makeFunction(function(v1, v2) { return makeBoolean(same(v1, v2)); });
    // JS function from Pyret values to Pyret booleans
    var sameJSPy = function(v1, v2) { return makeBoolean(same(v1, v2)); };

    var gensymCounter = Math.floor(Math.random() * 1000);
    var gensym = makeFunction(function(base) {
        checkString(base);
        return makeString(unwrap(base) + String(gensymCounter++))
      });

    // These are all intentional no-ops.  Some checker needs to be
    // defined by default in order to bootstrap libraries (since
    // all Pyret modules might use these functions to try and run
    // tests).  This one simply discards all the tests, and is
    // an appropriate choice for, say, loading a checker library.
    // See src/arr/trove/checker.arr for the default check
    // implementation in Pyret that is used by the standard evaluator
    var nullChecker = makeObject({
      "run-checks": makeFunction(function(moduleName, checks) {
        return nothing;
      }),
      "check-is": makeFunction(function(code, left, right, loc) {
        return nothing;
      }),
      "check-satisfies": makeFunction(function(code, left, pred, loc) {
        return nothing;
      }),
      "results": makeFunction(function() {
        return nothing;
      })
    });

    setParam("current-checker", nullChecker);

    /** type {!PBase} */
    var builtins = makeObject({
        'has-field': hasField,
        'equiv': samePyPy,
        'current-checker': makeFunction(function() {
          return getParam("current-checker");
        })
      });


    function unwrap(v) {
      if(isNumber(v)) { return v; }
      else if(isString(v)) { return v; }
      else if(isBoolean(v)) { return v; }
      else if(isObject(v)) { return v; }
      else if(isOpaque(v)) { return v; }
      else { ffi.throwInternalError("Cannot unwrap", [v]); }
    }

    function wrap(v) {
      if(jsnums.isSchemeNumber(v)) { return makeNumberBig(v); }
      else if(typeof v === "number") { return makeNumber(v); }
      else if(typeof v === "string") { return makeString(v); }
      else if(typeof v === "boolean") { return makeBoolean(v); }
      else if(isOpaque(v)) { return v; }
      else if(isObject(v)) { return v; }
      else { ffi.throwInternalError("Cannot wrap", v); }
    }

    function mkPred(jsPred) {
      return makeFunction(function(v) { return makeBoolean(jsPred(v)); });
    }

    /********************

     *******************/

    /********************
           Results
     *******************/
    /**
      Result containing the value of a successful evaluation

      @constructor
      @param {!PBase} r result value
    */
    function SuccessResult(r, stats) {
      this.result = r;
      this.stats = stats;
    }

    /**
      Tests if result is a successResult
      @param {Object} val the value to test
      @return {boolean} true if it is a SuccessResult
    */
    function isSuccessResult(val) { return val instanceof SuccessResult; }
    function makeSuccessResult(r) { return new SuccessResult(r); }

    /**
      Result containing the exception of a failed evaluation

      @constructor
      @param {!Error} e exception's value
    */
    function FailureResult(e, stats) {
      this.exn = e;
      this.stats = stats;
    }
    FailureResult.prototype.toString = function() {
      return "FailureResult(" + this.exn + ")";
    };
    /**
      Tests if result is a FailueResult
      @param {Object} val the value to test
      @return {boolean} true if it is a FailueResult
    */
    function isFailureResult(val) { return val instanceof FailureResult; }
    function makeFailureResult(e) { 
      //console.log("NEW RUNTIME FAILURE!!!!!!!!!!!!!!!!!!");
      //try { nonexistent.bad; } catch(exn) { console.log("In mFR: stack = " + exn.stack); }
      return new FailureResult(e); 
    }

    /**
      Represents a continuation
      @constructor
    */

    function Cont(stack) {
      this.stack = stack;
    }
    function makeCont() { return new Cont([]); }
    function isCont(v) { return v instanceof Cont; }

    function Pause(stack, pause, resumer) {
      this.stack = stack;
      this.pause = pause;
      this.resumer = resumer;
    }
    function makePause(pause, resumer) { return new Pause([], pause, resumer); }
    function isPause(v) { return v instanceof Pause; }
    Pause.prototype = Object.create(Cont.prototype);

    function safeTail(fun) {
      return fun();
    }

    function safeCall(fun, after, stackFrame) {
      var $ans = undefined;
      var $step = 0;
      if (thisRuntime.isActivationRecord(fun)) {
        var $ar = fun;
        $step = $ar.step;
        $ans = $ar.ans;
        fun = $ar.args[0];
        after = $ar.args[1];
        stackFrame = $ar.args[2];
        $fun_ans = $ar.vars[0];
      }
      try {
        while(true) {
          // console.log("In safeCall2, step ", $step, ", GAS = ", thisRuntime.GAS);
          if (--thisRuntime.GAS <= 0) {
            thisRuntime.EXN_STACKHEIGHT = 0;
            throw thisRuntime.makeCont();
          }
          switch($step) {
          case 0:
            $step = 1;
            $ans = fun();
            break;
          case 1:
            var $fun_ans = $ans;
            $step = 2;
            $ans = after($fun_ans);
            break;
          case 2: return $ans;
          }
        }
      } catch($e) {
        if (thisRuntime.isCont($e)) {
          $e.stack[thisRuntime.EXN_STACKHEIGHT++] =
            thisRuntime.makeActivationRecord(
              "safeCall2 for " + stackFrame,
              safeCall,
              $step,
              $ans,
              [ fun, after, stackFrame ],
              [ $fun_ans ]
            );
        }
        if (thisRuntime.isPyretException($e)) {
          $e.pyretStack.push(stackFrame);
        }
        throw $e;
      }
    }



    var RUN_ACTIVE = false;
    var currentThreadId = 0;
    var activeThreads = {};
      

  function run(program, namespace, options, onDone) {
    // console.log("In run2");
    if(RUN_ACTIVE) {
      onDone(makeFailureResult(ffi.makeMessageException("Internal: run called while already running")));
      return;
    }
    RUN_ACTIVE = true;
    var start;
    function startTimer() {
      if (typeof window !== "undefined" && window.performance) {
        start = window.performance.now();
      } else if (typeof process !== "undefined" && process.hrtime) {
        start = process.hrtime();
      }
    }
    function endTimer() {
      if (typeof window !== "undefined" && window.performance) {
        return window.performance.now() - start; 
      } else if (typeof process !== "undefined" && process.hrtime) {
        return process.hrtime(start);
      }
    }
    function getStats() {
      return { bounces: BOUNCES, tos: TOS, time: endTimer() };
    }
    function finishFailure(exn) {
      RUN_ACTIVE = false;
      delete activeThreads[thisThread.id];
      onDone(makeFailureResult(exn, getStats()));
    }
    function finishSuccess(answer) {
      RUN_ACTIVE = false;
      delete activeThreads[thisThread.id];
      onDone(new SuccessResult(answer, getStats()));
    }

    startTimer();
    var that = this;
    var theOneTrueStackTop = ["top-of-stack"]
    var kickoff = makeActivationRecord(
      "<top of stack>",
      makeFunction(function topOfStack(ignored) {
        return program(thisRuntime, namespace);
      }),
      0,
      {},
      [],
      []
    );
    var theOneTrueStack = [kickoff];
    var theOneTrueStart = {};
    var val = theOneTrueStart;
    var theOneTrueStackHeight = 1;
    var BOUNCES = 0;
    var TOS = 0;

    var sync = options.sync || false;
    var initialGas = options.initialGas || INITIAL_GAS;

    var threadIsCurrentlyPaused = false;
    var threadIsDead = false;
    currentThreadId += 1;
    // Special case of the first thread to run in between breaks.
    // This is the only thread notified of the break, others just die
    // silently.
    if(Object.keys(activeThreads).length === 0) {
      var breakFun = function() {
        threadIsCurrentlyPaused = true;
        threadIsDead = true;
        finishFailure(new PyretFailException(ffi.userBreak));
      };
    }
    else {
      var breakFun = function() {
        threadIsCurrentlyPaused = true;
        threadIsDead = true;
      };
    }

    var thisThread = {
      handlers: {
        resume: function(restartVal) {
          if(!threadIsCurrentlyPaused) { throw new Error("Stack already running"); }
          if(threadIsDead) { throw new Error("Failed to resume; thread has been killed"); }
          threadIsCurrentlyPaused = false;
          val = restartVal;
          // console.log("Resuming, with val = " + JSON.stringify(val, null, "  "));
          TOS++;
          RUN_ACTIVE = true;
          setTimeout(iter, 0);
        },
        break: breakFun,
        error: function(errVal) {
          threadIsCurrentlyPaused = true;
          threadIsDead = true;
          finishFailure(new PyretFailException(errVal));
        }
      },
      pause: function() {
        threadIsCurrentlyPaused = true;
      },
      id: currentThreadId
    };
    activeThreads[currentThreadId] = thisThread;

    // iter :: () -> Undefined
    // This function should not return anything meaningful, as state
    // and fallthrough are carefully managed.
    function iter() {
      // console.log("In run2::iter, GAS is ", thisRuntime.GAS);
      // If the thread is dead, return has already been processed
      if (threadIsDead) {
        return;
      }
      // If the thread is paused, something is wrong; only resume() should
      // be used to re-enter
      if (threadIsCurrentlyPaused) { throw new Error("iter entered during stopped execution"); }
      var loop = true;
      while (loop) {
        loop = false;
        try {
          if (manualPause !== null) {
            var thePause = manualPause;
            manualPause = null;
            pauseStack(function(restarter) {
              thePause.setHandlers({
                resume: function() { restarter.resume(val); },
                break: restarter.break,
                error: restarter.error
              });
            });
          }
          var frameCount = 0;
          while(theOneTrueStackHeight > 0) {
            if(!sync && frameCount++ > 100) {
              TOS++;
              // console.log("Setting timeout to resume iter");
              setTimeout(iter, 0);
              return;
            }
            var next = theOneTrueStack[--theOneTrueStackHeight];
            // console.log("ActivationRecord[" + theOneTrueStackHeight + "] = " + JSON.stringify(next, null, "  "));
            theOneTrueStack[theOneTrueStackHeight] = undefined;
            // console.log("theOneTrueStack = ", theOneTrueStack);
            // console.log("Setting ans to " + JSON.stringify(val, null, "  "));
            next.ans = val;
            // console.log("GAS = ", thisRuntime.GAS);
            if (isFunction(next.fun))
              val = next.fun.app(next);
            else if (next.fun instanceof Function)
              val = next.fun(next);
            else if (!(next instanceof ActivationRecord)) {
              console.log("Our next stack frame doesn't look right!");
              console.log(JSON.stringify(next));
              console.log(theOneTrueStack);
              throw false;
            }
            // console.log("Frame returned, val = " + JSON.stringify(val, null, "  "));
          }
        } catch(e) {
          if(thisRuntime.isCont(e)) {
            // console.log("BOUNCING");
            BOUNCES++;
            thisRuntime.GAS = initialGas;
            for(var i = e.stack.length - 1; i >= 0; i--) {
              theOneTrueStack[theOneTrueStackHeight++] = e.stack[i];
            }
            // console.log("The new stack height is ", theOneTrueStackHeight);
            // console.log("theOneTrueStack = ", theOneTrueStack);

            if(isPause(e)) {
              thisThread.pause();
              e.pause.setHandlers(thisThread.handlers);
              if(e.resumer) { e.resumer(e.pause); }
              return;
            }
            else if(thisRuntime.isCont(e)) {
              val = theOneTrueStack[theOneTrueStackHeight - 1].ans;
              if(sync) {
                loop = true;
                // DON'T return; we synchronously loop back to the outer while loop
                continue;
              }
              else {
                TOS++;
                setTimeout(iter, 0);
                return;
              }
            }
          }

          else if(isPyretException(e)) {
            while(theOneTrueStackHeight > 0) {
              var next = theOneTrueStack[--theOneTrueStackHeight];
              theOneTrueStack[theOneTrueStackHeight] = "sentinel";
              e.pyretStack.push(next.from);
            }
            finishFailure(e);
            return;
          } else {
            console.log("Wait, what?");
            console.log(e);
            finishFailure(e);
            return;
          }
        }
      }
      finishSuccess(val);
      return;
    }
    thisRuntime.GAS = initialGas;
    iter();
  }

    function runThunk(f, then) {
      return run(f, thisRuntime.namespace, {}, then);
    }


  function ActivationRecord(from, fun, step, ans, args, vars) {
    this.from = from;
    this.fun = fun; 
    this.step = step;
    this.ans = ans;
    this.args = args;
    this.vars = vars;
  }  
  ActivationRecord.prototype.toString = function() { 
    return "{from: " + this.from + ", fun: " + this.fun + ", step: " + this.step
      + ", ans: " + JSON.stringify(this.ans) + ", args: " + JSON.stringify(this.args)
      + ", vars: " + JSON.stringify(this.vars) + "}";
  }
  function makeActivationRecord(from, fun, step, ans, args, vars) {
    return new ActivationRecord(from, fun, step, ans, args, vars);
  }
  function isActivationRecord(obj) {
    return obj instanceof ActivationRecord;
  }

    function printPyretStack(stack) {
      if (stack === undefined) return "  undefined";
      var stackStr = stack.map(function(val) {
        if (val instanceof Array && val.length == 7) {
          return (val[0] + ": line " + val[1] + ", column " + val[2]);
        } else if (val) {
          return JSON.stringify(val);
        }
      });
      return "  " + stackStr.join("\n  ");
    }

    function breakAll() {
      RUN_ACTIVE = false;
      var threadsToBreak = activeThreads;
      var keys = Object.keys(threadsToBreak);
      activeThreads = {};
      for(var i = 0; i < keys.length; i++) {
        threadsToBreak[keys[i]].handlers.break();
      }
    }

    function pauseStack(resumer) {
      RUN_ACTIVE = false;
      thisRuntime.EXN_STACKHEIGHT = 0;
      var pause = new PausePackage();
      throw makePause(pause, resumer);
    }

    function PausePackage() {
      this.resumeVal = null;
      this.errorVal = null;
      this.breakFlag = false;
      this.handlers = null;
    }
    PausePackage.prototype = {
      setHandlers: function(handlers) {
        if(this.breakFlag) {
          handlers.break();
        }
        else if (this.resumeVal !== null) {
          handlers.resume(this.resumeVal);
        }
        else if (this.errorVal !== null) {
          handlers.error(this.errorVal);
        }
        else {
          this.handlers = handlers;
        }
      },
      break: function() {
        if(this.resumeVal !== null || this.errorVal !== null) {
          throw "Cannot break with resume or error requested";
        }
        if(this.handlers !== null) {
          this.handlers.break();
        }
        else {
          this.breakFlag = true;
        }
      },
      error: function(err) {
        if(this.resumeVal !== null || this.breakFlag) {
          throw "Cannot error with resume or break requested";
        }
        if(this.handlers !== null) {
          this.handlers.error(err);
        }
        else {
          this.errorVal = err;
        }
      },
      resume: function(val) {
        if(this.errorVal !== null || this.breakFlag) {
          throw "Cannot resume eith error or break requested";
        }
        if(this.handlers !== null) {
          this.handlers.resume(val);
        }
        else {
          this.resumeVal = val;
        }
      }
    };

    var manualPause = null;
    function schedulePause(resumer) {
      var pause = new PausePackage();
      manualPause = pause;
      resumer(pause);
    }

    function execThunk(thunk) {
      function wrapResult(res) {
        if(isSuccessResult(res)) {
          return ffi.makeLeft(res.result);
        } else if (isFailureResult(res)) {
          if(isPyretException(res.exn)) {
            return ffi.makeRight(res.exn.exn);
          }
          else {
            return ffi.makeRight(makeMessageException(String(res.exn)));
          }
        } else {
          console.error("Bad execThunk result: ", res);
          return;
        }
      }
      thisRuntime.pauseStack(function(restarter) {
        thisRuntime.run(function(_, __) {
            return thunk.app();
          }, thisRuntime.namespace, {
            sync: false
          }, function(result) {
            if(isFailureResult(result) &&
               isPyretException(result.exn) &&
               ffi.isUserBreak(result.exn.exn)) { restarter.break(); }
            else {
              restarter.resume(wrapResult(result))
            }
          });
      });
    }

    var INITIAL_GAS = theOutsideWorld.initialGas || 1000;

    var DEBUGLOG = true;
    /**
      @type {function(...[?]): undefined}
    */
    var log = function() {
      if(DEBUGLOG) { console.log.apply(console, arguments); }
    }

    var plus = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkNumber(r);
        return thisRuntime.makeNumberBig(jsnums.add(l, r));
      } else if (thisRuntime.isString(l)) {
        thisRuntime.checkString(r);
        return thisRuntime.makeString(l.concat(r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_plus")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_plus").app(r);
          });
      } else {
        ffi.throwPlusError(l, r);        
      }
    };

    var minus = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkNumber(r)
        return thisRuntime.makeNumberBig(jsnums.subtract(l, r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_minus")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_minus").app(r);
          });
      } else {
        throw makeMessageException("First argument to _minus was not a number, or did not have a _minus method: " + JSON.stringify(l));
      }
    };

    var times = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkNumber(r);
        return thisRuntime.makeNumberBig(jsnums.multiply(l, r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_times")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_times").app(r);
          });
      } else {
        throw makeMessageException("First argument to _times was not a number, or did not have a _times method: " + JSON.stringify(l));
      }
    };

    var divide = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkNumber(r);
        if (jsnums.equals(0, r)) {
          throw makeMessageException("Division by zero");
        }
        return thisRuntime.makeNumberBig(jsnums.divide(l, r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_divide")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_divide").app(r);
          });
      } else {
        throw makeMessageException("First argument to _divide was not a number, or did not have a _divide method: " + JSON.stringify(l));
      }
    };

    var lessthan = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkNumber(r);
        return thisRuntime.makeBoolean(jsnums.lessThan(l, r));
      } else if (thisRuntime.isString(l)) {
        thisRuntime.checkString(r);
        return thisRuntime.makeBoolean(l < r);
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_lessthan")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_lessthan").app(r);
          });
      } else {
        throw makeMessageException("First argument to _lessthan was not a number, or did not have a _lessthan method: " + JSON.stringify(l));
      }
    };

    var greaterthan = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkNumber(r);
        return thisRuntime.makeBoolean(jsnums.greaterThan(l, r));
      } else if (thisRuntime.isString(l)) {
        thisRuntime.checkString(r);
        return thisRuntime.makeBoolean(l > r);
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_greaterthan")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_greaterthan").app(r);
          });
      } else {
        throw makeMessageException("First argument to _greaterthan was not a number, or did not have a _greaterthan method: " + JSON.stringify(l));
      }
    };

    var lessequal = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkNumber(r);
        return thisRuntime.makeBoolean(jsnums.lessThanOrEqual(l, r));
      } else if (thisRuntime.isString(l)) {
        thisRuntime.checkString(r);
        return thisRuntime.makeBoolean(l <= r);
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_lessequal")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_lessequal").app(r);
          });
      } else {
        throw makeMessageException("First argument to _lessequal was not a number, or did not have a _lessequal method: " + JSON.stringify(l));
      }
    };

    var greaterequal = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkNumber(r);
        return thisRuntime.makeBoolean(jsnums.greaterThanOrEqual(l, r));
      } else if (thisRuntime.isString(l)) {
        thisRuntime.checkString(r);
        return thisRuntime.makeBoolean(l >= r);
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_greaterequal")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_greaterequal").app(r);
          });
      } else {
        throw makeMessageException("First argument to _greaterequal was not a number, or did not have a _greaterequal method: " + JSON.stringify(l));
      }
    };

    var checkArrayIndex = function(methodName, arr, ix) {
      var throwErr = function(reason) {
        ffi.throwInvalidArrayIndex(methodName, arr, ix, reason);
      };
      if(ix >= arr.length) {
        throwErr("index too large; array length was " + arr.length);
      }
      if(ix < 0) {
        throwErr("negative index");
      }
      if(!(num_is_integer(ix))) {
        throwErr("non-integer index");
      }
    }

    var raw_array_of = function(val, len) {
      thisRuntime.checkArity(2, arguments, "raw-array-of");
      thisRuntime.checkNumber(len);
      var arr = new Array(len);
      var i = 0;
      while(i < len) {
        arr[i++] = val;
      }
      return arr;
    }

    var raw_array_get = function(arr, ix) {
      thisRuntime.checkArity(2, arguments, "raw-array-get");
      thisRuntime.checkArray(arr);
      thisRuntime.checkNumber(ix);
      checkArrayIndex("raw-array-get", arr, ix);
      return arr[ix];
    };

    var raw_array_set = function(arr, ix, newVal) {
      thisRuntime.checkArity(3, arguments, "raw-array-set");
      thisRuntime.checkArray(arr);
      thisRuntime.checkNumber(ix);
      checkArrayIndex("raw-array-set", arr, ix);
      arr[ix] = newVal;
      return arr;
    };

    var raw_array_length = function(arr) {
      thisRuntime.checkArity(1, arguments, "raw-array-length");
      thisRuntime.checkArray(arr);
      return makeNumber(arr.length);
    };

    var raw_array_to_list = function(arr) {
      thisRuntime.checkArity(1, arguments, "raw-array-to-list");
      thisRuntime.checkArray(arr);
      return ffi.makeList(arr);
    };

    var raw_array_fold = function(f, init, arr, start) {
      thisRuntime.checkArity(4, arguments, "raw-array-fold");
      thisRuntime.checkFunction(f);
      thisRuntime.checkPyretVal(init);
      thisRuntime.checkArray(arr);
      thisRuntime.checkNumber(start);
      var currentIndex = -1;
      var currentAcc = init;
      var length = arr.length;
      function foldHelp() {
        while(++currentIndex < length) {
          currentAcc = f.app(currentAcc, arr[currentIndex], currentIndex + start);
        }
        return currentAcc;
      }
      function foldFun($ar) {
        try {
          if (thisRuntime.isActivationRecord($ar)) {
            currentAcc = $ar.ans;
          }
          return foldHelp();
        } catch ($e) {
          if (thisRuntime.isCont($e)) {
            $e.stack[thisRuntime.EXN_STACKHEIGHT++] = thisRuntime.makeActivationRecord(
              ["raw-array-fold"],
              foldFun,
              0, // step doesn't matter here
              undefined, // answer will be filled in by stack unwinder
              [], []);
          }
          if (thisRuntime.isPyretException($e)) {
            $e.pyretStack.push(["raw-array-fold"]);
          }
          throw $e;
        }
      }
      return foldFun();
    };

    var string_substring = function(s, min, max) {
      thisRuntime.checkString(s);
      thisRuntime.checkNumber(min);
      thisRuntime.checkNumber(max);
      if(jsnums.greaterThan(min, max)) {
        throw makeMessageException("substring: min index " + String(min) + " is greater than max index " + String(max));
      }
      if(jsnums.lessThan(min, 0)) {
        throw makeMessageException("substring: min index " + String(min) + " is less than 0");
      }
      if(jsnums.greaterThan(max, string_length(s))) {
        throw makeMessageException("substring: max index " + String(max) + " is larger than the string length " + String(string_length(s)));
      }
      return thisRuntime.makeString(s.substring(jsnums.toFixnum(min), jsnums.toFixnum(max)));
    }
    var string_replace = function(s, find, replace) {
      thisRuntime.checkString(s);
      thisRuntime.checkString(find);
      thisRuntime.checkString(replace);
      var escapedFind = find.replace(/\\/g, "\\\\");
      return thisRuntime.makeString(s.replace(new RegExp(escapedFind,'g'), replace));
    }

    var string_equals = function(l, r) {
      thisRuntime.checkString(l);
      thisRuntime.checkString(r);
      return thisRuntime.makeBoolean(same(l, r));
    }
    var string_append = function(l, r) {
      thisRuntime.checkString(l);
      thisRuntime.checkString(r);
      return thisRuntime.makeString(l.concat(r));
    }
    var string_contains = function(l, r) {
      thisRuntime.checkString(l);
      thisRuntime.checkString(r);
      return thisRuntime.makeBoolean(l.indexOf(r) !== -1);
    }
    var string_length = function(s) {
      thisRuntime.checkString(s);
      return thisRuntime.makeNumber(s.length);
    }
    var string_tonumber = function(s) {
      thisRuntime.checkString(s);
      var num = jsnums.fromString(s);
      if(num !== false) {
        return makeNumberBig(/**@type {Bignum}*/ (num));
      }
      else {
        return makeNothing();
      }
    }
    var string_repeat = function(s, n) {
      thisRuntime.checkString(s);
      thisRuntime.checkNumber(n);
      var resultStr = "";
      // TODO(joe): loop up to a fixnum?
      for(var i = 0; i < jsnums.toFixnum(n); i++) {
        resultStr += s;
      }
      return makeString(resultStr);
    }
    var string_split = function(s, splitstr) {
      thisRuntime.checkString(s);
      thisRuntime.checkString(splitstr);
      
      return ffi.makeList(s.split(splitstr).map(thisRuntime.makeString));
    }
    var string_charat = function(s, n) {
      thisRuntime.checkString(s);
      thisRuntime.checkNumber(n);
      
      //TODO: Handle bignums that are beyond javascript
      return thisRuntime.makeString(String(s.charAt(jsnums.toFixnum(n))));
    }
    var string_toupper = function(s) {
      thisRuntime.checkString(s);
      return thisRuntime.makeString(s.toUpperCase());
    }
    var string_tolower = function(s) {
      thisRuntime.checkString(s);
      return thisRuntime.makeString(s.toLowerCase());
    }
    var string_explode = function(s) {
      thisRuntime.checkString(s);
      return ffi.makeList(s.split("").map(thisRuntime.makeString));
    }
    var string_indexOf = function(s, find) {
      thisRuntime.checkString(s);
      thisRuntime.checkString(find);
      return thisRuntime.makeNumberBig(s.indexOf(find));
    }

    var bool_not = function(l) {
      checkArity(1, arguments, "not");
      thisRuntime.checkBoolean(l);
      return thisRuntime.makeBoolean(!l);
    }

    var num_equals = function(l, r) {
      thisRuntime.checkNumber(l);
      thisRuntime.checkNumber(r);
      return thisRuntime.makeBoolean(same(l, r));
    }
    var num_max = function(l, r) {
      thisRuntime.checkNumber(l);
      thisRuntime.checkNumber(r);
      if (jsnums.greaterThanOrEqual(l, r)) { return l; } else { return r; }
    }

    var num_min = function(l, r) {
      thisRuntime.checkNumber(l);
      thisRuntime.checkNumber(r);
      if (jsnums.lessThanOrEqual(l, r)) { return l; } else { return r; }
    }

    var num_abs = function(n) {
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.abs(n));
    }
      
    var num_sin = function(n) {
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.sin(n));
    }
    var num_cos = function(n) {
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.cos(n));
    }
    var num_tan = function(n) {
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.tan(n));
    }
    var num_asin = function(n) {
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.asin(n));
    }
    var num_acos = function(n) {
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.acos(n));
    }
    var num_atan = function(n) {
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.atan(n));
    }

    var num_modulo = function(n, mod) { 
      thisRuntime.checkNumber(n);
      thisRuntime.checkNumber(mod);
      return thisRuntime.makeNumberBig(jsnums.modulo(n, mod));
    }
    var num_truncate = function(n) { 
      thisRuntime.checkNumber(n);
      if (isNaN(n)) {
        return n;
      } else if (jsnums.greaterThanOrEqual(n, 0)) {
        return thisRuntime.makeNumberBig(jsnums.floor(n));
      } else {
        return thisRuntime.makeNumberBig(jsnums.ceiling(n));
      }
    }
    var num_sqrt = function(n) { 
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.sqrt(n));
    }
    var num_sqr = function(n) { 
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.sqr(n));
    }
    var num_ceiling = function(n) { 
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.ceiling(n));
    }
    var num_floor = function(n) { 
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.floor(n));
    }
    var num_log = function(n) {
      thisRuntime.checkNumber(n);
      if (jsnums.greaterThan(n, 0)) {
        return thisRuntime.makeNumberBig(jsnums.log(n));
      }
      else {
        throw makeMessageException("log: expected a number greater than 0 but got " + String(n));
      }
    }
    var num_exp = function(n) {
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.exp(n));
    }
    var num_exact = function(n) {
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.toExact(n));
    }
    var num_is_integer = function(n) {
      thisRuntime.checkNumber(n);
      return thisRuntime.makeBoolean(jsnums.isInteger(n))
    }
    var num_is_fixnum = function(n) {
      thisRuntime.checkNumber(n);
      return thisRuntime.makeBoolean(typeof n === "number");
    }
    var num_expt = function(n, pow) {
      thisRuntime.checkNumber(n);
      thisRuntime.checkNumber(pow);
      return thisRuntime.makeNumberBig(jsnums.expt(n, pow));
    }
    var num_tostring = function(n) {
      checkArity(1, arguments, "num-tostring");
      thisRuntime.checkNumber(n);
      return makeString(String(n));
    }
    var num_tostring_digits = function(n, digits) {
      checkArity(2, arguments, "num-tostring-digits");
      thisRuntime.checkNumber(n);
      thisRuntime.checkNumber(digits);
      var d = jsnums.toFixnum(digits);
      var tenDigits = jsnums.expt(10, digits);
      if (n != n) { return thisRuntime.makeString("NaN"); }
      else if (jsnums.equals(n, Number.POSITIVE_INFINITY)) { return thisRuntime.makeString("+inf"); }
      else if (jsnums.equals(n, Number.NEGATIVE_INFINITY)) { return thisRuntime.makeString("-inf"); }
      else if (jsnums.equals(n, jsnums.negative_zero)) { 
        var s = "-0.";
        for (var i = 0; i < d; i++)
          s += "0";
        return thisRuntime.makeString(s);
      } else if (jsnums.isReal(n)) {
        n = jsnums.divide(jsnums.round(jsnums.multiply(n, tenDigits)), tenDigits)
        var s = jsnums.toFixnum(n).toString().split(".");
        s[1] = (s[1] || "").substring(0, d);
        for (var i = s[1].length; i < d; i++)
          s[1] += "0";
        return thisRuntime.makeString(s[0] + "." + s[1]);
      } else {
        var nreal = jsnums.realPart(n);
        nreal = jsnums.divide(jsnums.round(jsnums.multiply(nreal, tenDigits)), tenDigits)
        var sreal = jsnums.toFixnum(nreal).toString().split(".");
        sreal[1] = (sreal[1] || "").substring(0, d);
        for (var i = sreal[1].length; i < d; i++)
          sreal[1] += "0";
        var nimag = jsnums.imaginaryPart(n);
        nimag = jsnums.divide(jsnums.round(jsnums.multiply(nimag, tenDigits)), tenDigits)
        var simag = jsnums.toFixnum(nimag).toString().split(".");
        simag[1] = (simag[1] || "").substring(0, d);
        for (var i = simag[1].length; i < d; i++)
          simag[1] += "0";
        if (simag[0][0] === "-" || simag[0][0] === "+") {
          return thisRuntime.makeString(sreal[0] + "." + sreal[1] + simag[0] + "." + simag[1] + "i");
        } else {
          return thisRuntime.makeString(sreal[0] + "." + sreal[1] + "+" + simag[0] + "." + simag[1] + "i");
        }
      }
    }
    function random(max) {
      return makeNumber(jsnums.floor(jsnums.multiply(Math.random(), max)));
    }

    function loadModule(module, runtime, namespace, withModule) {
      return thisRuntime.safeCall(function() {
          return module(thisRuntime, namespace);
        },
        function(m) { return withModule(getField(m, "provide")); },
        "loadModule");
    }
    function loadModules(namespace, modules, withModules) {
      function loadModulesInt(toLoad, loaded) {
        if(toLoad.length > 0) {
          var nextMod = toLoad.pop();
          return loadModule(nextMod, thisRuntime, namespace, function(m) {
            return safeTail(function() {
              loaded.unshift(m);
              return loadModulesInt(toLoad, loaded);
            });
          });
        }
        else {
          return safeTail(function() { return withModules.apply(null, loaded); });
        }
      }
      var modulesCopy = modules.slice(0, modules.length);
      return loadModulesInt(modulesCopy, []);
    }

    //Export the runtime
    //String keys should be used to prevent renaming
    var thisRuntime = {
        'namespace': Namespace.namespace({
          'torepr': torepr,
          'tostring': tostring,
          'test-print': print,
          'print': print,
          'display': display,
          'print-error': print_error,
          'display-error': display_error,
          'brander': brander,
          'raise': raisePyPy,
          'builtins': builtins,
          'nothing': nothing,
          'is-nothing': mkPred(isNothing),
          'is-number': mkPred(isNumber),
          'is-boolean': mkPred(isBoolean),
          'is-string': mkPred(isString),
          'is-function': mkPred(isFunction),
          'is-object': mkPred(isObject),
          'is-raw-array': mkPred(isArray),

          'run-task': makeFunction(execThunk),

          'gensym': gensym,
          'random': makeFunction(random),

          '_plus': makeFunction(plus),
          '_minus': makeFunction(minus),
          '_times': makeFunction(times),
          '_divide': makeFunction(divide),
          '_lessthan': makeFunction(lessthan),
          '_greaterthan': makeFunction(greaterthan),
          '_greaterequal': makeFunction(greaterequal),
          '_lessequal': makeFunction(lessequal),

          'num-max': makeFunction(num_max),
          'num-min': makeFunction(num_min),
          'nums-equal': makeFunction(num_equals),
          'num-abs': makeFunction(num_abs),
          'num-sin': makeFunction(num_sin),
          'num-cos': makeFunction(num_cos),
          'num-tan': makeFunction(num_tan),
          'num-asin': makeFunction(num_asin),
          'num-acos': makeFunction(num_acos),
          'num-atan': makeFunction(num_atan),
          'num-modulo': makeFunction(num_modulo),
          'num-truncate': makeFunction(num_truncate),
          'num-sqrt': makeFunction(num_sqrt),
          'num-sqr': makeFunction(num_sqr),
          'num-ceiling': makeFunction(num_ceiling),
          'num-floor': makeFunction(num_floor),
          'num-log': makeFunction(num_log),
          'num-exp': makeFunction(num_exp),
          'num-exact': makeFunction(num_exact),
          'num-is-integer': makeFunction(num_is_integer),
          'num-is-fixnum': makeFunction(num_is_fixnum),
          'num-expt': makeFunction(num_expt),
          'num-tostring': makeFunction(num_tostring),

          'strings-equal': makeFunction(string_equals),
          'string-contains': makeFunction(string_contains),
          'string-append': makeFunction(string_append),
          'string-length': makeFunction(string_length),
          'string-tonumber': makeFunction(string_tonumber),
          'string-repeat': makeFunction(string_repeat),
          'string-substring': makeFunction(string_substring),
          'string-replace': makeFunction(string_replace),
          'string-split': makeFunction(string_split),
          'string-char-at': makeFunction(string_charat),
          'string-toupper': makeFunction(string_toupper),
          'string-tolower': makeFunction(string_tolower),
          'string-explode': makeFunction(string_explode),
          'string-index-of': makeFunction(string_indexOf),

          'raw-array-of': makeFunction(raw_array_of),
          'raw-array-get': makeFunction(raw_array_get),
          'raw-array-set': makeFunction(raw_array_set),
          'raw-array-length': makeFunction(raw_array_length),
          'raw-array-to-list': makeFunction(raw_array_to_list),
          'raw-array-fold': makeFunction(raw_array_fold),

          'not': makeFunction(bool_not)

        }),
        'run': run,
        'runThunk': runThunk,
        'safeCall': safeCall,
        'safeTail': safeTail,
        'printPyretStack': printPyretStack,

        'isActivationRecord'   : isActivationRecord,
        'makeActivationRecord' : makeActivationRecord,

        'GAS': INITIAL_GAS,

        'makeCont'    : makeCont,
        'isCont'      : isCont,
        'makePause'   : makePause,
        'isPause'     : isPause,

        'pauseStack'  : pauseStack,
        'schedulePause'  : schedulePause,
        'breakAll' : breakAll,

        'getField'    : getField,
        'getFieldLoc'    : getFieldLoc,
        'getFields'    : getFields,
        'getColonField'    : getColonField,

        'hasBrand' : hasBrand,

        'isPyretTrue' : isPyretTrue,
        'isPyretFalse' : isPyretFalse,

        'isBase'      : isBase,
        'isNothing'   : isNothing,
        'isNumber'    : isNumber,
        'isString'    : isString,
        'isBoolean'   : isBoolean,
        'isFunction'  : isFunction,
        'isMethod'    : isMethod,
        'isObject'    : isObject,
        'isOpaque'    : isOpaque,
        'isPyretVal'  : isPyretVal,

        'makePyretFailException': makePyretFailException,

        'isSuccessResult' : isSuccessResult,
        'makeSuccessResult' : makeSuccessResult,
        'isFailureResult' : isFailureResult,
        'makeFailureResult' : makeFailureResult,
        'isPyretException' : isPyretException,

        'makeNothing'  : makeNothing,
        'makeNumber'   : makeNumber,
        'makeNumberBig'   : makeNumberBig,
        'makeNumberFromString'   : makeNumberFromString,
        'makeBoolean'  : makeBoolean,
        'makeString'   : makeString,
        'makeFunction' : makeFunction,
        'makeMethod'   : makeMethod,
        'makeMethodFromFun' : makeMethodFromFun,
        'makeObject'   : makeObject,
        'makeArray' : makeArray,
        'makeBrandedObject'   : makeBrandedObject,
        'makeOpaque'   : makeOpaque,

        'plus': plus,
        'minus': minus,
        'times': times,
        'divide': divide,
        'lessthan': lessthan,
        'greaterthan': greaterthan,
        'greaterequal': greaterequal,
        'lessequal': lessequal,

        'num_max': num_max,
        'num_min': num_min,
        'num_abs': num_abs,
        'num_sin': num_sin,
        'num_cos': num_cos,
        'num_tan': num_tan,
        'num_asin': num_asin,
        'num_acos': num_acos,
        'num_atan': num_atan,
        'num_modulo': num_modulo,
        'num_truncate': num_truncate,
        'num_sqrt': num_sqrt,
        'num_ceiling': num_ceiling,
        'num_floor': num_floor,
        'num_log': num_log,
        'num_exp': num_exp,
        'num_exact': num_exact,
        'num_is_integer': num_is_integer,
        'num_expt': num_expt,
        'num_tostring': num_tostring,

        'string_contains': string_contains,
        'string_append': string_append,
        'string_length': string_length,
        'string_tonumber': string_tonumber,
        'string_repeat': string_repeat,
        'string_substring': string_substring,
        'string_replace': string_replace,
        'string_split': string_split,
        'string_charat': string_charat,
        'string_toupper': string_toupper,
        'string_tolower': string_tolower,
        'string_explode': string_explode,
        'string_indexOf': string_indexOf,

        'raw_array_of': raw_array_of,
        'raw_array_get': raw_array_get,
        'raw_array_set': raw_array_set,
        'raw_array_length': raw_array_length,
        'raw_array_to_list': raw_array_to_list,


        'not': bool_not,

        'equiv': sameJSPy,
        'raise': raiseJSJS,

        'pyretTrue': pyretTrue,
        'pyretFalse': pyretFalse,

        'undefined': undefined,
        'create': Object.create,

        'hasField' : hasField.app,

        'toReprJS' : toReprJS,
        'toRepr' : function(val) { return toReprJS(val, "_torepr"); },

        'same' : same,
        'wrap' : wrap,
        'unwrap' : unwrap,

        'checkString' : checkString,
        'checkNumber' : checkNumber,
        'checkBoolean' : checkBoolean,
        'checkObject' : checkObject,
        'checkFunction' : checkFunction,
        'checkMethod' : checkMethod,
        'checkArray' : checkArray,
        'checkOpaque' : checkOpaque,
        'checkPyretVal' : checkPyretVal,
        'checkArity': checkArity,
        'makeCheckType' : makeCheckType,
        'checkIf'      : checkIf,
        'confirm'      : confirm,
        'makeMessageException'      : makeMessageException,
        'serial' : Math.random(),
        'log': log,

        'makeSrcloc': makeSrcloc,
        '_link': function(f, r) { return getField(list, "link").app(f, r); },

        'loadModule' : loadModule,
        'loadModules' : loadModules,
        'modules' : Object.create(null),
        'setStdout': function(newStdout) {
          theOutsideWorld.stdout = newStdout;
        },
        'getParam' : getParam,
        'setParam' : setParam,
        'hasParam' : hasParam
    };


    var list = getField(require("trove/lists")(thisRuntime, thisRuntime.namespace), "provide");
    var srcloc = getField(require("trove/srcloc")(thisRuntime, thisRuntime.namespace), "provide");
    var ffi = require("js/ffi-helpers")(thisRuntime, thisRuntime.namespace);
    thisRuntime["ffi"] = ffi;

    // NOTE(joe): set a few of these explicitly to work with s-prim-app
    thisRuntime["throwMessageException"] = ffi.throwMessageException;
    thisRuntime["throwNoBranchesMatched"] = ffi.throwNoBranchesMatched;
    thisRuntime["throwNonBooleanCondition"] = ffi.throwNonBooleanCondition;
    thisRuntime["throwNonBooleanOp"] = ffi.throwNonBooleanOp;

    var ns = thisRuntime.namespace;
    var nsWithList = ns.set("_link", getField(list, "link"))
                       .set("_empty", getField(list, "empty"));
    thisRuntime.namespace = nsWithList;

    var checkList = makeCheckType(ffi.isList, "List");
    thisRuntime["checkList"] = checkList;

    return thisRuntime;
}

return  {'makeRuntime' : makeRuntime};

       });
