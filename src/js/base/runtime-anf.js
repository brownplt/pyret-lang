/***
This is the runtime for the ANF'd version of pyret
*/
"use strict";
/** @typedef {!Object} */
var Bignum;


define(["require", "./namespace", "../../../lib/js-numbers/src/js-numbers"],
       function (require, Namespace, jsnumsIn) {



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
function getField(val, field) {
    if(val === undefined) {
      throw makeMessageException("FATAL: Tried to look up " + field + ", but object was undefined");
    }
    if(val.dict === undefined) {
      throw makeMessageException("FATAL: Tried to look up " + field + ", but object was: " + val);
    }
    var fieldVal = val.dict[field];
    if(fieldVal === undefined) {
        //TODO: Throw field not found error
        //NOTE: When we change JSON.stringify to toReprJS, we'll need to support
        //reentrant errors (see commit 24ff13d9e9)
        throw makeMessageException("field " + field + " not found on " + JSON.stringify(val));
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

/**
  Gets the field from an object of the given name
  -Returns the raw field value

  @param {!PBase} val
  @param {string} field

  @return {!PBase}
**/
function getColonField(val, field) {
    var fieldVal = val.dict[field];
    if(fieldVal === undefined) {
        throw makeMessageException("field " + val + " not found.");
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
  throw makeMessageException("Cannot extend opaque values");
};
POpaque.prototype.updateDict = function(dict, keepBrands) {
  throw makeMessageException("Cannot clone opaque values");
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
        throw makeMessageException("Could not create number from: " + s);
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

    function confirm(val, test) {
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
    /**
      Creates the js string representation for the value
      @param {!PBase} val

      @return {!string} the value given in
    */
    function toReprJS(val, method) {
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
      try {
        return toReprHelp();
      } catch(e) {
        if (thisRuntime.isCont(e)) {
          var stacklet = {
            from: topSrc(new Error()), near: "toRepr",
            go: function(ret) {
              if (stack.length === 0) {
                throw makeMessageException("Somehow we've drained the toRepr worklist, but have results coming back");
              }
              var top = stack[stack.length - 1];
              top.todo.pop();
              top.done.push(thisRuntime.unwrap(ret));
              return makeString(toReprHelp());
            }
          };
          e.stack[thisRuntime.EXN_STACKHEIGHT++] = stacklet;
          throw e;
        } else {
          if (thisRuntime.isPyretException(e)) {
            e.pyretStack.push(topSrc(new Error())); 
          }
          throw e;
        }
      }
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
        this.pyretStack.map(function(s) {
            return s ? s.src + " at " + s["start-line"] + ":" + (s["start-column"] + 1) : "<unknown frame>";
          }).join("\n") :
        "<no stack trace>";
      return toReprJS(this.exn, "tostring") + "\n" + stackStr;
    };

    /**
      Raises a PyretFailException with the given string
      @param {!string} str
      @return {!PyretFailException}
    */
    function makeMessageException(str) {
       return new PyretFailException(makeString(str));
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
          checkIf(str, isString);
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
        checkIf(base, isString);
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
      else { throw makeMessageException("Cannot unwrap yet: " + v); }
    }

    function wrap(v) {
      if(jsnums.isSchemeNumber(v)) { return makeNumberBig(v); }
      else if(typeof v === "number") { return makeNumber(v); }
      else if(typeof v === "string") { return makeString(v); }
      else if(typeof v === "boolean") { return makeBoolean(v); }
      else if(isOpaque(v)) { return v; }
      else if(isObject(v)) { return v; }
      else { throw makeMessageException("Cannot unwrap yet: " + v); }
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
    function makeFailureResult(e) { return new FailureResult(e); }

    /**
      Represents a continuation
      @constructor
    */
    function Cont(stack, bottom) {
      this.stack = stack;
      this.bottom = bottom;
    }
    function makeCont(bottom) { return new Cont([], bottom); }
    function isCont(v) { return v instanceof Cont; }

    function Pause(stack, resumer) {
      this.stack = stack;
      this.resumer = resumer;
    }
    function makePause(resumer) { return new Pause([], resumer); }
    function isPause(v) { return v instanceof Pause; }
    Pause.prototype = Object.create(Cont.prototype);

    function safeCall(fun, after, stackFrame) {
      var result;
      try {
        if (thisRuntime.GAS-- > 0) {
          result = fun();
        }
        else {
          thisRuntime.EXN_STACKHEIGHT = 0;
          throw thisRuntime.makeCont({
              go: function(ignored) {
                return fun();
              }
            });
        }
      }
      catch(e) {
        if (isCont(e)) {
          e.stack[thisRuntime.EXN_STACKHEIGHT++] = {
              go: function(retval) {
                return after(retval);
              },
              from: stackFrame
            };
          throw e;
        }
        else if (isPyretException(e)) {
          e.pyretStack.push(stackFrame);
          throw e;
        }
        else {
          throw e;
        }
      }
      return after(result);
    }

    // Call like: topSrc(new Error())
    function topSrc(error) {
      var stackFrame = error.stack.split("\n")[1].trim().split(":");
      return {
        src: stackFrame[0].split(" ")[1],
        "start-line": stackFrame[1],
        "start-column": stackFrame[2],
        "end-line": stackFrame[1],
        "end-column": stackFrame[2]
      };
    }

    /**@type {function(function(Object, Object) : !PBase, Object, function(Object))}*/
    function run(program, namespace, options, onDone) {
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
      startTimer();
      var that = this;
      var theOneTrueStackTop = topSrc(new Error());
      var kickoff = {
          go: function(ignored) {
            return program(thisRuntime, namespace);
          },
          from: theOneTrueStackTop
        };
      var theOneTrueStack = [kickoff];
      var theOneTrueStart = {};
      var val = theOneTrueStart;
      var theOneTrueStackHeight = 1;
      var BOUNCES = 0;
      var TOS = 0;

      var sync = options.sync || false;
      var initialGas = options.initialGas || INITIAL_GAS;

      // iter :: () -> Undefined
      // This function should not return anything meaningful, as state
      // and fallthrough are carefully managed.
      function iter() {
        var loop = true;
        while (loop) {
          loop = false;
          try {
            if (manualPause !== null) {
              var thePause = manualPause;
              manualPause = null;
              pauseStack(function(restarter) {
                  return thePause({
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
                setTimeout(iter, 0);
                return;
              }
              var next = theOneTrueStack[--theOneTrueStackHeight];
              theOneTrueStack[theOneTrueStackHeight] = undefined;
              val = next.go(val);
            }
          } catch(e) {
            if(isCont(e)) {
              BOUNCES++;
              thisRuntime.GAS = initialGas;
              for(var i = e.stack.length - 1; i >= 0; i--) {
                theOneTrueStack[theOneTrueStackHeight++] = e.stack[i];
              }

              if(isPause(e)) {
                (function(hasBeenResumed) {
                  function checkResume() {
                    if(hasBeenResumed) {
                      throw Error("This stack has already been resumed or broken ", theOneTrueStack);
                    }
                    hasBeenResumed = true;
                  }
                  e.resumer({
                    resume: function(restartVal) {
                      checkResume();
                      val = restartVal;
                      TOS++;
                      setTimeout(iter, 0);
                    },
                    break: function() {
                      checkResume();
                      onDone(new FailureResult(makeMessageException("User break"), { bounces: BOUNCES, tos: TOS, time: endTimer() }));
                    },
                    error: function(err) {
                      checkResume();
                      onDone(new FailureResult(err, { bounces: BOUNCES, tos: TOS, time: endTimer() }));
                    }
                  });
                })(false);
                return;
              }
              else if(isCont(e)) {
                theOneTrueStack[theOneTrueStackHeight++] = e.bottom;
                val = theOneTrueStart;
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
              onDone(new FailureResult(e, { bounces: BOUNCES, tos: TOS, time: endTimer() }));
              return;
            } else {
              onDone(new FailureResult(e, { bounces: BOUNCES, tos: TOS, time: endTimer() }));
              return;
            }
          }
        }
        onDone(new SuccessResult(val, { bounces: BOUNCES, tos: TOS, time: endTimer() }));
        return;
      }
      thisRuntime.GAS = initialGas;
      iter();
    }

    function pauseStack(resumer) {
      thisRuntime.EXN_STACKHEIGHT = 0;
      throw makePause(resumer);
    }

    var manualPause = null;
    function schedulePause(resumer) {
      manualPause = resumer;
    }

    function execThunk(thunk) {
      function wrapResult(res) {
        var ffi = require("./ffi-helpers")(thisRuntime, thisRuntime.namespace);
        if(isSuccessResult(res)) {
          return ffi.makeLeft(res.result);
        } else if (isFailureResult(res)) {
          return ffi.makeRight(res.exn.exn);
        } else {
          console.error("Bad execThunk result: ", res);
          return;
        }
      }
      thisRuntime.pauseStack(function(restarter) {
        thisRuntime.run(function(_, __) {
            return thunk.app();
          }, thisRuntime.namespace, {
            sync: true
          }, function(result) { restarter.resume(wrapResult(result)) });
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
        thisRuntime.checkIf(r, thisRuntime.isNumber);
        return thisRuntime.makeNumberBig(jsnums.add(l, r));
      } else if (thisRuntime.isString(l)) {
        thisRuntime.checkIf(r, thisRuntime.isString);
        return thisRuntime.makeString(l.concat(r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_plus")) {
        try {
          return thisRuntime.getField(l, "_plus").app(r);
        } catch(e) {
          if (thisRuntime.isCont(e)) {
            var stacklet = {
              from: topSrc(new Error()),
              go: function(ret) {
                return ret;
              }
            }
            e.stack[thisRuntime.EXN_STACKHEIGHT++] = stacklet;
            throw e;
          } else {
            if (thisRuntime.isPyretException(e)) {
              e.pyretStack.push(topSrc(new Error()));
            }
            throw e;
          }
        }
      } else {
        throw makeMessageException("First argument to _plus was not a number, string, or did not have a _plus method: " + JSON.stringify(l));
      }
    };

    var minus = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkIf(r, thisRuntime.isNumber);
        return thisRuntime.makeNumberBig(jsnums.subtract(l, r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_minus")) {
        try {
          return thisRuntime.getField(l, "_minus").app(r);
        } catch(e) {
          if (thisRuntime.isCont(e)) {
            var stacklet = {
              from: topSrc(new Error()),
              go: function(ret) {
                return ret;
              }
            }
            e.stack[thisRuntime.EXN_STACKHEIGHT++] = stacklet;
            throw e;
          } else {
            if (thisRuntime.isPyretException(e)) {
              e.pyretStack.push(topSrc(new Error()));
            }
            throw e;
          }
        }
      } else {
        throw makeMessageException("First argument to _minus was not a number, or did not have a _minus method: " + JSON.stringify(l));
      }
    };

    var times = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkIf(r, thisRuntime.isNumber);
        return thisRuntime.makeNumberBig(jsnums.multiply(l, r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_times")) {
        try {
          return thisRuntime.getField(l, "_times").app(r);
        } catch(e) {
          if (thisRuntime.isCont(e)) {
            var stacklet = {
              from: topSrc(new Error()),
              go: function(ret) {
                return ret;
              }
            }
            e.stack[thisRuntime.EXN_STACKHEIGHT++] = stacklet;
            throw e;
          } else {
            if (thisRuntime.isPyretException(e)) {
              e.pyretStack.push(topSrc(new Error()));
            }
            throw e;
          }
        }
      } else {
        throw makeMessageException("First argument to _times was not a number, or did not have a _times method: " + JSON.stringify(l));
      }
    };

    var divide = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkIf(r, thisRuntime.isNumber);
        if (jsnums.equals(0, r)) {
          throw makeMessageException("Division by zero");
        }
        return thisRuntime.makeNumberBig(jsnums.divide(l, r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_divide")) {
        try {
          return thisRuntime.getField(l, "_divide").app(r);
        } catch(e) {
          if (thisRuntime.isCont(e)) {
            var stacklet = {
              from: topSrc(new Error()),
              go: function(ret) {
                return ret;
              }
            }
            e.stack[thisRuntime.EXN_STACKHEIGHT++] = stacklet;
            throw e;
          } else {
            if (thisRuntime.isPyretException(e)) {
              e.pyretStack.push(topSrc(new Error()));
            }
            throw e;
          }
        }
      } else {
        throw makeMessageException("First argument to _divide was not a number, or did not have a _divide method: " + JSON.stringify(l));
      }
    };

    var lessthan = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkIf(r, thisRuntime.isNumber);
        return thisRuntime.makeBoolean(jsnums.lessThan(l, r));
      } else if (thisRuntime.isString(l)) {
        thisRuntime.checkIf(r, thisRuntime.isString);
        return thisRuntime.makeBoolean(l < r);
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_lessthan")) {
        try {
          return thisRuntime.getField(l, "_lessthan").app(r);
        } catch(e) {
          if (thisRuntime.isCont(e)) {
            var stacklet = {
              from: topSrc(new Error()),
              go: function(ret) {
                return ret;
              }
            }
            e.stack[thisRuntime.EXN_STACKHEIGHT++] = stacklet;
            throw e;
          } else {
            if (thisRuntime.isPyretException(e)) {
              e.pyretStack.push(topSrc(new Error()));
            }
            throw e;
          }
        }
      } else {
        throw makeMessageException("First argument to _lessthan was not a number, or did not have a _lessthan method: " + JSON.stringify(l));
      }
    };

    var greaterthan = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkIf(r, thisRuntime.isNumber);
        return thisRuntime.makeBoolean(jsnums.greaterThan(l, r));
      } else if (thisRuntime.isString(l)) {
        thisRuntime.checkIf(r, thisRuntime.isString);
        return thisRuntime.makeBoolean(l > r);
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_greaterthan")) {
        try {
          return thisRuntime.getField(l, "_greaterthan").app(r);
        } catch(e) {
          if (thisRuntime.isCont(e)) {
            var stacklet = {
              from: topSrc(new Error()),
              go: function(ret) {
                return ret;
              }
            }
            e.stack[thisRuntime.EXN_STACKHEIGHT++] = stacklet;
            throw e;
          } else {
            if (thisRuntime.isPyretException(e)) {
              e.pyretStack.push(topSrc(new Error()));
            }
            throw e;
          }
        }
      } else {
        throw makeMessageException("First argument to _greaterthan was not a number, or did not have a _greaterthan method: " + JSON.stringify(l));
      }
    };

    var lessequal = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkIf(r, thisRuntime.isNumber);
        return thisRuntime.makeBoolean(jsnums.lessThanOrEqual(l, r));
      } else if (thisRuntime.isString(l)) {
        thisRuntime.checkIf(r, thisRuntime.isString);
        return thisRuntime.makeBoolean(l <= r);
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_lessequal")) {
        try {
          return thisRuntime.getField(l, "_lessequal").app(r);
        } catch(e) {
          if (thisRuntime.isCont(e)) {
            var stacklet = {
              from: topSrc(new Error()),
              go: function(ret) {
                return ret;
              }
            }
            e.stack[thisRuntime.EXN_STACKHEIGHT++] = stacklet;
            throw e;
          } else {
            if (thisRuntime.isPyretException(e)) {
              e.pyretStack.push(topSrc(new Error()));
            }
            throw e;
          }
        }
      } else {
        throw makeMessageException("First argument to _lessequal was not a number, or did not have a _lessequal method: " + JSON.stringify(l));
      }
    };

    var greaterequal = function(l, r) {
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkIf(r, thisRuntime.isNumber);
        return thisRuntime.makeBoolean(jsnums.greaterThanOrEqual(l, r));
      } else if (thisRuntime.isString(l)) {
        thisRuntime.checkIf(r, thisRuntime.isString);
        return thisRuntime.makeBoolean(l >= r);
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_greaterequal")) {
        try {
          return thisRuntime.getField(l, "_greaterequal").app(r);
        } catch(e) {
          if (thisRuntime.isCont(e)) {
            var stacklet = {
              from: topSrc(new Error()),
              go: function(ret) {
                return ret;
              }
            }
            e.stack[thisRuntime.EXN_STACKHEIGHT++] = stacklet;
            throw e;
          } else {
            if (thisRuntime.isPyretException(e)) {
              e.pyretStack.push(topSrc(new Error()));
            }
            throw e;
          }
        }
      } else {
        throw makeMessageException("First argument to _greaterequal was not a number, or did not have a _greaterequal method: " + JSON.stringify(l));
      }
    };



    var string_substring = function(s, min, max) {
      thisRuntime.checkIf(s, thisRuntime.isString);
      thisRuntime.checkIf(min, thisRuntime.isNumber);
      thisRuntime.checkIf(max, thisRuntime.isNumber);
      return thisRuntime.makeString(s.substring(jsnums.toFixnum(min), jsnums.toFixnum(max)));
    }
    var string_replace = function(s, find, replace) {
      thisRuntime.checkIf(s, thisRuntime.isString);
      thisRuntime.checkIf(find, thisRuntime.isString);
      thisRuntime.checkIf(replace, thisRuntime.isString);
      return thisRuntime.makeString(s.replace(new RegExp(find,'g'), replace));
    }


    var string_append = function(l, r) {
      thisRuntime.checkIf(l, thisRuntime.isString);
      thisRuntime.checkIf(r, thisRuntime.isString);
      return thisRuntime.makeString(l.concat(r));
    }
    var string_contains = function(l, r) {
      thisRuntime.checkIf(l, thisRuntime.isString);
      thisRuntime.checkIf(r, thisRuntime.isString);
      return thisRuntime.makeBoolean(l.indexOf(r) !== -1);
    }
    var string_length = function(s) {
      thisRuntime.checkIf(s, thisRuntime.isString);
      return thisRuntime.makeNumber(s.length);
    }
    var string_tonumber = function(s) {
      thisRuntime.checkIf(s, thisRuntime.isString);
      var num = jsnums.fromString(s);
      if(num !== false) {
        return makeNumberBig(/**@type {Bignum}*/ (num));
      }
      else {
        return makeNothing();
      }
    }
    var string_repeat = function(s, n) {
      thisRuntime.checkIf(s, thisRuntime.isString);
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      var resultStr = "";
      // TODO(joe): loop up to a fixnum?
      for(var i = 0; i < jsnums.toFixnum(n); i++) {
        resultStr += s;
      }
      return makeString(resultStr);
    }
    var string_split = function(s, splitstr, repeated) {
      thisRuntime.checkIf(s, thisRuntime.isString);
      thisRuntime.checkIf(splitstr, thisRuntime.isString);
      
      var list = require("./ffi-helpers")(thisRuntime, thisRuntime.namespace);
      // TODO: Repeated?
      return list.makeList(s.split(splitstr).map(thisRuntime.makeString));
    }
    var string_charat = function(s, n) {
      thisRuntime.checkIf(s, thisRuntime.isString);
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      
      //TODO: Handle bignums that are beyond javascript
      return thisRuntime.makeString(String(s.charAt(jsnums.toFixnum(n))));
    }
    var string_toupper = function(s) {
      thisRuntime.checkIf(s, thisRuntime.isString);
      return thisRuntime.makeString(s.toUpperCase());
    }
    var string_tolower = function(s) {
      thisRuntime.checkIf(s, thisRuntime.isString);
      return thisRuntime.makeString(s.toLowerCase());
    }
    var string_explode = function(s) {
      thisRuntime.checkIf(s, thisRuntime.isString);
      var list = require("./ffi-helpers")(thisRuntime, thisRuntime.namespace);
      return list.makeList(s.split("").map(thisRuntime.makeString));
    }
    var string_indexOf = function(s, find) {
      thisRuntime.checkIf(s, thisRuntime.isString);
      thisRuntime.checkIf(find, thisRuntime.isString);
      return thisRuntime.makeNumberBig(s.indexOf(find));
    }


    var num_max = function(l, r) {
      thisRuntime.checkIf(l, thisRuntime.isNumber);
      thisRuntime.checkIf(r, thisRuntime.isNumber);
      if (jsnums.greaterThanOrEqual(l, r)) { return l; } else { return r; }
    }

    var num_min = function(l, r) {
      thisRuntime.checkIf(l, thisRuntime.isNumber);
      thisRuntime.checkIf(r, thisRuntime.isNumber);
      if (jsnums.lessThanOrEqual(l, r)) { return l; } else { return r; }
    }

    var num_abs = function(n) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.abs(n));
    }
      
    var num_sin = function(n) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.sin(n));
    }
    var num_cos = function(n) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.cos(n));
    }
    var num_tan = function(n) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.tan(n));
    }
    var num_asin = function(n) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.asin(n));
    }
    var num_acos = function(n) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.acos(n));
    }
    var num_atan = function(n) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.atan(n));
    }

    var num_modulo = function(n, mod) { 
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      thisRuntime.checkIf(mod, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.modulo(n, mod));
    }
    var num_truncate = function(n) { 
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      if (isNaN(n)) {
        return n;
      } else if (jsnums.greaterThanOrEqual(n, 0)) {
        return thisRuntime.makeNumberBig(jsnums.floor(n));
      } else {
        return thisRuntime.makeNumberBig(jsnums.floor(n));
      }
    }
    var num_sqrt = function(n) { 
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.sqrt(n));
    }
    var num_ceiling = function(n) { 
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.ceiling(n));
    }
    var num_floor = function(n) { 
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.floor(n));
    }
    var num_log = function(n) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.log(n));
    }
    var num_exp = function(n) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.exp(n));
    }
    var num_exact = function(n) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.toExact(n));
    }
    var num_is_integer = function(n) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      return thisRuntime.makeBoolean(jsnums.isInteger(n))
    }
    var num_expt = function(n, pow) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      thisRuntime.checkIf(pow, thisRuntime.isNumber);
      return thisRuntime.makeNumberBig(jsnums.exp(n, pow));
    }
    var num_tostring = function(n, digits) {
      thisRuntime.checkIf(n, thisRuntime.isNumber);
      thisRuntime.checkIf(digits, thisRuntime.isNumber);
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

          'run-task': makeFunction(execThunk),

          'gensym': gensym,

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
          'num-ceiling': makeFunction(num_ceiling),
          'num-floor': makeFunction(num_floor),
          'num-log': makeFunction(num_log),
          'num-exp': makeFunction(num_exp),
          'num-exact': makeFunction(num_exact),
          'num-is-integer': makeFunction(num_is_integer),
          'num-expt': makeFunction(num_expt),
          'num-tostring': makeFunction(num_tostring),

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
          'string-index-of': makeFunction(string_indexOf)

        }),
        'run': run,
        'safeCall': safeCall,

        'GAS': INITIAL_GAS,

        'makeCont'    : makeCont,
        'isCont'      : isCont,
        'makePause'   : makePause,
        'isPause'     : isPause,

        'pauseStack'  : pauseStack,
        'schedulePause'  : schedulePause,

        'getField'    : getField,
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

        'equiv': sameJSPy,
        'raise': raiseJSJS,

        'hasField' : hasField.app,

        'toReprJS' : toReprJS,

        'same' : same,
        'wrap' : wrap,
        'unwrap' : unwrap,

        'checkIf'      : checkIf,
        'confirm'      : confirm,
        'makeMessageException'      : makeMessageException,
        'serial' : Math.random(),
        'log': log,

        'modules' : Object.create(null),
        'getParam' : getParam,
        'setParam' : setParam
    };

    
    thisRuntime['pyretTrue'] = pyretTrue;
    thisRuntime['pyretFalse'] = pyretFalse;

    var list = getField(require("trove/list")(thisRuntime, thisRuntime.namespace), "provide");
    var ns = thisRuntime.namespace;
    var nsWithList = ns.set("_link", getField(list, "link"))
                       .set("_empty", getField(list, "empty"));
    thisRuntime.namespace = nsWithList;

    return thisRuntime;
}

return  {'makeRuntime' : makeRuntime};

       });
