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
      if(hasProperty(this.dict, field)) {
        allNewFields = false;
        if(isRef(this.dict[field])) {
          ffi.throwMessageException("Cannot update ref field " + field);
        }
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
function getFieldLocInternal(val, field, loc, isBang) {
    if(val === undefined) { 
      if (ffi === undefined) {
        throw ("FFI is not yet defined, and lookup of field " + field + " on undefined failed at location " + JSON.stringify(loc));
      } else {
        ffi.throwInternalError("Field lookup on undefined ", ffi.makeList([field])); }
    }
    if(!isObject(val)) { ffi.throwLookupNonObject(makeSrcloc(loc), val, field); }
    var fieldVal = val.dict[field];
    if(fieldVal === undefined) {
      if (ffi === undefined) {
        throw ("FFI is not yet defined, and lookup of field " + field + " on " + toReprJS(val, "_torepr") + " failed at location " + JSON.stringify(loc));
      } else {
        throw ffi.throwFieldNotFound(makeSrcloc(loc), val, field);
      }
    }
    else if(isRef(fieldVal)){
      if(!isBang) {
        return fieldVal;
        // NOTE(joe Aug 8 2014): This is a design decision whether we
        // want this to be an error or not
        // ffi.throwMessageException("Got ref in dot lookup");
      }
      return getRef(fieldVal);
    }
    else if(isMethod(fieldVal)){
      var curried = fieldVal['meth'](val);
      return makeFunctionArity(curried, fieldVal.arity - 1);
    }
    else {
      return fieldVal;
    }
}

function getFieldLoc(obj, field, loc) {
  return getFieldLocInternal(obj, field, loc, false);
}

function getFieldRef(obj, field, loc) {
  return getFieldLocInternal(obj, field, loc, true);
}

function getField(obj, field) {
  return thisRuntime.getFieldLoc(obj, field, ["runtime"]);
}

function extendObj(loc, val, extension) {
  if (!isObject(val)) { ffi.throwExtendNonObject(makeSrcloc(loc), val); }
  return val.extendWith(extension);
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

    var GRAPHABLE = 0;
    var UNGRAPHABLE = 1;
    var SET = 2;
    var FROZEN = 3;
    function PRef() {
      this.state = GRAPHABLE;
      this.anns = makePAnnList([]);
      this.value = undefined;
    }

    function makeGraphableRef() {
      return new PRef();
    }
    function makeRef(ann, loc) {
      var loc = typeof loc === "undefined" ? ["references"] : loc;
      var r = new PRef();
      addRefAnn(r, ann, loc);
      r.state = UNGRAPHABLE;
      return r;
    }
    function makeUnsafeSetRef(ann, value, loc) {
      var r = new PRef();
      r.state = SET;
      r.anns = makePAnnList([{ann: ann, loc: loc}]);
      r.value = value;
      return r;
    }
    function isRef(val) {
      return val instanceof PRef;
    }
    function isGraphableRef(ref) {
      return isRef(ref) && isRefGraphable(ref);
    }
    function isRefGraphable(ref) {
      return ref.state === GRAPHABLE;
    }
    function isRefSet(ref) {
      return ref.state >= SET;
    }
    function isRefFrozen(ref) {
      return ref.state >= FROZEN;
    }
    function getRefAnns(ref) {
      return ref.anns;
    }
    function refEndGraph(ref) {
      if(ref.state >= UNGRAPHABLE) {
        ffi.throwMessageException("Attempted to end graphing of already-done with graph ref");
      }
      ref.state = UNGRAPHABLE;
      return ref;
    }
    function addRefAnn(ref, ann, loc) {
      if(ref.state > UNGRAPHABLE) {
        ffi.throwMessageException("Attempted to annotate already-set ref");
      }
      ref.anns.addAnn(ann, loc);
      return ref;
    }
    function addRefAnns(ref, anns, locs) {
      if(ref.state > UNGRAPHABLE) {
        ffi.throwMessageException("Attempted to annotate already-set ref");
      }
      for(var i = 0; i < anns.length; i++) {
        ref.anns.addAnn(anns[i], locs[i]);
      }
      return ref;
    }
    function freezeRef(ref) {
      if(ref.state >= SET) {
        ref.state = FROZEN;
        return ref;
      }
      ffi.throwMessageException("Attempted to freeze an unset ref");
    }
    function unsafeSetRef(ref, value) {
      if(ref.state === UNGRAPHABLE || ref.state === SET) {
        ref.value = value;
        ref.state = SET;
        return ref;
      }
      ffi.throwMessageException("Attempted to set an unsettable ref");
    }
    /* Not stack-safe */
    function setRef(ref, value) {
      if(ref.state === UNGRAPHABLE || ref.state === SET) {
        return checkAnn(["references"], ref.anns, value, function(_) {
          ref.value = value; 
          ref.state = SET;
          return ref;
        });
      }
      ffi.throwMessageException("Attempted to set an unsettable ref");
    }
    function getRef(ref) {
      if(ref.state >= SET) { return ref.value; }
      ffi.throwMessageException("Attempt to get an unset ref");
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

    function makeMatch(name, arity) {
      if(arity === -1) {
        return makeMethod(function(self) {
          return function(handlers, els) {
            if(hasField(handlers, name)) {
              return getField(handlers, name).app();
            }
            else {
              return els.app(self);
            }
          };
        }, { length: 3 });
      }
      else {
        return makeMethod(function(self) {
          return function(handlers, els) {
            if(hasField(handlers, name)) {
              return getField(handlers, name).app.apply(null, self.$app_fields(function() { return arguments; }));
            }
            else {
              return els.app(self);
            }
          };
        }, { length: 3 });
      }
    }

    function makeDataValue(dict, brands, $name, $app_fields, $app_fields_raw, $arity, $mut_fields_mask) {
      var ret = new PObject(dict, brands);
      ret.$name = $name;
      ret.$app_fields = $app_fields;
      ret.$app_fields_raw = $app_fields_raw;
      ret.$mut_fields_mask = $mut_fields_mask;
      ret.$arity = $arity;
      return ret;
    }


    function isDataValue(v) {
      return hasProperty(v, "$name") && hasProperty(v, "$app_fields") && hasProperty(v, "$arity");
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
               isRef(val) ||
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
        thisRuntime.checkArity(1, arguments, "runtime");
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


    var NumberC = makePrimitiveAnn("Number", isNumber);
    var StringC = makePrimitiveAnn("String", isString);
    var BooleanC = makePrimitiveAnn("Boolean", isBoolean);
    var RawArrayC = makePrimitiveAnn("RawArray", isArray);
    var FunctionC = makePrimitiveAnn("Function",
      function(v) { return isFunction(v) || isMethod(v) });
    var MethodC = makePrimitiveAnn("Method", isMethod);
    var NothingC = makePrimitiveAnn("Nothing", isNothing);
    var ObjectC = makePrimitiveAnn("Object", isObject);
    var AnyC = makePrimitiveAnn("Any", function() { return true; });

    function confirm(val, test) {
      thisRuntime.checkArity(2, arguments, "runtime");
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
    function mkBrandName(name) {
      if(typeof name === "undefined") { name = ""; }
      var thisBrandStr = "$brand" + name + String(++brandCounter);
      return thisBrandStr;
    }
    var namedBrander = function(name) {
      var thisBrandStr = mkBrandName(name);
      var brander = makeObject({
          'test': makeFunction(function(obj) {
              thisRuntime.checkArity(1, arguments, "brander-test");
              return makeBoolean(hasBrand(obj, thisBrandStr));
            }),
          'brand': makeFunction(function(obj) {
              thisRuntime.checkArity(1, arguments, "brander-brand");
              return obj.brand(thisBrandStr);
            })
        });
      brander._brand = thisBrandStr;
      return brander;
    }
    /**@type {PFunction} */
    var brander = makeFunction(
    /**
      @return {!PBase}
    */
    function() {
      thisRuntime.checkArity(0, arguments, "brander");
      return namedBrander("brander");
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
      var stack = [];
      var stackOfStacks = [];
      var seen = [];
      var needsGraph = false;
      var seenFrozenRef = false;
      var seenUnfrozenRef = false;
      var gensymCount = 1;
      function makeName() {
        return "cyc_" + (gensymCount++) + "_";
      }
      function findSeen(obj) {
        for (var i = 0; i < seen.length; i++) {
          if (seen[i].obj === obj) {
            seen[i].count++;
            // console.log("Incrementing count for " + seen[i].asName + " => " + seen[i].count);
            needsGraph = true;
            seenFrozenRef = seenFrozenRef || isRefFrozen(obj);
            seenUnfrozenRef = seenUnfrozenRef || !isRefFrozen(obj);
            return seen[i].asName;
          }
        }
        return undefined;
      }
      function addNewRef(obj) {
        obj.seenIt = true;
        var newObj = {count: 1, asName: makeName(), asDoc: "", obj: obj};
        // console.log("Initializing count for " + newObj.asName + " => 1");
        seen.push(newObj);
        return newObj.asName;
      }
      function setRefDoc(obj, doc) {
        for (var i = 0; i < seen.length; i++) {
          if (seen[i].obj === obj) {
            seen[i].asDoc = doc;
            return seen[i].asName;
          }
        }
      }
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
                var s = getField(next, method).app(toReprFunPy); // NOTE: Passing in the function below!
                // the continuation stacklet will get the result value, and do the next two steps manually
                top.todo.pop();
                top.done.push(thisRuntime.unwrap(s));
              } else if(isDataValue(next)) {
                var vals = next.$app_fields_raw(function(/* varargs */) {
                  return Array.prototype.slice.call(arguments); // args are processed in reverse order...
                });
                stack.push({todo: vals, done: [], arity: next.$arity, 
                            implicitRefs: next.$mut_fields_mask, constructor: next.$name});
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
            } else if (isRef(next)) {
              var found = findSeen(next);
              var implicitRef = hasProperty(top, "implicitRefs") && top.implicitRefs[top.todo.length - 1];
              if (found) {
                top.todo.pop();
                if (implicitRef || isRefFrozen(next)) {
                  top.done.push(found);
                } else {
                  top.done.push("make-ref(" + found + ")");
                }
              } else {
                var newName = addNewRef(next);
                // Constructors implicitly wrap their mutable args in a reference if necessary
                // So the constructor case above will use implicitRefs to indicate where 
                // the refs were made implicitly, so they aren't printed.
                stack.push({todo: [getRef(next)], done: [], theRef: next, wrapRef: !implicitRef});
              }
            } else if (typeof next === "object") {
              top.todo.pop();
              top.done.push(JSON.stringify(next, null, "  "));
            } else {
              top.todo.pop();
              top.done.push(String(next));
            }
          } else { // All fields of a nested object or data value have been stringified; collapse
            stack.pop();
            var prev = stack[stack.length - 1];
            prev.todo.pop();
            var s = "";
            if(hasProperty(top, "keys")) {
              s += "{";
              for (var i = 0; i < top.keys.length; i++) {
                if (i > 0) { s += ", "; }
                s += top.keys[i] + ": " + top.done[i];
              }
              s += "}";
            } else if (hasProperty(top, "wrapRef")) {
              var refName = setRefDoc(top.theRef, top.done[0]);
              if (top.wrapRef && !isRefFrozen(top.theRef)) {
                s += "make-ref(" + refName + ")";
              } else {
                s += refName;
              }
            } else if(hasProperty(top, "constructor")) {
              s += top.constructor;
              // Sentinel value for singleton constructors
              if(top.arity !== -1) {
                // console.log("Constructing " + top.constructor + ", implicitRefs = " + top.implicitRefs);
                s += "(";
                for(var i = top.done.length - 1; i >= 0; i--) {
                  if(i < top.done.length - 1) { s += ", "; }
                  // console.log("  Field #" + i + ": implicitRef? " +
                  //             top.implicitRefs[i] + ", and field: " + top.done[i]);
                  s += top.done[i];
                }
                s += ")";
              }
            }
            prev.done.push(s);
          }
        }
        var finalAns = stack[0].done[0];
        var needsMutableGraph = false;
        if (needsGraph) {
          // console.log("FinalAns currently: " + finalAns);
          for (var i = 0; i < seen.length; i++) {
            if (seen[i].count > 1 && !isRefFrozen(seen[i].obj)) {
              needsMutableGraph = true;
              break;
            }
          }
          if (needsMutableGraph) {
            finalAns = "block:\n  m-graph:\n";
          } else {
            finalAns = "block:\n  graph:\n";
          }
          for (var i = 0; i < seen.length; i++) {
            if (seen[i].count > 1 || isRefFrozen(seen[i].obj)) {
              // console.log("Including " + seen[i].asName + " => " + seen[i].asDoc + " because "
              //             + "count? " + (seen[i].count) + ", frozen ref? " + isRefFrozen(seen[i].obj));
              finalAns += "  " + seen[i].asName + " = " + seen[i].asDoc + "\n";
            // } else {
            //   console.log("Skipping  " + seen[i].asName + " => " + seen[i].asDoc + " because "
            //               + "count? " + (seen[i].count) + ", frozen ref? " + isRefFrozen(seen[i].obj));
            }
          }
          finalAns += "  end\n"; 
          if (needsMutableGraph) {
            for (var i = 0; i < seen.length; i++) {
              if (isRefFrozen(seen[i].obj)) {
                finalAns += "  freeze-ref(" + seen[i].asName + ")\n";
              }
            }
          }
          finalAns += "  " + stack[0].done[0] + "\nend";
        }
        var replacementsNeeded = true;
        while (replacementsNeeded) {
          replacementsNeeded = false;
          for (var i = 0; i < seen.length; i++) {
            if (seen[i].count === 1 && !isRefFrozen(seen[i].obj)) {
              var replaced = finalAns.replace(new RegExp(seen[i].asName + "(?! =)", "g"), seen[i].asDoc);
              if (replaced !== finalAns) { 
                replacementsNeeded = true; 
              }
              finalAns = replaced;
            }
          }
        }
        return finalAns; 
      }
      function toReprFun($ar) {
        var $step = 0;
        var $ans = undefined;
        try {
          if (thisRuntime.isActivationRecord($ar)) {
            $step = $ar.step;
            $ans = $ar.ans;
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
              [],
              []);
          }
          if (thisRuntime.isPyretException($e)) {
            $e.pyretStack.push(["runtime torepr"]);
          }
          throw $e;
        }
      }
      function reenterToReprFun(val) {
        // arity check
        var $step = 0;
        var $ans = undefined;
        try {
          if (thisRuntime.isActivationRecord(val)) {
            $step = val.step;
            $ans = val.ans;
          }
          while(true) {
            switch($step) {
            case 0:
              stackOfStacks.push(stack);
              stack = [{todo: [val], done: [], implicitRefs: [true]}];
              $step = 1;
              $ans = toReprFun();
              break;
            case 1:
              stack = stackOfStacks.pop();
              return $ans;
            }
          }
        } catch($e) {
          if (thisRuntime.isCont($e)) {
            $e.stack[thisRuntime.EXN_STACKHEIGHT++] = thisRuntime.makeActivationRecord(
              ["runtime torepr (reentrant)"],
              reenterToReprFun,
              $step,
              [],
              []);
          }
          if (thisRuntime.isPyretException($e)) {
            $e.pyretStack.push(["runtime torepr"]);
          }
          throw $e;
        }
      }
      var toReprFunPy = makeFunction(reenterToReprFun);
      return reenterToReprFun(val);
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
    var torepr = makeFunction(function(val) {
      thisRuntime.checkArity(1, arguments, "torepr");
      return makeString(toReprJS(val, "_torepr"));
    });
    var tostring = makeFunction(function(val) {
        thisRuntime.checkArity(1, arguments, "tostring");
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
        thisRuntime.checkArity(1, arguments, "print");
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
        thisRuntime.checkArity(1, arguments, "display");
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
        thisRuntime.checkArity(1, arguments, "print-error");
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
        thisRuntime.checkArity(1, arguments, "display-error");
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
            return s && hasField(s, "source") ? g(s, "source") +
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
        if (srcloc === undefined) {
          return makeString(JSON.stringify(arr));
        } else {
          return getField(srcloc, "builtin").app(arr[0])
        }
      }
      else if (typeof arr === "object" && arr.length === 7) {
        return getField(srcloc, "srcloc").app(
            arr[0], arr[1], arr[2], arr[3], arr[4], arr[5], arr[6]
          );
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
        thisRuntime.checkArity(1, arguments, "raise");
        throw new PyretFailException(val); 
      };
    /** type {!PFunction} */
    var raisePyPy = makeFunction(raiseJSJS);

    /** type {!PFunction} */
    var hasField =
        /**
          Checks if an object has a given field
          @param {!PBase} obj The object to test
          @param {!PBase} str The field to test for, signals error if non-string
          @return {!PBase} 
        */
        function(obj, str) {
          thisRuntime.checkArity(2, arguments, "has-field");
          checkString(str);
          return makeBoolean(hasProperty(obj.dict, str));
        };

    function sameBrands(brands1, brands2) {
      if (brands1.brandCount !== brands2.brandCount) { return false; }
      for(var i in brands1) {
        if(brands1[i] !== brands2[i]) { return false; }
      }
      return true;
    }


    function combineEquality(e1, e2) {
      if (ffi.isEqual(e1)) { return e2; }
      else if (ffi.isNotEqual(e1)) { return e1; }
      else if (ffi.isNotEqual(e2)) { return e2; }
      else return e1;
    }
    // JS function from Pyret values to Pyret equality answers
    function equal3(left, right, alwaysFlag) {
      var isIdentical = identical3(left, right);
      if (!ffi.isNotEqual(isIdentical)) { return isIdentical; } // if Equal or Unknown...
      
      var stackOfToCompare = [];
      var toCompare = [];
      var current, curLeft, curRight, curAns, cache;
      curAns = ffi.equal;
      cache = {left: [], right: []};
      function findPair(obj1, obj2) {
        for (var i = 0; i < cache.length; i++) {
          if (cache.left[i] === obj1 && cache.right[i] === obj2)
            return true;
        }
        return false;
      }
      function cachePair(obj1, obj2) {
        cache.left.push(obj1);
        cache.right.push(obj2);
      }
      function equalHelp() {
        while (toCompare.length > 0 && !ffi.isNotEqual(curAns)) {
          current = toCompare.pop();
          curLeft = current.left;
          curRight = current.right;
          
          if (ffi.isEqual(identical3(curLeft, curRight))) {
            continue;
          } else if (isNumber(curLeft) && isNumber(curRight)) {
            if (jsnums.equals(curLeft, curRight)) {
              continue;
            } else {
              curAns = ffi.notEqual.app(current.path);
            }
          } else if (isFunction(curLeft) && isFunction(curRight)) {
            curAns = ffi.unknown;
          } else if (isMethod(curLeft) && isMethod(curRight)) {
            curAns = ffi.unknown;
          } else if (isOpaque(curLeft) && isOpaque(curRight)) {
            if (curLeft.equals(curLeft.val, curRight.val)) {
              continue;
            } else {
              curAns = ffi.notEqual.app(current.path);
            }
          } else {
            if (findPair(curLeft, curRight)) {
              continue; // Already checked this pair of objects
            } else {
              cachePair(curLeft, curRight);
              if (isRef(curLeft) && isRef(curRight)) {
                if (alwaysFlag) { // In equal-always, non-identical refs are not equal
                  curAns = ffi.notEqual.app(current.path); // We would've caught identical refs already
                } else { // In equal-now, we walk through the refs
                  var newPath = current.path;
                  var lastDot = newPath.lastIndexOf(".");
                  if (lastDot > -1) {
                    newPath = newPath.substr(0, lastDot) + "!" + newPath.substr(lastDot + 1);
                  } else {
                    newPath = "deref(" + newPath + ")";
                  }
                  toCompare.push({
                    left: getRef(curLeft),
                    right: getRef(curRight),
                    path: newPath
                  });
                }
              } else if (isArray(curLeft) && isArray(curRight)) {
                if (curLeft.length !== curRight.length) {
                  curAns = ffi.notEqual.app(current.path);
                } else {
                  for (var i = 0; i < curLeft.length; i++) {
                    toCompare.push({
                      left: curLeft[i],
                      right: curRight[i],
                      path: "raw-array-get(" + current.path + ", " + i + ")"
                    });
                  }
                }
              } else if (isObject(curLeft) && isObject(curRight)) {
                if (curLeft.dict["_equals"]) {
                  // If this call fails,
                  var newAns = getField(curLeft, "_equals").app(curRight, equalFunPy);
                  // the continuation stacklet will get the result, and comine them manually
                  curAns = combineEquality(curAns, newAns);
                } else {
                  var dictLeft = curLeft.dict;
                  var dictRight = curRight.dict;
                  var fieldsLeft;
                  var fieldsRight;
                  // Fast case, for objects that get extended with similar patterns
                  // (e.g. variants of data have same proto), just check own props
                  if(getProto(dictLeft) === getProto(dictRight)) {
                    fieldsLeft = Object.keys(dictLeft);
                    fieldsRight = Object.keys(dictRight);
                    if(fieldsLeft.length !== fieldsRight.length) { 
                      curAns = ffi.notEqual.app(current.path); 
                    } else {
                      for(var k = 0; k < fieldsLeft.length; k++) {
                        toCompare.push({
                          left: curLeft.dict[fieldsLeft[k]],
                          right: curRight.dict[fieldsLeft[k]],
                          path: current.path + "." + fieldsLeft[k]
                        });
                      }
                    }
                  }
                  // Slower case, just iterate all fields, all the way down to the bottom
                  else {
                    fieldsLeft = getFields(curLeft);
                    fieldsRight = getFields(curRight);
                    if(fieldsLeft.length !== fieldsRight.length) { return false; }
                    for(var k = 0; k < fieldsLeft.length; k++) {
                      toCompare.push({
                        left: curLeft.dict[fieldsLeft[k]],
                        right: curRight.dict[fieldsLeft[k]],
                        path: current.path + "." + fieldsLeft[k]
                      });
                    }
                  }
                  if (!sameBrands(getBrands(curLeft), getBrands(curRight))) {
                    curAns = ffi.notEqual.app(current.path);
                  }
                }
              } else {
                curAns = ffi.notEqual.app(current.path);
              }
            }
          }
        }
        return curAns;
      }
      var stackFrameDesc = [alwaysFlag ? "runtime equal-always" : "runtime equal-now"];
      function equalFun($ar) {
        var $step = 0;
        var $ans = undefined;
        try {
          if (thisRuntime.isActivationRecord($ar)) {
            $step = $ar.step;
            $ans = $ar.ans;
          }
          while(true) {
            switch($step) {
            case 0:
              $step = 1;
              return equalHelp();
            case 1:
              curAns = combineEquality(curAns, $ans);
              $step = 0;
              break;
            }
          }
        } catch($e) {
          if (thisRuntime.isCont($e)) {
            $e.stack[thisRuntime.EXN_STACKHEIGHT++] = thisRuntime.makeActivationRecord(
              stackFrameDesc,
              equalFun,
              $step,
              [],
              []);
          }
          if (thisRuntime.isPyretException($e)) {
            $e.pyretStack.push(stackFrameDesc);
          }
          throw $e;
        }
      }
      function reenterEqualFun(left, right) {
        // arity check
        var $step = 0;
        var $ans = undefined;
        try {
          if (thisRuntime.isActivationRecord(left)) {
            $step = val.step;
            $ans = val.ans;
          }
          while(true) {
            switch($step) {
            case 0:
              stackOfToCompare.push(toCompare);
              toCompare = [{left: left, right: right, path: "the-value"}];
              $step = 1;
              $ans = equalFun();
              break;
            case 1:
              toCompare = stackOfToCompare.pop();
              return $ans;
            }
          }
        } catch($e) {
          if (thisRuntime.isCont($e)) {
            $e.stack[thisRuntime.EXN_STACKHEIGHT++] = thisRuntime.makeActivationRecord(
              stackFrameDesc,
              reenterEqualFun,
              $step,
              [],
              []);
          }
          if (thisRuntime.isPyretException($e)) {
            $e.pyretStack.push(stackFrameDesc);
          }
          throw $e;
        }
      }
      var equalFunPy = makeFunction(reenterEqualFun);
      return reenterEqualFun(left, right);
    }
        

    // JS function from Pyret values to Pyret equality answers
    function equalAlways3(left, right) {
      thisRuntime.checkArity(2, arguments, "equal-always3");
      return equal3(left, right, true);
    };
    // JS function from Pyret values to JS booleans (or throws)
    function equalAlways(v1, v2) {
      thisRuntime.checkArity(2, arguments, "equal-always");
      var ans = equal3(v1, v2, true);
      if (ffi.isEqual(ans)) { return true; }
      else if (ffi.isNotEqual(ans)) { return false; }
      else { ffi.throwMessageException("Attempted to compare functions or methods with equal-always"); }
    };
    // Pyret function from Pyret values to Pyret booleans (or throws)
    var equalAlwaysPy = makeFunction(function(left, right) {
        return makeBoolean(equalAlways(left, right));
    });
    // JS function from Pyret values to Pyret equality answers
    function equalNow3(left, right) {
      thisRuntime.checkArity(2, arguments, "equal-now3");
      return equal3(left, right, false);
    };
    // JS function from Pyret values to JS booleans (or throws)
    function equalNow(v1, v2) {
      thisRuntime.checkArity(2, arguments, "equal-now");
      var ans = equal3(v1, v2, false);
      if (ffi.isEqual(ans)) { return true; }
      else if (ffi.isNotEqual(ans)) { return false; }
      else { ffi.throwMessageException("Attempted to compare functions or methods with equal-now"); }
    };
    // Pyret function from Pyret values to Pyret booleans (or throws)
    var equalNowPy = makeFunction(function(left, right) {
        return makeBoolean(equalNow(left, right));
    });
  

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
    var samePyPy = makeFunction(function(v1, v2) { 
      thisRuntime.checkArity(2, arguments, "same");
      return makeBoolean(same(v1, v2)); 
    });
    // JS function from Pyret values to Pyret booleans
    var sameJSPy = function(v1, v2) { return makeBoolean(same(v1, v2)); };

    // JS function from Pyret values to Pyret equality answers
    function identical3(v1, v2) {
      if (isFunction(v1) && isFunction(v2)) {
        return ffi.unknown;
      } else if (isMethod(v1) && isMethod(v2)) {
        return ffi.unknown;
      } else if (v1 === v2) {
        return ffi.equal;
      } else {
        return ffi.notEqual.app("");
      }
    };
    // Pyret function from Pyret values to Pyret equality answers
    var identical3Py = makeFunction(function(v1, v2) {
      thisRuntime.checkArity(2, arguments, "identical3");
      return identical3(v1, v2);
    });
    // JS function from Pyret values to JS true/false or throws
    function identical(v1, v2) {
      thisRuntime.checkArity(2, arguments, "identical");
      var ans = identical3(v1, v2);
      if (ffi.isEqual(ans)) { return true; }
      else if (ffi.isNotEqual(ans)) { return false; }
      else { ffi.throwMessageException("Attempted to compare functions or methods with identical"); }
    };
    // Pyret function from Pyret values to Pyret booleans (or throws)
    var identicalPy = makeFunction(function(v1, v2) {
        return makeBoolean(identical(v1, v2));
    });

    var gensymCounter = Math.floor(Math.random() * 1000);
    var gensym = makeFunction(function(base) {
        thisRuntime.checkArity(1, arguments, "gensym");
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
        thisRuntime.checkArity(2, arguments, "run-checks");
        return nothing;
      }),
      "check-is": makeFunction(function(code, left, right, loc) {
        thisRuntime.checkArity(4, arguments, "check-is");
        return nothing;
      }),
      "check-satisfies": makeFunction(function(code, left, pred, loc) {
        thisRuntime.checkArity(4, arguments, "check-satisfies");
        return nothing;
      }),
      "results": makeFunction(function() {
        thisRuntime.checkArity(0, arguments, "results");
        return nothing;
      })
    });

    setParam("current-checker", nullChecker);

    /** type {!PBase} */
    var builtins = makeObject({
        'has-field': makeFunction(hasField),
        'equiv': samePyPy,
        'current-checker': makeFunction(function() {
          thisRuntime.checkArity(0, arguments, "current-checker");
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
      return makeFunction(function(v) { 
        return makeBoolean(jsPred(v)); 
      });
    }

    function returnOrRaise(result, val, after) {
      if(ffi.isOk(result)) { return after(val); }
      if(ffi.isFail(result)) { raiseJSJS(result); }
      throw "Internal error: got invalid result from annotation check";
    }

    function isCheapAnnotation(ann) {
      return !(ann.refinement || ann instanceof PRecordAnn);
    }

    function checkAnn(compilerLoc, ann, val, after) {
      if(isCheapAnnotation(ann)) {
        return returnOrRaise(ann.check(compilerLoc, val), val, after);
      }
      else {
        return safeCall(function() {
          return ann.check(compilerLoc, val);
        }, function(result) {
          return returnOrRaise(result, val, after);
        },
        "checkAnn");
      }
    }

    function _checkAnn(compilerLoc, ann, val) {
      if (isCheapAnnotation(ann)) {
        var result = ann.check(compilerLoc, val);
        if(ffi.isOk(result)) { return val; }
        if(ffi.isFail(result)) { raiseJSJS(result); }
        throw "Internal error: got invalid result from annotation check";
      } else {
        return safeCall(function() {
          return ann.check(compilerLoc, val);
        }, function(result) {
          if(ffi.isOk(result)) { return val; }
          if(ffi.isFail(result)) { raiseJSJS(result); }
          throw "Internal error: got invalid result from annotation check";
        },
        "_checkAnn");
      }
    }

    function safeCheckAnnArg(compilerLoc, ann, val, after) {
      if(isCheapAnnotation(ann)) {
        return returnOrRaise(ann.check(compilerLoc, val), val, after);
      }
      else {
        return safeCall(function() {
          return ann.check(compilerLoc, val);
        }, function(result) {
          return returnOrRaise(result, val, after);
        },
        "safeCheckAnnArg");
      }
    }

    function checkAnnArg(compilerLoc, ann, val) {
      return safeCall(function() {
        return ann.check(compilerLoc, val);
      }, function(result) {
        if(ffi.isOk(result)) { return val; }
        if(ffi.isFail(result)) {
          raiseJSJS(ffi.contractFailArg(getField(result, "loc"), getField(result, "reason")));
        }
        throw "Internal error: got invalid result from annotation check";
      },
      "checkAnnArg");
    }

    function _checkAnnArg(compilerLoc, ann, val) {
      var result = ann.check(compilerLoc, val);
      if(ffi.isOk(result)) { return val; }
      if(ffi.isFail(result)) {
        raiseJSJS(ffi.contractFailArg(getField(result, "loc"), getField(result, "reason")));
      }
      throw "Internal error: got invalid result from annotation check";
    }

    function checkAnnArgs(anns, args, locs, after) {
      function checkI(i) {
        if(i >= args.length) { return after(); }
        else {
          return safeCheckAnnArg(locs[i], anns[i], args[i], function(ignoredArg) {
            return checkI(i + 1);
          });
        }
      }
      return checkI(0);
    }
    function _checkAnnArgs(anns, args, locs) {
      function checkI(i) {
        if(i >= args.length) { return nothing; }
        else {
          return safeCheckAnnArg(locs[i], anns[i], args[i], function(ignoredArg) {
            return checkI(i + 1);
          });
        }
      }
      return checkI(0);
    }

    function checkConstructorArgs(anns, args, locs, after) {
      function checkI(i) {
        if(i >= args.length) { return after(); }
        else {
          if(isRefGraphable(args[i])) { return checkI(i + 1); }
          else {
            return safeCheckAnnArg(locs[i], anns[i], args[i], function(ignoredArg) {
              return checkI(i + 1);
            });
          }
        }
      }
      return checkI(0);
    }
    function checkRefAnns(obj, fields, vals, locs) {
      if (!isObject(obj)) { ffi.throwMessageException("Update non-object"); }
      var anns = new Array(fields.length);
      var refs = new Array(fields.length);
      var field = null;
      var ref = null;
      for(var i = 0; i < vals.length; i++) {
        field = fields[i];
        if(hasField(obj, field)) {
          ref = obj.dict[field];
          if(isRef(ref)) {
            if(isRefFrozen(ref)) {
              ffi.throwMessageException("Update of frozen ref " + field);
            }
            anns[i] = getRefAnns(ref);
            refs[i] = ref;
          }
          else {
            ffi.throwMessageException("Update of non-ref field " + field);
          }
        }
        else {
          ffi.throwMessageException("Update of non-existent field " + field);
        }
      }
      function afterCheck() {
        for(var i = 0; i < refs.length; i++) {
          unsafeSetRef(refs[i], vals[i]);
        }
        return obj;
      }
      return checkAnnArgs(anns, vals, locs, afterCheck);
    }

    function getDotAnn(loc, name, ann, field) {
      checkString(name);
      checkString(field);
      if(ann.hasOwnProperty(field)) {
        return ann[field];
      }
      raiseJSJS(ffi.contractFail(makeSrcloc(loc),
              ffi.makeDotAnnNotPresent(name, field)))
    }

    function PPrimAnn(name, pred) {
      this.name = name;
      this.pred = pred;
      this.refinement = false;
    }
    PPrimAnn.prototype.checkOrFail = function(passed, val, loc) {
      var that = this;
      if(passed) { return ffi.contractOk; }
      else {
        return ffi.contractFail(
          makeSrcloc(loc),
          ffi.makeTypeMismatch(val, that.name));
      }
    }
    PPrimAnn.prototype.check = function(compilerLoc, val) {
      var that = this;
      if(isCheapAnnotation(this)) {
        return this.checkOrFail(this.pred(val), val, compilerLoc);
      }
      else {
        return safeCall(function() {
          return that.pred(val);
        }, function(passed) {
          return that.checkOrFail(passed, val, compilerLoc);
        },
        "PPrimAnn.check");
      }
    }

    function makePrimitiveAnn(name, jsPred) {
      return new PPrimAnn(name, jsPred);
    }

    function PAnnList(anns) {
      this.anns = anns;
      var refinement = true;
//      for(var i = 0; i < anns.length; i++) {
//        if(anns[i].refinement) { refinement = true; }
//      }
      this.refinement = refinement;
    }

    function makePAnnList(anns) {
      return new PAnnList(anns);
    }
    PAnnList.prototype.addAnn = function(ann, loc) {
//      this.refinement = ann.refinement || this.refinement;
      this.anns.push({ ann: ann, loc: loc });
    }

    PAnnList.prototype.check = function(compilerLoc, val) {
      var that = this;
      function checkI(i) {
        if(i >= that.anns.length) { return ffi.contractOk; }
        else {
          return safeCall(function() {
            return that.anns[i].ann.check(compilerLoc, val);
          }, function(passed) {
            if(ffi.isOk(passed)) { return checkI(i + 1); }
            else {
              return ffi.contractFail(
                getField(passed, "loc"),
                ffi.makeRefInitFail(makeSrcloc(that.anns[i].loc), getField(passed, "reason"))
              );
            }
          });
        }
      }
      return checkI(0);
    }

    function PPredAnn(ann, pred, predname) {
      this.ann = ann;
      this.pred = pred;
      this.predname = predname;
      this.refinement = true;
    }
    function makePredAnn(ann, pred, predname) {
      checkFunction(pred);
      checkString(predname);
      return new PPredAnn(ann, pred, predname);
    }
    PPredAnn.prototype.check = function(compilerLoc, val) {
      var that = this;
      return safeCall(function() {
        return that.ann.check(compilerLoc, val);
      }, function(result) {
        if(ffi.isOk(result)) {
          return safeCall(function() {
            return that.pred.app(val);
          }, function(result) {
            if(isPyretTrue(result)) {
              return ffi.contractOk;
            }
            else {
              return ffi.contractFail(
                makeSrcloc(compilerLoc),
                ffi.makePredicateFailure(val, that.predname));
            }
          },
          "PPredAnn.check (after the check)")
        }
        else {
          return result;
        }
      },
      "PPredAnn.check");
    }

    function makeBranderAnn(brander, name) {
      return makePrimitiveAnn(name, function(val) {
        return isObject(val) && hasBrand(val, brander._brand);
      });
    }

    function PRecordAnn(fields, locs, anns) {
      this.fields = fields;
      this.locs = locs;
      this.anns = anns;
      var hasRefinement = false;
      for (var i = 0; i < fields.length; i++) {
        hasRefinement = hasRefinement || anns[fields[i]].refinement;
      }
      this.refinement = hasRefinement;
    }
    function makeRecordAnn(fields, locs, anns) {
      return new PRecordAnn(fields, locs, anns);
    }
    PRecordAnn.prototype.createMissingFieldsError = function(compilerLoc, val) {
      var that = this;
      var missingFields = [];
      for(var i = 0; i < that.fields.length; i++) {
        if(!hasField(val, that.fields[i])) {
          var reason = ffi.makeMissingField(
            makeSrcloc(that.locs[i]),
            that.fields[i]
          );
          missingFields.push(reason);
        }
      }
      return ffi.contractFail(
        makeSrcloc(compilerLoc),
        ffi.makeRecordFieldsFail(val, ffi.makeList(missingFields))
      );
    };
    PRecordAnn.prototype.createRecordFailureError = function(compilerLoc, val, field, result) {
      var that = this;
      var loc;
      for(var i = 0; i < that.fields.length; i++) {
        if(that.fields[i] === field) { loc = that.locs[i]; }
      }
      return ffi.contractFail(
        makeSrcloc(compilerLoc),
        ffi.makeRecordFieldsFail(val, ffi.makeList([
            ffi.makeFieldFailure(
              makeSrcloc(loc),
              field,
              getField(result, "reason")
            )
          ]))
      );
    };
    PRecordAnn.prototype.check = function(compilerLoc, val) {
      var that = this;
      if(!isObject(val)) {
        return ffi.contractFail(
            makeSrcloc(compilerLoc),
            ffi.makeTypeMismatch(val, "Object")
          );
      }
      for(var i = 0; i < that.fields.length; i++) {
        if(!hasField(val, that.fields[i])) {
          return that.createMissingFieldsError(compilerLoc, val);
        }
      }

      function deepCheckFields(remainingFields) {
        var thisField;
        return safeCall(function() {
          thisField = remainingFields.pop();
          var thisChecker = that.anns[thisField];
          return thisChecker.check(that.locs[that.locs.length - remainingFields.length], getColonField(val, thisField));
        }, function(result) {
          if(ffi.isOk(result)) {
            if(remainingFields.length === 0) { return ffi.contractOk; }
            else { return deepCheckFields(remainingFields); }
          }
          else if(ffi.isFail(result)) {
            return that.createRecordFailureError(compilerLoc, val, thisField, result);
          }
        },
        "deepCheckFields");
      }
      if(that.fields.length === 0) { return ffi.contractOk; }
      else { return deepCheckFields(that.fields.slice()); }
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
    Cont.prototype.toString = function() {
      var stack = this.stack;
      var stackStr = stack && stack.length > 0 ? 
        stack.map(function(s) { return s && s.from ? s.from.join(",") : "<blank frame>"; }).join("\n") : "<no stack trace>";
      return stackStr;
    }

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
        if (--thisRuntime.GAS <= 0) {
          thisRuntime.EXN_STACKHEIGHT = 0;
          throw thisRuntime.makeCont();
        }
        while(true) {
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
      function(ignored) {
        return program(thisRuntime, namespace);
      },
      0,
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
    var initialGas = thisRuntime.INITIAL_GAS;

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
          TOS++;
          RUN_ACTIVE = true;
          setTimeout(iter, 0);
        },
        break: breakFun,
        error: function(errVal) {
          threadIsCurrentlyPaused = true;
          threadIsDead = true;
          var exn;
          if(isPyretException(errVal)) {
            exn = errVal;
          } else {
            exn = new PyretFailException(errVal); 
          }
          finishFailure(exn);
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
            
            if (next.fun instanceof Function) {
              val = next.fun(next);
            }
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
            // console.log("theOneTrueStack = ", theOneTrueStack.slice(0, theOneTrueStackHeight).map(function(f) {
            //   if (f && f.from) { return f.from.toString(); }
            //   else { return f; }
            // }));

            if(isPause(e)) {
              thisThread.pause();
              e.pause.setHandlers(thisThread.handlers);
              if(e.resumer) { e.resumer(e.pause); }
              return;
            }
            else if(thisRuntime.isCont(e)) {
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


  var UNINITIALIZED_ANSWER = {'uninitialized answer': true};
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
  function makeActivationRecord(from, fun, step, args, vars) {
    return new ActivationRecord(from, fun, step, UNINITIALIZED_ANSWER, args, vars);
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
          throw "Cannot resume with error or break requested";
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

    function getExnValue(v) {
      if(!isOpaque(v) && !isPyretException(v.val)) {
        ffi.throwMessageException("Got non-exception value in getExnVal");
      }
      return v.val.exn;
    }

    function execThunk(thunk) {
      function wrapResult(res) {
        if(isSuccessResult(res)) {
          return ffi.makeLeft(res.result);
        } else if (isFailureResult(res)) {
          if(isPyretException(res.exn)) {
            return ffi.makeRight(makeOpaque(res.exn));
          }
          else {
            return ffi.makeRight(makeOpaque(makePyretFailException(ffi.makeMessageException(String(res.exn + "\n" + res.exn.stack)))));
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
      thisRuntime.checkArity(2, arguments, "_plus");
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
      thisRuntime.checkArity(2, arguments, "_minus");
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkNumber(r)
        return thisRuntime.makeNumberBig(jsnums.subtract(l, r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_minus")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_minus").app(r);
          });
      } else {
        ffi.throwNumericBinopError(l, r, "-", "_minus");
      }
    };

    var times = function(l, r) {
      thisRuntime.checkArity(2, arguments, "_times");
      if (thisRuntime.isNumber(l)) {
        thisRuntime.checkNumber(r);
        return thisRuntime.makeNumberBig(jsnums.multiply(l, r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_times")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_times").app(r);
          });
      } else {
        ffi.throwNumericBinopError(l, r, "*", "_times");
      }
    };

    var divide = function(l, r) {
      thisRuntime.checkArity(2, arguments, "_divide");
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
        ffi.throwNumericBinopError(l, r, "/", "_divide");
      }
    };

    var lessthan = function(l, r) {
      thisRuntime.checkArity(2, arguments, "_lessthan");
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
        ffi.throwNumericBinopError(l, r, "<", "_lessthan");
      }
    };

    var greaterthan = function(l, r) {
      thisRuntime.checkArity(2, arguments, "_greaterthan");
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
        ffi.throwNumericBinopError(l, r, ">", "_greaterthan");
      }
    };

    var lessequal = function(l, r) {
      thisRuntime.checkArity(2, arguments, "_lessequal");
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
        ffi.throwNumericBinopError(l, r, "<=", "_lessequal");
      }
    };

    var greaterequal = function(l, r) {
      thisRuntime.checkArity(2, arguments, "_greaterequal");
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
        ffi.throwNumericBinopError(l, r, ">=", "_greaterequal");
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
      thisRuntime.checkArity(3, arguments, "string-substring");
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
      thisRuntime.checkArity(3, arguments, "string-replace");
      thisRuntime.checkString(s);
      thisRuntime.checkString(find);
      thisRuntime.checkString(replace);
      var escapedFind = find.replace(/\\/g, "\\\\");
      return thisRuntime.makeString(s.replace(new RegExp(escapedFind,'g'), replace));
    }

    var string_equals = function(l, r) {
      thisRuntime.checkArity(2, arguments, "string-equals");
      thisRuntime.checkString(l);
      thisRuntime.checkString(r);
      return thisRuntime.makeBoolean(same(l, r));
    }
    var string_append = function(l, r) {
      thisRuntime.checkArity(2, arguments, "string-append");
      thisRuntime.checkString(l);
      thisRuntime.checkString(r);
      return thisRuntime.makeString(l.concat(r));
    }
    var string_contains = function(l, r) {
      thisRuntime.checkArity(2, arguments, "string-contains");
      thisRuntime.checkString(l);
      thisRuntime.checkString(r);
      return thisRuntime.makeBoolean(l.indexOf(r) !== -1);
    }
    var string_length = function(s) {
      thisRuntime.checkArity(1, arguments, "string-length");
      thisRuntime.checkString(s);
      return thisRuntime.makeNumber(s.length);
    }
    var string_isnumber = function(s) {
      thisRuntime.checkArity(1, arguments, "string-isnumber");
      checkString(s);
      var num = jsnums.fromString(s);
      if(num) { return true; }
      else { return false; }
    }
    var string_tonumber = function(s) {
      thisRuntime.checkArity(1, arguments, "string-tonumber");
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
      thisRuntime.checkArity(2, arguments, "string-repeat");
      thisRuntime.checkString(s);
      thisRuntime.checkNumber(n);
      var resultStr = "";
      // TODO(joe): loop up to a fixnum?
      for(var i = 0; i < jsnums.toFixnum(n); i++) {
        resultStr += s;
      }
      return makeString(resultStr);
    }
    var string_split_all = function(s, splitstr) {
      thisRuntime.checkArity(2, arguments, "string-split-all");
      thisRuntime.checkString(s);
      thisRuntime.checkString(splitstr);
      
      return ffi.makeList(s.split(splitstr).map(thisRuntime.makeString));
    }
    var string_split = function(s, splitstr) {
      thisRuntime.checkArity(2, arguments, "string-split");
      thisRuntime.checkString(s);
      thisRuntime.checkString(splitstr);

      var idx = s.indexOf(splitstr);
      if (idx === -1) 
        return ffi.makeList([thisRuntime.makeString(s)]);
      else
        return ffi.makeList([thisRuntime.makeString(s.slice(0, idx)), 
                             thisRuntime.makeString(s.slice(idx + splitstr.length))]);
    }
    var string_charat = function(s, n) {
      thisRuntime.checkArity(2, arguments, "string-char-at");
      thisRuntime.checkString(s);
      thisRuntime.checkNumber(n);
      
      //TODO: Handle bignums that are beyond javascript
      return thisRuntime.makeString(String(s.charAt(jsnums.toFixnum(n))));
    }
    var string_toupper = function(s) {
      thisRuntime.checkArity(1, arguments, "string-toupper");
      thisRuntime.checkString(s);
      return thisRuntime.makeString(s.toUpperCase());
    }
    var string_tolower = function(s) {
      thisRuntime.checkArity(1, arguments, "string-tolower");
      thisRuntime.checkString(s);
      return thisRuntime.makeString(s.toLowerCase());
    }
    var string_explode = function(s) {
      thisRuntime.checkArity(1, arguments, "string-explode");
      thisRuntime.checkString(s);
      return ffi.makeList(s.split("").map(thisRuntime.makeString));
    }
    var string_indexOf = function(s, find) {
      thisRuntime.checkArity(2, arguments, "string-index-of");
      thisRuntime.checkString(s);
      thisRuntime.checkString(find);
      return thisRuntime.makeNumberBig(s.indexOf(find));
    }

    var bool_not = function(l) {
      thisRuntime.checkArity(1, arguments, "not");
      thisRuntime.checkBoolean(l);
      return thisRuntime.makeBoolean(!l);
    }

    var num_equals = function(l, r) {
      thisRuntime.checkArity(2, arguments, "num-equals");
      thisRuntime.checkNumber(l);
      thisRuntime.checkNumber(r);
      return thisRuntime.makeBoolean(same(l, r));
    }
    var num_max = function(l, r) {
      thisRuntime.checkArity(2, arguments, "num-max");
      thisRuntime.checkNumber(l);
      thisRuntime.checkNumber(r);
      if (jsnums.greaterThanOrEqual(l, r)) { return l; } else { return r; }
    }

    var num_min = function(l, r) {
      thisRuntime.checkArity(2, arguments, "num-min");
      thisRuntime.checkNumber(l);
      thisRuntime.checkNumber(r);
      if (jsnums.lessThanOrEqual(l, r)) { return l; } else { return r; }
    }

    var num_abs = function(n) {
      thisRuntime.checkArity(1, arguments, "num-abs");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.abs(n));
    }
      
    var num_sin = function(n) {
      thisRuntime.checkArity(1, arguments, "num-sin");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.sin(n));
    }
    var num_cos = function(n) {
      thisRuntime.checkArity(1, arguments, "num-cos");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.cos(n));
    }
    var num_tan = function(n) {
      thisRuntime.checkArity(1, arguments, "num-tan");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.tan(n));
    }
    var num_asin = function(n) {
      thisRuntime.checkArity(1, arguments, "num-asin");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.asin(n));
    }
    var num_acos = function(n) {
      thisRuntime.checkArity(1, arguments, "num-acos");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.acos(n));
    }
    var num_atan = function(n) {
      thisRuntime.checkArity(1, arguments, "num-atan");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.atan(n));
    }

    var num_modulo = function(n, mod) {
      thisRuntime.checkArity(2, arguments, "num-modulo");
      thisRuntime.checkNumber(n);
      thisRuntime.checkNumber(mod);
      return thisRuntime.makeNumberBig(jsnums.modulo(n, mod));
    }
    var num_truncate = function(n) {
      thisRuntime.checkArity(1, arguments, "num-truncate");
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
      thisRuntime.checkArity(1, arguments, "num-sqrt");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.sqrt(n));
    }
    var num_sqr = function(n) {
      thisRuntime.checkArity(1, arguments, "num-sqr");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.sqr(n));
    }
    var num_ceiling = function(n) {
      thisRuntime.checkArity(1, arguments, "num-ceiling");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.ceiling(n));
    }
    var num_floor = function(n) {
      thisRuntime.checkArity(1, arguments, "num-floor");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.floor(n));
    }
    var num_log = function(n) {
      thisRuntime.checkArity(1, arguments, "num-log");
      thisRuntime.checkNumber(n);
      if (jsnums.greaterThan(n, 0)) {
        return thisRuntime.makeNumberBig(jsnums.log(n));
      }
      else {
        throw makeMessageException("log: expected a number greater than 0 but got " + String(n));
      }
    }
    var num_exp = function(n) {
      thisRuntime.checkArity(1, arguments, "num-exp");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.exp(n));
    }
    var num_exact = function(n) {
      thisRuntime.checkArity(1, arguments, "num-exact");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.toExact(n));
    }
    var num_is_integer = function(n) {
      thisRuntime.checkArity(1, arguments, "num-is-integer");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeBoolean(jsnums.isInteger(n))
    }
    var num_is_fixnum = function(n) {
      thisRuntime.checkArity(1, arguments, "num-is-fixnum");
      thisRuntime.checkNumber(n);
      return thisRuntime.makeBoolean(typeof n === "number");
    }
    var num_expt = function(n, pow) {
      thisRuntime.checkArity(2, arguments, "num-expt");
      thisRuntime.checkNumber(n);
      thisRuntime.checkNumber(pow);
      return thisRuntime.makeNumberBig(jsnums.expt(n, pow));
    }
    var num_tostring = function(n) {
      thisRuntime.checkArity(1, arguments, "num-tostring");
      thisRuntime.checkNumber(n);
      return makeString(String(n));
    }
    var num_tostring_digits = function(n, digits) {
      thisRuntime.checkArity(2, arguments, "num-tostring-digits");
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
      thisRuntime.checkArity(1, arguments, "random");
      return makeNumber(jsnums.floor(jsnums.multiply(Math.random(), max)));
    }

    function loadModule(module, runtime, namespace, withModule) {
      var modstring = String(module).substring(0, 500);
      return thisRuntime.safeCall(function() {
          return module(thisRuntime, namespace);
        },
        withModule, "loadModule(" + modstring.substring(0, 70) + ")");
    }
    function loadJSModules(namespace, modules, withModules) {
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
    function loadModulesNew(namespace, modules, withModules) {
      return loadJSModules(namespace, modules, function(/* args */) {
        var ms = Array.prototype.slice.call(arguments);
        function wrapMod(m) {
          if (hasField(m, "provide-plus-types")) {
            return getField(m, "provide-plus-types");
          }
          else {
            return thisRuntime.makeObject({
              "values": getField(m, "provide"),
              "types": {}
            });
          }
        };
        var wrappedMods = ms.map(wrapMod);
        return withModules.apply(null, wrappedMods);
      });
    }
    function loadModules(namespace, modules, withModules) {
      return loadModulesNew(namespace, modules, function(/* varargs */) {
        var ms = Array.prototype.slice.call(arguments);
        return safeTail(function() {
          return withModules.apply(null, ms.map(function(m) { return getField(m, "values"); }));
        });
      });
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

          // NOTE(joe): the $type$ sadness is because we only have one dynamic
          // namespace
          '$type$Number': NumberC,
          '$type$String': StringC,
          '$type$Boolean': BooleanC,
          '$type$Nothing': NothingC,
          '$type$Function': FunctionC,
          '$type$RawArray': RawArrayC,
          '$type$Method': MethodC,
          '$type$Object': ObjectC,
          '$type$Any': AnyC,

          'Number': NumberC,
          'String': StringC,
          'Boolean': BooleanC,
          'Nothing': NothingC,
          'Function': FunctionC,
          'RawArray': RawArrayC,
          'Method': MethodC,
          'Object': ObjectC,
          'Any': AnyC,

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
          'string-isnumber': makeFunction(string_isnumber),
          'string-tonumber': makeFunction(string_tonumber),
          'string-repeat': makeFunction(string_repeat),
          'string-substring': makeFunction(string_substring),
          'string-replace': makeFunction(string_replace),
          'string-split': makeFunction(string_split),
          'string-split-all': makeFunction(string_split_all),
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

          'not': makeFunction(bool_not),

          'ref-set'    : makeFunction(setRef),
          'ref-get'    : makeFunction(getRef),
          'ref-end-graph'   : makeFunction(refEndGraph),
          'ref-freeze' : makeFunction(freezeRef),

          'identical3': identical3Py,
          'identical': identicalPy,
          'equal-now3': makeFunction(equalNow3),
          'equal-now': equalNowPy,
          'equal-always3': makeFunction(equalAlways3),
          'equal-always': equalAlwaysPy,

          'exn-unwrap': makeFunction(getExnValue)

        }),
        'run': run,
        'runThunk': runThunk,
        'safeCall': safeCall,
        'safeTail': safeTail,
        'printPyretStack': printPyretStack,

        'isActivationRecord'   : isActivationRecord,
        'makeActivationRecord' : makeActivationRecord,

        'GAS': INITIAL_GAS,
        'INITIAL_GAS': INITIAL_GAS,

        'namedBrander': namedBrander,

        'checkAnn': checkAnn,
        '_checkAnn': _checkAnn,
        'checkAnnArg': checkAnnArg,
        'checkAnnArgs': checkAnnArgs,
        'checkConstructorArgs': checkConstructorArgs,
        '_checkAnnArgs': _checkAnnArgs,
        'getDotAnn': getDotAnn,
        'makePredAnn': makePredAnn,
        'makePrimitiveAnn': makePrimitiveAnn,
        'makeBranderAnn': makeBranderAnn,
        'makeRecordAnn': makeRecordAnn,

        'Number': NumberC,
        'String': StringC,
        'Boolean': BooleanC,
        'RawArray': RawArrayC,
        'Any': AnyC,
        'Function': FunctionC,
        'Method': MethodC,
        'Object': ObjectC,
        'Nothing': NothingC,

        'makeCont'    : makeCont,
        'isCont'      : isCont,
        'makePause'   : makePause,
        'isPause'     : isPause,

        'pauseStack'  : pauseStack,
        'schedulePause'  : schedulePause,
        'breakAll' : breakAll,

        'getField'      : getField,
        'getFieldLoc'   : getFieldLoc,
        'getFieldRef'   : getFieldRef,
        'getFields'     : getFields,
        'getColonField' : getColonField,
        'extendObj'     : extendObj,

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
        'isRef'       : isRef,
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
        'makeGraphableRef' : makeGraphableRef,
        'makeRef' : makeRef,
        'makeUnsafeSetRef' : makeUnsafeSetRef,
        'makeDataValue': makeDataValue,
        'makeMatch': makeMatch,
        'makeOpaque'   : makeOpaque,

        'checkRefAnns' : checkRefAnns,

        'isGraphableRef' : isGraphableRef,
        'isRefGraphable' : isRefGraphable,
        'isRefFrozen' : isRefFrozen,
        'isRefSet' : isRefSet,
        'setRef' : setRef,
        'unsafeSetRef' : unsafeSetRef,
        'getRef' : getRef,
        'refEndGraph' : refEndGraph,
        'addRefAnn' : addRefAnn,
        'addRefAnns' : addRefAnns,
        'freezeRef' : freezeRef,

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
        'string_isnumber': string_isnumber,
        'string_tonumber': string_tonumber,
        'string_repeat': string_repeat,
        'string_substring': string_substring,
        'string_replace': string_replace,
        'string_split': string_split,
        'string_split_all': string_split_all,
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
        'identical3': identical3,
        'identical': identical,
        'equal_now3': equalNow3,
        'equal_now': equalNow,
        'equal_always3': equalAlways3,
        'equal_always': equalAlways,
        'raise': raiseJSJS,

        'pyretTrue': pyretTrue,
        'pyretFalse': pyretFalse,

        'undefined': undefined,
        'create': Object.create,

        'hasField' : hasField,

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
        'loadModulesNew' : loadModulesNew,
        'loadJSModules' : loadJSModules,
        'modules' : Object.create(null),
        'setStdout': function(newStdout) {
          theOutsideWorld.stdout = newStdout;
        },
        'getParam' : getParam,
        'setParam' : setParam,
        'hasParam' : hasParam
    };

    var ffi = {
      contractOk: true,
      isOk: function() { return true; }
    }


    var list;
    var srcloc;
    var ffi;
    loadModulesNew(thisRuntime.namespace,
      [require("trove/lists"), require("trove/srcloc")],
      function(listsLib, srclocLib) {
        list = getField(listsLib, "values");
        srcloc = getField(srclocLib, "values");
      });
    loadJSModules(thisRuntime.namespace, [require("js/ffi-helpers")], function(f) {
      ffi = f;
      thisRuntime["ffi"] = ffi;
    });

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
