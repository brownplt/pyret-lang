/***
This is the runtime for the ANF'd version of pyret
*/
"use strict";
/** @typedef {!Object} */
var Bignum;

define(["js/namespace", "js/js-numbers", "js/codePoint", "seedrandom", "js/runtime-util"],
       function (Namespace, jsnums, codePoint, seedrandom, util) {

  if(util.isBrowser()) {
    var require = requirejs;
  }
  else {
    var require = requirejs.nodeRequire("requirejs");
  }

  function copyArgs(args) {
    return Array.prototype.slice.call(args);
  }

  var codePointAt = codePoint.codePointAt;
  var fromCodePoint = codePoint.fromCodePoint;

/**
Creates a Pyret runtime
@param {{stdout : function(string), initialGas : number}}
@return {Object} that contains all the necessary components of a runtime
*/
function makeRuntime(theOutsideWorld) {
  var CONSOLE = theOutsideWorld.console || console;
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
  sub.prototype = Object.create(from.prototype);
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
  var fieldsObj = Object.create(null);
  var fields = [];
  var currentProto = obj.dict;
  while(currentProto !== null) {
    var keys = Object.keys(currentProto);
    for (var i = 0; i < keys.length; i++)
      fieldsObj[keys[i]] = true;
    currentProto = getProto(currentProto);
  }
  fields = Object.keys(fieldsObj)
  return fields;
}

var emptyDict = Object.create(null);

/**Tests whether an object is a PBase
    @param {Object} obj the item to test
    @return {boolean} true if object is a PBase
*/
function isBase(obj) { return obj instanceof PBase; }

  function renderValueSkeleton(val, values) {
    if (ffi.isVSValue(val)) { return values.pop(); } // double-check order!
    else if (ffi.isVSStr(val)) { return thisRuntime.unwrap(thisRuntime.getField(val, "s")); }
    else if (ffi.isVSCollection(val)) {
      var name = thisRuntime.unwrap(thisRuntime.getField(val, "name"));
      var items = ffi.toArray(thisRuntime.getField(val, "items"));
      var s = "[" + name + ": ";
      for (var i = 0; i < items.length; i++) {
        if (i > 0) { s += ", "; }
        s += renderValueSkeleton(items[i], values);
      }
      return s + "]";
    } else if (ffi.isVSConstr(val)) {
      var name = thisRuntime.unwrap(thisRuntime.getField(val, "name"));
      var items = ffi.toArray(thisRuntime.getField(val, "args"));
      var s = name + "(";
      for (var i = 0; i < items.length; i++) {
        if (i > 0) { s += ", "; }
        s += renderValueSkeleton(items[i], values);
      }
      return s + ")";
    } else if (ffi.isVSSeq(val)) {
      var items = ffi.toArray(thisRuntime.getField(val, "items"));
      var s = "";
      for (var i = 0; i < items.length; i++) {
        s += renderValueSkeleton(items[i], values);
      }
      return s;
    }
  }

  var DefaultReprMethods = {
    "string": String,
    "number": String,
    "boolean": String,
    "nothing": function(val) { return "nothing"; },
    "function": function(val) { return "<function>"; },
    "method": function(val) { return "<method>"; },
    "cyclic": function(val) { return val; },
    "opaque": function(val) {
      if (thisRuntime.imageLib.isImage(val.val)) {
        return "<image (" + String(val.val.getWidth()) + "x" + String(val.val.getHeight()) + ")>";
      } else {
        return "<internal value>";
      }
    },
    "object": function(val, pushTodo) {
      var keys = [];
      var vals = [];
      for (var field in val.dict) {
        keys.push(field); // NOTE: this is reversed order from the values,
        vals.unshift(val.dict[field]); // because processing will reverse them back
      }
      pushTodo(undefined, val, undefined, vals, "render-object", { keys: keys });
    },
    "render-object": function(top) {
      var s = "{";
      for (var i = 0; i < top.extra.keys.length; i++) {
        if (i > 0) { s += ", "; }
        s += top.extra.keys[i] + ": " + top.done[i];
      }
      s += "}";
      return s;
    },
    "ref": function(val, implicit, pushTodo) {
      pushTodo(undefined, undefined, val, [getRef(val)], "render-ref", { implicit: implicit });
    },
    "render-ref": function(top) {
      var s = "";
      if (top.extra.implicit) {
        s += top.done[0];
      } else {
        s += "ref(" + top.done[0] + ")";
      }
      return s;
    },
    "data": function(val, pushTodo) {
      var vals = val.$app_fields_raw(function(/* varargs */) {
        var ans = new Array(arguments.length);
        for (var i = 0; i < arguments.length; i++) ans[i] = arguments[i];
        return ans;
      });
      pushTodo(undefined, val, undefined, vals, "render-data",
               { arity: val.$arity, implicitRefs: val.$mut_fields_mask,
                 fields: val.$constructor.$fieldNames, constructorName: val.$name });
    },
    "render-data": function(top) {
      var s = top.extra.constructorName;
      // Sentinel value for singleton constructors
      if(top.extra.arity !== -1) {
        s += "(";
        for(var i = top.done.length - 1; i >= 0; i--) {
          if(i < top.done.length - 1) { s += ", "; }
          s += top.done[i];
        }
        s += ")";
      }
      return s;
    },
    "array": function(val, pushTodo) {
      pushTodo(val, undefined, undefined, Array.prototype.slice.call(val), "render-array");
    },
    "render-array": function(top) {
      var s = "[raw-array: ";
      for(var i = top.done.length - 1; i >= 0; i--) {
        if(i < top.done.length - 1) { s += ", "; }
        s += top.done[i];
      }
      s += "]";
      return s;
    },
    "valueskeleton": function(val, output, pushTodo) {
      // NOTE: this is the eager version;
      // a lazy version would skip getting the skeleton values altogether
      var values = ffi.skeletonValues(output);
      pushTodo(undefined, val, undefined, values, "render-valueskeleton",
               { skeleton: output });
    },
    "render-valueskeleton": function(top) {
      var skel = top.extra.skeleton;
      top.extra.skeleton = undefined;
      return renderValueSkeleton(skel, top.done);
    }
  };

  var ReprMethods = {};
  ReprMethods["_torepr"] = Object.create(DefaultReprMethods);
  ReprMethods["_torepr"]["string"] = function(str) {
    return '"' + replaceUnprintableStringChars(String(str)) + '"';
  };

  ReprMethods["_tostring"] = Object.create(DefaultReprMethods);

  ReprMethods.createNewRenderer = function createNewRenderer(name, base) {
    if (ReprMethods[name]) { return false; }
    ReprMethods[name] = Object.create(base);
    return true;
  }

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
      if (ffi === undefined || ffi.throwInternalError === undefined) {
        throw ("FFI or ffi.throwInternalError is not yet defined, and lookup of field " + field + " on undefined failed at location " + JSON.stringify(loc));
      } else {
        ffi.throwInternalError("Field lookup on undefined ", ffi.makeList([field]));
      }
    }
    if(!isObject(val)) { ffi.throwLookupNonObject(makeSrcloc(loc), val, field); }
    var fieldVal = val.dict[field];
    if(fieldVal === undefined) {
      if (ffi === undefined || ffi.throwFieldNotFound === undefined) {
        throw ("FFI or ffi.throwFieldNotFound is not yet defined, and lookup of field " + field + " on " + toReprJS(val, ReprMethods._torepr) + " failed at location " + JSON.stringify(loc));
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
  return getColonFieldLoc(val, field, ["runtime"]);
}
function getColonFieldLoc(val, field, loc) {
  if(val === undefined) { ffi.throwInternalError("Field lookup on undefined ", [field]); }
  if(!isObject(val)) { ffi.throwLookupNonObject(makeSrcloc(loc), val, field); }
  var fieldVal = val.dict[field];
  if(fieldVal === undefined) {
    ffi.throwFieldNotFound(makeSrcloc(loc), val, field);
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

var isNumber = jsnums.isPyretNumber;

function isJSNumber(obj) {
  return typeof obj === "number";
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
    this.dict = emptyDict;

    /**@type {!Object.<string, Boolean>}*/
    this.brands = noBrands;
}

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
    this.dict = emptyDict;

    /**@type {!Object.<string, Boolean>}*/
    this.brands = noBrands;

}

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

/**Makes a PMethod using the given function
  The function first argument should be self

  @param {Function} meth The Curried JS function that represents the body of the method
  @param {Function} full_meth The Full JS function that represents the body of the method
      @return {!PMethod} with app of fun
    */
    function makeMethod(meth, full_meth) {
      return new PMethod(meth, full_meth);
    }
    var app0 = function(obj) {
      var that = this;
      return function() { return that.full_meth(obj); }
    };
    var app1 = function(obj) {
      var that = this;
      return function(v) { return that.full_meth(obj, v); };
    };
    var app2 = function(obj) {
      var that = this;
      return function(v1, v2) { return that.full_meth(obj, v1, v2); };
    };
    var app3 = function(obj) {
      var that = this;
      return function(v1, v2, v3) { return that.full_meth(obj, v1, v2, v3); };
    };
    var app4 = function(obj) {
      var that = this;
      return function(v1, v2, v3, v4) { return that.full_meth(obj, v1, v2, v3, v4); };
    };
    var app5 = function(obj) {
      var that = this;
      return function(v1, v2, v3, v4, v5) { return that.full_meth(obj, v1, v2, v3, v4, v5); };
    };
    var app6 = function(obj) {
      var that = this;
      return function(v1, v2, v3, v4, v5, v6) { return that.full_meth(obj, v1, v2, v3, v4, v5, v6); };
    };
    var app7 = function(obj) {
      var that = this;
      return function(v1, v2, v3, v4, v5, v6, v7) { return that.full_meth(obj, v1, v2, v3, v4, v5, v6, v7); };
    };
    var app8 = function(obj) {
      var that = this;
      return function(v1, v2, v3, v4, v5, v6, v7, v8) { return that.full_meth(obj, v1, v2, v3, v4, v5, v6, v7, v8); };
    };
    var appN = function(obj) {
      var that = this;
      return function() {
        var argList = new Array(arguments.length);
        for (var i = 0; i < arguments.length; i++) argList[i] = arguments[i];
        return that.full_meth.apply(null, [obj].concat(argList));
      };
    }
    function makeMethod0(meth) {
      return new PMethod(app0, meth);
    }
    function makeMethod1(meth) {
      return new PMethod(app1, meth);
    }
    function makeMethod2(meth) {
      return new PMethod(app2, meth);
    }
    function makeMethod3(meth) {
      return new PMethod(app3, meth);
    }
    function makeMethod3(meth) {
      return new PMethod(app3, meth);
    }
    function makeMethod4(meth) {
      return new PMethod(app4, meth);
    }
    function makeMethod5(meth) {
      return new PMethod(app5, meth);
    }
    function makeMethod6(meth) {
      return new PMethod(app6, meth);
    }
    function makeMethod7(meth) {
      return new PMethod(app7, meth);
    }
    function makeMethod8(meth) {
      return new PMethod(app8, meth);
    }

    function makeMethodFromFun(meth) {
      return new PMethod(appN, meth);
    }
    var makeMethodN = makeMethodFromFun;

    function callIfPossible0(L, fun, obj) {
      if (isMethod(fun)) {
        return fun.full_meth(obj);
      } else if (isFunction(fun)) {
        return fun.app();
      } else {
        ffi.throwNonFunApp(L, fun);
      }
    }
    function callIfPossible1(L, fun, obj, v1) {
      if (isMethod(fun)) {
        return fun.full_meth(obj, v1);
      } else if (isFunction(fun)) {
        return fun.app(v1);
      } else {
        ffi.throwNonFunApp(L, fun);
      }
    }
    function callIfPossible2(L, fun, obj, v1, v2) {
      if (isMethod(fun)) {
        return fun.full_meth(obj, v1, v2);
      } else if (isFunction(fun)) {
        return fun.app(v1, v2);
      } else {
        ffi.throwNonFunApp(L, fun);
      }
    }
    function callIfPossible3(L, fun, obj, v1, v2, v3) {
      if (isMethod(fun)) {
        return fun.full_meth(obj, v1, v2, v3);
      } else if (isFunction(fun)) {
        return fun.app(v1, v2, v3);
      } else {
        ffi.throwNonFunApp(L, fun);
      }
    }
    function callIfPossible4(L, fun, obj, v1, v2, v3, v4) {
      if (isMethod(fun)) {
        return fun.full_meth(obj, v1, v2, v3, v4);
      } else if (isFunction(fun)) {
        return fun.app(v1, v2, v3, v4);
      } else {
        ffi.throwNonFunApp(L, fun);
      }
    }
    function callIfPossible5(L, fun, obj, v1, v2, v3, v4, v5) {
      if (isMethod(fun)) {
        return fun.full_meth(obj, v1, v2, v3, v4, v5);
      } else if (isFunction(fun)) {
        return fun.app(v1, v2, v3, v4, v5);
      } else {
        ffi.throwNonFunApp(L, fun);
      }
    }
    function callIfPossible6(L, fun, obj, v1, v2, v3, v4, v5, v6) {
      if (isMethod(fun)) {
        return fun.full_meth(obj, v1, v2, v3, v4, v5, v6);
      } else if (isFunction(fun)) {
        return fun.app(v1, v2, v3, v4, v5, v6);
      } else {
        ffi.throwNonFunApp(L, fun);
      }
    }
    function callIfPossible7(L, fun, obj, v1, v2, v3, v4, v5, v6, v7) {
      if (isMethod(fun)) {
        return fun.full_meth(obj, v1, v2, v3, v4, v5, v6, v7);
      } else if (isFunction(fun)) {
        return fun.app(v1, v2, v3, v4, v5, v6, v7);
      } else {
        ffi.throwNonFunApp(L, fun);
      }
    }
    function callIfPossible8(L, fun, obj, v1, v2, v3, v4, v5, v6, v7, v8) {
      if (isMethod(fun)) {
        return fun.full_meth(obj, v1, v2, v3, v4, v5, v6, v7, v8);
      } else if (isFunction(fun)) {
        return fun.app(v1, v2, v3, v4, v5, v6, v7, v8);
      } else {
        ffi.throwNonFunApp(L, fun);
      }
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
      return typeof val === "object" && val instanceof PRef;
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
        var f = function(self, handlers, els) {
          if(hasField(handlers, name)) {
            return getField(handlers, name).app();
          }
          else {
            return els.app(self);
          }
        };
        return makeMethod2(f);
      }
      else {
        var f = function(self, handlers, _else) {
          if(hasField(handlers, name)) {
            return self.$app_fields(getField(handlers, name).app, self.$mut_fields_mask);
          }
          else {
            return _else.app(self);
          }
        };
        return makeMethod2(f);
      }
    }

    function makeDataValue(dict, brands, $name, $app_fields, $app_fields_raw, $arity, $mut_fields_mask, constructor) {
      var ret = new PObject(dict, brands);
      ret.$name = $name;
      ret.$app_fields = $app_fields;
      ret.$app_fields_raw = $app_fields_raw;
      ret.$mut_fields_mask = $mut_fields_mask;
      ret.$arity = $arity;
      ret.$constructor = constructor
      return ret;
    }

    function isDataValue(v) {
      return hasProperty(v, "$name") && hasProperty(v, "$app_fields") && hasProperty(v, "$arity");
    }

    function derefField(value, fieldIsRef, lookupIsRef) {
      if(isRef(value)) {
        if(lookupIsRef) {
          // ref keyword in cases and either kind of field
          // Update fields in place with deref
          return getRef(value);
          fields[i] = getRef(fields[i]);
        } else if(fieldIsRef) {
          ffi.throwMessageException("Cases on ref field needs to use ref");
        }
        else {
          return value;
        }
      }
      else {
        if(lookupIsRef) {
          ffi.throwMessageException("Cannot use ref in cases to access non-ref field");
        }
        else {
          return value;
        }
      }
    }

    /**The representation of an array
       A PArray is simply a JavaScript array
    */
    function isArray(val) {
      return Array.isArray(val);
    }
    function makeArray(arr) {
      return arr;
    }

    /************************
          Type Checking
    ************************/
    function checkType(val, test, typeName) {
      if(!test(val)) {
        debugger;
        ffi.throwTypeMismatch(val, typeName);
      }
      return true;
    }

    function isPyretVal(val) {
      if (typeof val === "string" || typeof val === "boolean" || val instanceof Array) {
        return true;
      }
      else if (jsnums.isPyretNumber(val)) {
        return true;
      }
      else if (isObject(val) ||
               isFunction(val) ||
               isMethod(val) ||
               isRef(val) ||
               isOpaque(val) ||
               isNothing(val)) {
        return true;
      }
      return false;
    }

    var checkArity = function(expected, args, source) {
      if (expected !== args.length) {
        throw ffi.throwArityErrorC([source], expected, args);
      }
    }
    var checkArityC = function(cloc, expected, args) {
      if (expected !== args.length) {
        throw ffi.throwArityErrorC(cloc, expected, args);
      }
    }

    var makeCheckType = function(test, typeName) {
      if (arguments.length !== 2) {
        // can't use checkArity yet because ffi isn't initialized
        throw("MakeCheckType was called with the wrong number of arguments: expected 2, got " + arguments.length);
      }
      return function(val) {
        if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["runtime"], 1, $a); }
        return checkType(val, test, typeName);
      };
    }
    var checkString = makeCheckType(isString, "String");
    var checkNumber = makeCheckType(isNumber, "Number");
    var checkExactnum = makeCheckType(jsnums.isRational, "Exactnum");
    var checkRoughnum = makeCheckType(jsnums.isRoughnum, "Roughnum");
    var checkNumInteger = makeCheckType(jsnums.isInteger, "NumInteger");
    var checkNumRational = makeCheckType(jsnums.isRational, "NumRational");
    var checkNumPositive = makeCheckType(jsnums.isPositive, "NumPositive");
    var checkNumNegative = makeCheckType(jsnums.isNegative, "NumNegative");
    var checkNumNonPositive = makeCheckType(jsnums.isNonPositive, "NumNonPositive");
    var checkNumNonNegative = makeCheckType(jsnums.isNonNegative, "NumNonNegative");
    // var checkArray = makeCheckType(isArray, "Array");
    var checkArray = makeCheckType(isArray, "RawArray");
    var checkBoolean = makeCheckType(isBoolean, "Boolean");
    var checkObject = makeCheckType(isObject, "Object");
    var checkFunction = makeCheckType(isFunction, "Function");
    var checkMethod = makeCheckType(isMethod, "Method");
    var checkOpaque = makeCheckType(isOpaque, "Opaque");
    var checkPyretVal = makeCheckType(isPyretVal, "Pyret Value");

    var checkWrapBoolean = function(val) {
      checkBoolean(val);
      return val;
    };

    function confirm(val, test) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["runtime"], 2, $a); }
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
    var namedBrander = function(name, srcloc) {
      var thisBrandStr = mkBrandName(name);
      var testSrcloc = srcloc || ["brander-test: " + thisBrandStr];
      var brandSrcloc = srcloc || ["brander-brand: " + thisBrandStr]
      var brander = makeObject({
          'test': makeFunction(function(obj) {
              if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(testSrcloc, 1, $a); }
              return makeBoolean(hasBrand(obj, thisBrandStr));
            }),
          'brand': makeFunction(function(obj) {
              if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(brandSrcloc, 1, $a); }
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
      if (arguments.length !== 0) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["brander"], 0, $a); }
      return namedBrander("brander", undefined);
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

    function toReprLoop(val, reprMethods) {
      var stack = [];
      var stackOfStacks = [];
      function makeCache(type) {
        var cyclicCounter = 1;
        // Note (Ben): using concat was leading to quadratic copying times and memory usage...
        return {
          add: function(elts, elt) {
            return {elt: elt, name: null, next: elts};
          },
          check: function(elts, elt) {
            var cur = elts;
            while (cur !== undefined) {
              if (cur.elt === elt) {
                if (cur.name === null) {
                  cur.name = "<cyclic-" + type + "-" + cyclicCounter++ + ">";
                }
                return cur.name;
              } else {
                cur = cur.next;
              }
            }
            return null;
          }
        };
      }
      var arrayCache = makeCache("array");
      var addNewArray = arrayCache.add;
      var findSeenArray = arrayCache.check;
      var refCache = makeCache("ref");
      var addNewRef = refCache.add;
      var findSeenRef = refCache.check;
      var objCache = makeCache("object");
      var addNewObject = objCache.add;
      var findSeenObject = objCache.check;

      function pushTodo(newArray, newObject, newRef, todo, type, extra) {
        var top = stack[stack.length - 1];
        stack.push({
          arrays: (newArray !== undefined) ? addNewArray(top.arrays, newArray) : top.arrays,
          objects: (newObject !== undefined) ? addNewObject(top.objects, newObject) : top.objects,
          refs: (newRef !== undefined) ? addNewRef(top.refs, newRef) : top.refs,
          todo: todo,
          done: [],
          type: type,
          extra: extra
        });
      }
      function toReprHelp() {
        var top;
        function finishVal(str) {
          top.todo.pop();
          top.done.push(str);
        }
        function implicitRefs(stackFrame) {
          return stackFrame.extra && stackFrame.extra.implicitRefs;
        }
        while (stack.length > 0 && stack[0].todo.length > 0) {
          top = stack[stack.length - 1];
          if (top.todo.length > 0) {
            var next = top.todo[top.todo.length - 1];
            if(isNumber(next)) { finishVal(reprMethods["number"](next)); }
            else if (isBoolean(next)) { finishVal(reprMethods["boolean"](next)); }
            else if (isNothing(next)) { finishVal(reprMethods["nothing"](next)); }
            else if (isFunction(next)) { finishVal(reprMethods["function"](next)); }
            else if (isMethod(next)) { finishVal(reprMethods["method"](next)); }
            else if (isString(next)) { finishVal(reprMethods["string"](next)); }
            else if (isOpaque(next)) { finishVal(reprMethods["opaque"](next)); }
            else if (isArray(next)) {
              // NOTE(joe): need to copy the array below because we will pop from it
              // Baffling bugs will result if next is passed directly
              var arrayHasBeenSeen = findSeenArray(top.arrays, next);
              if(typeof arrayHasBeenSeen === "string") {
                finishVal(reprMethods["cyclic"](arrayHasBeenSeen));
              }
              else {
                reprMethods["array"](next, pushTodo);
              }
            }
            else if(isRef(next)) {
              var refHasBeenSeen = findSeenRef(top.refs, next);
              var implicit = implicitRefs(top) && top.extra.implicitRefs[top.todo.length - 1];
              if(typeof refHasBeenSeen === "string") {
                finishVal(reprMethods["cyclic"](refHasBeenSeen));
              }
              else if(!isRefSet(next)) {
                finishVal(reprMethods["cyclic"]("<uninitialized-ref>"));
              }
              else {
                reprMethods["ref"](next, implicit, pushTodo);
              }
            }
            else if(isObject(next)) {
              var objHasBeenSeen = findSeenObject(top.objects, next);
              if(typeof objHasBeenSeen === "string") {
                finishVal(reprMethods["cyclic"](objHasBeenSeen));
              }
              else if (next.dict["_output"] && isMethod(next.dict["_output"])) {
                var m = getColonField(next, "_output");
                var s = m.full_meth(next);
                reprMethods["valueskeleton"](next, thisRuntime.unwrap(s), pushTodo);
              }
              else if(isDataValue(next)) {
                reprMethods["data"](next, pushTodo);
              }
              else {
                reprMethods["object"](next, pushTodo);
              }
            }
            else {
              CONSOLE.log("UNKNOWN VALUE: ", next);
              finishVal(reprMethods["string"]("<Unknown value: details logged to console>"));
            }
          }
          else {
            // Done with object, array, or ref, so pop the todo list, and pop
            // the object/array/ref itself
            stack.pop();
            var prev = stack[stack.length - 1];
            prev.todo.pop();
            prev.done.push(reprMethods[top.type](top));
          }
        }
        var finalAns = stack[0].done[0];
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
              var a = thisRuntime.unwrap($ans);
              if (ffi.isValueSkeleton(a)) {
                reprMethods["valueskeleton"](top.todo[top.todo.length - 1], a, pushTodo);
              } else {
                // this is essentially finishVal
                top.todo.pop();
                top.done.push(a);
              }
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
        var oldStack = stack;
        function getOld(name) {
          if(oldStack.length > 0) {
            return oldStack[oldStack.length - 1][name];
          }
          else {
            return undefined;
          }
        }
        try {
          if (thisRuntime.isActivationRecord(val)) {
            $step = val.step;
            $ans = val.ans;
          }
          while(true) {
            switch($step) {
            case 0:
              stackOfStacks.push(stack);
              stack = [{
                arrays: getOld("arrays"),
                objects: getOld("objects"),
                refs: getOld("refs"),
                todo: [val],
                done: [],
                extra: { implicitRefs: [false] },
                root: val
              }];
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
    function toReprJS(val, reprMethods) {
      if (isNumber(val)) { return reprMethods["number"](val); }
      else if (isBoolean(val)) { return reprMethods["boolean"](val); }
      else if (isString(val)) { return reprMethods["string"](val); }
      else { return toReprLoop(val, reprMethods); }
    }

    /**@type {PFunction} */
    var torepr = makeFunction(function(val) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["torepr"], 1, $a); }
      return makeString(toReprJS(val, ReprMethods._torepr));
    });
    var tostring = makeFunction(function(val) {
        if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["tostring"], 1, $a); }
        if(isString(val)) {
          return makeString(val);
        }
        else {
          return makeString(toReprJS(val, ReprMethods._tostring));
        }
      });

    var print = makeFunction(
    /**
      Prints the value to the world by passing the repr to stdout
      @param {!PBase} val

      @return {!PBase} the value given in
    */
       function(val){
                if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["print"], 1, $a); }

        return thisRuntime.safeCall(function() {
          display.app(val);
        }, function(_) {
          theOutsideWorld.stdout("\n");
          return val;
        });
    });

    var display = makeFunction(
    /**
      Prints the value to the world by passing the repr to stdout
      @param {!PBase} val

      @return {!PBase} the value given in
    */
       function(val){
        if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["display"], 1, $a); }
        if (isString(val)) {
          theOutsideWorld.stdout(val);
          return val;
        }
        else {
          return thisRuntime.safeCall(function() {
            return toReprJS(val, ReprMethods._tostring);
          }, function(repr) {
            theOutsideWorld.stdout(repr);
            return val;
          });
        }
    });

    var print_error = makeFunction(
    /**
      Prints the value to the world by passing the repr to stderr
      @param {!PBase} val

      @return {!PBase} the value given in
    */
       function(val){
        if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["print-error"], 1, $a); }
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
        if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["display-error"], 1, $a); }
        if (isString(val)) {
          var repr = val;
        }
        else {
          var repr = toReprJS(val, ReprMethods._tostring);
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
            if(s && hasField(s, "source")) {
              return g(s, "source") +
                   " at " +
                   g(s, "start-line") +
                   ":" +
                   g(s, "start-column")
            } else if(s && hasField(s, "module-name")) {
              return "<builtin " + g(s, "module-name") + ">";
            } else {
              return "<builtin " + s + ">";
            }
          }).join("\n") :
        "<no stack trace>";
      return toReprJS(this.exn, ReprMethods._tostring) + "\n" + stackStr;
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
      else {
        return getField(srcloc, "builtin").app(String(arr));
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

    jsnums.setThrowRuntimeError(makeMessageException);

    var raiseJSJS =
      /**
        Raises any Pyret value as an exception
        @param {!PBase} val the value to raise
      */
      function(val) {
        if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raise"], 1, $a); }
        if(thisRuntime.isObject(val) &&
          (thisRuntime.hasField(val, "render-reason")
            || thisRuntime.hasField(val, "render-fancy-reason"))){
          throw new PyretFailException(val);
        } else {
          throw new PyretFailException(thisRuntime.ffi.makeUserException(val));
        }
      };
    /** type {!PFunction} */
    // function raiseUserException(err) {
    //   ffi.throwUserException(err);
    // }

    /** type {!PFunction} */
    var hasField =
        /**
          Checks if an object has a given field
          @param {!PBase} obj The object to test
          @param {!PBase} str The field to test for, signals error if non-string
          @return {!PBase}
        */
        function(obj, str) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["has-field"], 2, $a); }
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
      checkEQ(e1);
      checkEQ(e2);
      if (ffi.isEqual(e1)) { return e2; }
      else if (ffi.isNotEqual(e1)) { return e1; }
      else if (ffi.isNotEqual(e2)) { return e2; }
      else if (ffi.isUnknown(e1)) { return e1; }
    }

    // equal3 :: PyretVal * PyretVal * Bool * (PyretNum U Undefined) * (Boolean U Undefined) -> EqualityResult

    // JS function from Pyret values to Pyret equality answers

    // Used for equal-always(3), equal-now(3), and within-(abs|rel)(3)

    // left and right are two Pyret values to compare

    // alwaysFlag is true for -always semantics (ref equality on mutables),
    // false for -now semantics (cycle/deep equality on mutables)

    // tol is the tolerance, expressed as a Pyret number (possibly an exact
    // rational, possibly a roughnum).  For non-within calls, it isn't
    // provided and is undefined.

    // rel is a flag that indicates whether the tolerance should be
    // interpreted as _absolute_ (two numbers are equal +/- tol) or _relative_
    // (two numbers are equal +/- n * tol, where tol is between 0 and 1)
    function equal3(left, right, alwaysFlag, tol, rel) {
      if(tol === undefined) { // means that we aren't doing any kind of within
        var isIdentical = identical3(left, right);
        if (!ffi.isNotEqual(isIdentical)) { return isIdentical; } // if Equal or Unknown...
      }

      var stackOfToCompare = [];
      var toCompare = { stack: [], curAns: ffi.equal };
      var cache = {left: [], right: []};
      function findPair(obj1, obj2) {
        for (var i = 0; i < cache.left.length; i++) {
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
        var current, curLeft, curRight;
        while (toCompare.stack.length > 0 && !ffi.isNotEqual(toCompare.curAns)) {
          current = toCompare.stack.pop();
          curLeft = current.left;
          curRight = current.right;

          if (ffi.isEqual(identical3(curLeft, curRight))) {
            continue;
          } else if (isNumber(curLeft) && isNumber(curRight)) {
            if (tol) {
              if (rel) {
                if (jsnums.roughlyEqualsRel(curLeft, curRight, tol)) {
                  continue;
                } else {
                  toCompare.curAns = ffi.notEqual.app(current.path, curLeft, curRight);
                }
              } else if (jsnums.roughlyEquals(curLeft, curRight, tol)) {
                continue;
              } else {
                toCompare.curAns = ffi.notEqual.app(current.path, curLeft, curRight);
              }
            } else if (jsnums.isRoughnum(curLeft) || jsnums.isRoughnum(curRight)) {
              toCompare.curAns = ffi.unknown.app("Roughnums", curLeft, curRight);
            } else if (jsnums.equals(curLeft, curRight)) {
              continue;
            } else {
              toCompare.curAns = ffi.notEqual.app(current.path, curLeft, curRight);
            }
          } else if (isNothing(curLeft) && isNothing(curRight)) {
            continue;
          } else if (isFunction(curLeft) && isFunction(curRight)) {
            toCompare.curAns = ffi.unknown.app("Functions" , curLeft ,  curRight);
          } else if (isMethod(curLeft) && isMethod(curRight)) {
            toCompare.curAns = ffi.unknown.app("Methods" , curLeft , curRight);
          } else if (isOpaque(curLeft) && isOpaque(curRight)) {
            if (curLeft.equals(curLeft.val, curRight.val)) {
              continue;
            } else {
              toCompare.curAns = ffi.notEqual.app(current.path, curLeft, curRight);
            }
          } else {
            if (findPair(curLeft, curRight)) {
              continue; // Already checked this pair of objects
            } else {
              cachePair(curLeft, curRight);
              if (isRef(curLeft) && isRef(curRight)) {
                if (alwaysFlag && !(isRefFrozen(curLeft) && isRefFrozen(curRight))) { // In equal-always, non-identical refs are not equal
                  toCompare.curAns = ffi.notEqual.app(current.path, curLeft, curRight); // We would've caught identical refs already
                } else if(!isRefSet(curLeft) || !isRefSet(curRight)) {
                  toCompare.curAns = ffi.notEqual.app(current.path, curLeft, curRight);
                } else { // In equal-now, we walk through the refs
                  var newPath = current.path;
                  var lastDot = newPath.lastIndexOf(".");
                  var lastParen = newPath.lastIndexOf(")");
                  if (lastDot > -1 && lastDot > lastParen) {
                    newPath = newPath.substr(0, lastDot) + "!" + newPath.substr(lastDot + 1);
                  } else {
                    newPath = "deref(" + newPath + ")";
                  }
                  toCompare.stack.push({
                    left: getRef(curLeft),
                    right: getRef(curRight),
                    path: newPath
                  });
                }
              } else if (isArray(curLeft) && isArray(curRight)) {
                if (alwaysFlag || (curLeft.length !== curRight.length)) {
                  toCompare.curAns = ffi.notEqual.app(current.path, curLeft, curRight);
                } else {
                  for (var i = 0; i < curLeft.length; i++) {
                    toCompare.stack.push({
                      left: curLeft[i],
                      right: curRight[i],
                      path: "raw-array-get(" + current.path + ", " + i + ")"
                    });
                  }
                }
              } else if (isObject(curLeft) && isObject(curRight)) {
                if (!sameBrands(getBrands(curLeft), getBrands(curRight))) {
                  /* Two objects with brands that differ */
                  toCompare.curAns = ffi.notEqual.app(current.path, curLeft, curRight);
                }
                else if (isObject(curLeft) && curLeft.dict["_equals"]) {
                  /* Two objects with the same brands and the left has an _equals method */
                  // If this call stack-throws,
                  var newAns = getColonField(curLeft, "_equals").full_meth(curLeft, curRight, equalFunPy);
                  // the continuation stacklet will get the result, and combine them manually
                  toCompare.curAns = combineEquality(toCompare.curAns, newAns);
                }
                else if (isDataValue(curLeft) && isDataValue(curRight)) {
                  /* Two data values with the same brands and no equals method on the left */
                  var fieldsLeft = curLeft.$app_fields_raw(function(/* varargs */) {
                    var ans = new Array(arguments.length);
                    for (var i = 0; i < arguments.length; i++) ans[i] = arguments[i];
                    return ans;
                  });
                  if (fieldsLeft.length > 0) {
                    var fieldsRight = curRight.$app_fields_raw(function(/* varargs */) {
                      var ans = new Array(arguments.length);
                      for (var i = 0; i < arguments.length; i++) ans[i] = arguments[i];
                      return ans;
                    });
                    var fieldNames = curLeft.$constructor.$fieldNames;
                    for (var k = 0; k < fieldsLeft.length; k++) {
                      toCompare.stack.push({
                        left: fieldsLeft[k],
                        right: fieldsRight[k],
                        path: current.path + "." + fieldNames[k]
                      });
                    }
                  }
                } else {
                  /* Two non-data objects with the same brands and no equals method on the left */
                  var dictLeft = curLeft.dict;
                  var dictRight = curRight.dict;
                  var fieldsLeft;
                  var fieldsRight;
                  fieldsLeft = getFields(curLeft);
                  fieldsRight = getFields(curRight);
                  if(fieldsLeft.length !== fieldsRight.length) {
                    toCompare.curAns = ffi.notEqual.app(current.path, curLeft, curRight);
                  }
                  for(var k = 0; k < fieldsLeft.length; k++) {
                    toCompare.stack.push({
                      left: curLeft.dict[fieldsLeft[k]],
                      right: curRight.dict[fieldsLeft[k]],
                      path: current.path + "." + fieldsLeft[k]
                    });
                  }
                }
              } else {
                toCompare.curAns = ffi.notEqual.app(current.path, curLeft, curRight);
              }
            }
          }
        }
        return toCompare.curAns;
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
              toCompare.curAns = combineEquality(toCompare.curAns, $ans);
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
            $step = left.step;
            $ans = left.ans;
          }
          while(true) {
            switch($step) {
            case 0:
              stackOfToCompare.push(toCompare);
              toCompare = {stack: [{left: left, right: right, path: "the-value"}], curAns: ffi.equal};
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

    function equalWithinAbsNow3(tol) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-abs-now3"], 1, $a); }
      thisRuntime.checkNumber(tol);
      if (jsnums.lessThan(tol, 0)) {
        throw makeMessageException('negative tolerance ' + tol);
      }
      return makeFunction(function(l, r) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-abs-now3(...)"], 2, $a); }
        return equal3(l, r, false, tol);
      });
    };

    function equalWithinRelNow3(relTol) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-rel-now3"], 1, $a); }
      thisRuntime.checkNumber(relTol);
      return makeFunction(function(l, r) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-rel-now3(...)"], 2, $a); }
        return equal3(l, r, false, relTol, true);
      });
    };

    function equalWithinAbs3(tol) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-abs3"], 1, $a); }
      thisRuntime.checkNumber(tol);
      if (jsnums.lessThan(tol, 0)) {
        throw makeMessageException('negative tolerance ' + tol);
      }
      return makeFunction(function(l, r) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-abs3(...)"], 2, $a); }
        return equal3(l, r, true, tol);
      });
    };

    function equalWithinRel3(relTol) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-rel3"], 1, $a); }
      thisRuntime.checkNumber(relTol);
      return makeFunction(function(l, r) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-rel3(...)"], 2, $a); }
        return equal3(l, r, true, relTol);
      });
    };

    function equalWithinAbsNow(tol) {
      return makeFunction(function(l, r) {
        return safeCall(function () {
          return equal3(l, r, false, tol);
        }, function(ans) {
          if (ffi.isEqual(ans)) { return true; }
          else if (ffi.isNotEqual(ans)) { return false; }
          else if (ffi.isUnknown(ans)) {
            ffi.throwEqualityException(getField(ans, "reason"), getField(ans, "value1"), getField(ans, "value2"));
          }
        });
      });
    };

    var equalWithinAbsNowPy = makeFunction(function(tol) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-abs-now"], 1, $a); }
      thisRuntime.checkNumber(tol);
      if (jsnums.lessThan(tol, 0)) {
        throw makeMessageException('negative toelrance ' + tol);
      }
      return makeFunction(function(l, r) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-abs-now(...)"], 2, $a); }
        return makeBoolean(equalWithinAbsNow(tol).app(l, r));
      });
    });

    function equalWithin(tol) {
      return makeFunction(function(l, r) {
        return safeCall(function () {
          return equal3(l, r, true, tol);
        }, function(ans) {
          if (ffi.isEqual(ans)) { return true; }
          else if (ffi.isNotEqual(ans)) { return false; }
          else if (ffi.isUnknown(ans)) {
            ffi.throwEqualityException(getField(ans, "reason"), getField(ans, "value1"), getField(ans, "value2"));
          }
        });
      });
    };

    var equalWithinAbsPy = makeFunction(function(tol) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-abs"], 1, $a); }
      thisRuntime.checkNumber(tol);
      if (jsnums.lessThan(tol, 0)) {
        throw makeMessageException('negative tolerance ' + tol);
      }
      return makeFunction(function(l, r) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-abs(...)"], 2, $a); }
        return makeBoolean(equalWithin(tol).app(l, r));
      });
    });

    function equalWithinRelNow(relTol) {
      return makeFunction(function(l, r) {
        return safeCall(function () {
          return equal3(l, r, false, relTol, true);
        }, function(ans) {
          if (ffi.isEqual(ans)) { return true; }
          else if (ffi.isNotEqual(ans)) { return false; }
          else if (ffi.isUnknown(ans)) {
            ffi.throwEqualityException(getField(ans, "reason"), getField(ans, "value1"), getField(ans, "value2"));
          }
        });
      });
    };

    var equalWithinRelNowPy = makeFunction(function(relTol) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-rel-now"], 1, $a); }
      thisRuntime.checkNumber(relTol);
      return makeFunction(function(l, r) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-rel-now(...)"], 2, $a); }
        return makeBoolean(equalWithinRelNow(relTol).app(l, r));
      });
    });

    function equalWithinRel(relTol) {
      return makeFunction(function(l, r) {
        return safeCall(function () {
          return equal3(l, r, true, relTol, true);
        }, function(ans) {
          if (ffi.isEqual(ans)) { return true; }
          else if (ffi.isNotEqual(ans)) { return false; }
          else if (ffi.isUnknown(ans)) {
            ffi.throwEqualityException(getField(ans, "reason"), getField(ans, "value1"), getField(ans, "value2"));
          }
        });
      });
    };

    var equalWithinRelPy = makeFunction(function(relTol) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-rel"], 1, $a); }
      thisRuntime.checkNumber(relTol);
      return makeFunction(function(l, r) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-rel(...)"], 2, $a); }
        return makeBoolean(equalWithinRel(relTol).app(l, r));
      });
    });

    // JS function from Pyret values to Pyret equality answers
    function equalAlways3(left, right) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["equal-always3"], 2, $a); }
      return equal3(left, right, true);
    };
    var eqAlwaysAns = function(ans) {
        if (ffi.isEqual(ans)) { return true; }
        else if (ffi.isNotEqual(ans)) { return false; }
        else if (ffi.isUnknown(ans)) {
          ffi.throwEqualityException(getField(ans, "reason"), getField(ans, "value1"), getField(ans, "value2"));
        }
    };
    // JS function from Pyret values to JS booleans (or throws)
    function equalAlways(v1, v2) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["equal-always"], 2, $a); }
      if(typeof v1 === "number" || typeof v1 === "string" || typeof v1 === "boolean") {
        return v1 === v2;
      }
      return safeCall(function() {
        return equal3(v1, v2, true);
      }, eqAlwaysAns);
    };
    // Pyret function from Pyret values to Pyret booleans (or throws)
    var equalAlwaysPy = makeFunction(function(left, right) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["equal-always"], 2, $a); }
        return makeBoolean(equalAlways(left, right));
    });
    // JS function from Pyret values to Pyret equality answers
    function equalNow3(left, right) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["equal-now3"], 2, $a); }
      return equal3(left, right, false);
    };
    // JS function from Pyret values to JS booleans (or throws)
    function equalNow(v1, v2) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["equal-now"], 2, $a); }
      return safeCall(function() {
        return equal3(v1, v2, false);
      }, function(ans) {
        if (ffi.isEqual(ans)) { return true; }
        else if (ffi.isNotEqual(ans)) { return false; }
        else if (ffi.isUnknown(ans)) {
          ffi.throwEqualityException(getField(ans, "reason"), getField(ans, "value1"), getField(ans, "value2"));
        }
      });
    };
    // Pyret function from Pyret values to Pyret booleans (or throws)
    var equalNowPy = makeFunction(function(left, right) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["equal-now"], 2, $a); }
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
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["same"], 2, $a); }
      return makeBoolean(same(v1, v2));
    });
    // JS function from Pyret values to Pyret booleans
    var sameJSPy = function(v1, v2) { return makeBoolean(same(v1, v2)); };

    // JS function from Pyret values to Pyret equality answers
    function identical3(v1, v2) {
      if (isFunction(v1) && isFunction(v2)) {
        return ffi.unknown.app("Functions", v1,  v2);
      } else if (isMethod(v1) && isMethod(v2)) {
        return ffi.unknown.app('Methods', v1,  v2);
      } else if (jsnums.isRoughnum(v1) && jsnums.isRoughnum(v2)) {
        return ffi.unknown.app('Roughnums', v1,  v2);
      } else if (v1 === v2) {
        return ffi.equal;
      } else {
        return ffi.notEqual.app("", v1, v2);
      }
    };
    // Pyret function from Pyret values to Pyret equality answers
    var identical3Py = makeFunction(function(v1, v2) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["identical3"], 2, $a); }
      return identical3(v1, v2);
    });
    // JS function from Pyret values to JS true/false or throws
    function identical(v1, v2) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["identical"], 2, $a); }
      var ans = identical3(v1, v2);
      if (ffi.isEqual(ans)) { return true; }
      else if (ffi.isNotEqual(ans)) { return false; }
      else if (ffi.isUnknown(ans)) {
        ffi.throwEqualityException(getField(ans, "reason"), getField(ans, "value1"), getField(ans, "value2"));
      }
    };
    // Pyret function from Pyret values to Pyret booleans (or throws)
    var identicalPy = makeFunction(function(v1, v2) {
        return makeBoolean(identical(v1, v2));
    });

    var gensymCounter = Math.floor(Math.random() * 1000);
    var gensym = makeFunction(function(base) {
        if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["gensym"], 1, $a); }
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
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["run-checks"], 2, $a); }
        return nothing;
      }),
      "check-is": makeFunction(function(left, right, loc) {
        if (arguments.length !== 4) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["check-is"], 4, $a); }
        return nothing;
      }),
      "check-satisfies": makeFunction(function(left, pred, loc) {
        if (arguments.length !== 4) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["check-satisfies"], 4, $a); }
        return nothing;
      }),
      "results": makeFunction(function() {
        if (arguments.length !== 0) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["results"], 0, $a); }
        return nothing;
      })
    });

    setParam("current-checker", nullChecker);

    /** type {!PBase} */
    var builtins = makeObject({
        'has-field': hasField,
        'current-checker': makeFunction(function() {
          if (arguments.length !== 0) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["current-checker"], 0, $a); }
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
      if(jsnums.isPyretNumber(v)) { return makeNumberBig(v); }
      else if(typeof v === "number") { return makeNumber(v); }
      else if(typeof v === "string") { return makeString(v); }
      else if(typeof v === "boolean") { return makeBoolean(v); }
      else if(isOpaque(v)) { return v; }
      else if(isObject(v)) { return v; }
      else { ffi.throwInternalError("Cannot wrap", [v]); }
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
          if(isGraphableRef(args[i])) { return checkI(i + 1); }
          else {
            return safeCheckAnnArg(locs[i], anns[i], args[i], function(ignoredArg) {
              return checkI(i + 1);
            });
          }
        }
      }
      return checkI(0);
    }

    function checkConstructorArgs2(anns, args, locs, mutMask, after) {
      function checkI(i) {
        if(i >= args.length) { return after(); }
        else {
          if(isGraphableRef(args[i]) && mutMask[i]) { return checkI(i + 1); }
          else {
            return safeCheckAnnArg(locs[i], anns[i], args[i], function(ignoredArg) {
              return checkI(i + 1);
            });
          }
        }
      }
      return checkI(0);
    }

    function checkRefAnns(obj, fields, vals, locs, exprloc, objloc) {
      if (!isObject(obj)) { ffi.throwUpdateNonObj(makeSrcloc(exprloc), obj, makeSrcloc(objloc));}
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
              ffi.throwUpdateFrozenRef(makeSrcloc(exprloc), obj, makeSrcloc(objloc), field, makeSrcloc(locs[i]));
            }
            anns[i] = getRefAnns(ref);
            refs[i] = ref;
          }
          else {
            ffi.throwUpdateNonRef(makeSrcloc(exprloc), obj, makeSrcloc(objloc), field, makeSrcloc(locs[i]));
          }
        }
        else {
          ffi.throwUpdateNonExistentField(makeSrcloc(exprloc), obj, makeSrcloc(objloc), field, makeSrcloc(locs[i]));
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

    function makePrimAnn(name, jsPred) {
      var nameC = new PPrimAnn(name, jsPred);
          // NOTE(joe): the $type$ sadness is because we only have one dynamic
          // namespace
      runtimeNamespaceBindings['$type$' + name] = nameC;
      runtimeNamespaceBindings[name] = nameC;
      thisRuntime[name] = nameC;
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
    Cont.prototype._toString = function() {
      var stack = this.stack;
      var stackStr = stack && stack.length > 0 ?
        stack.map(function(s) {
          if(!s && s.from) { return "<blank frame>"; }
          else {
            if(typeof s.from === "string") { return s; }
            else {
              return s.from.join(",");
            }
          }
        }).join("\n") : "<no stack trace>";
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
              "safeCall for " + stackFrame,
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
    // CONSOLE.log("In run2");
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
          util.suspend(iter);
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
      // CONSOLE.log("In run2::iter, GAS is ", thisRuntime.GAS);
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
              // CONSOLE.log("Setting timeout to resume iter");
              util.suspend(iter);
              return;
            }
            var next = theOneTrueStack[--theOneTrueStackHeight];
            // CONSOLE.log("ActivationRecord[" + theOneTrueStackHeight + "] = " + JSON.stringify(next, null, "  "));
            theOneTrueStack[theOneTrueStackHeight] = undefined;
            // CONSOLE.log("theOneTrueStack = ", theOneTrueStack);
            // CONSOLE.log("Setting ans to " + JSON.stringify(val, null, "  "));
            next.ans = val;
            // CONSOLE.log("GAS = ", thisRuntime.GAS);

            if (next.fun instanceof Function) {
              val = next.fun(next);
            }
            else if (!(next instanceof ActivationRecord)) {
              CONSOLE.log("Our next stack frame doesn't look right!");
              CONSOLE.log(JSON.stringify(next));
              CONSOLE.log(theOneTrueStack);
              throw false;
            }
            // CONSOLE.log("Frame returned, val = " + JSON.stringify(val, null, "  "));
          }
        } catch(e) {
          if(thisRuntime.isCont(e)) {
            // CONSOLE.log("BOUNCING");
            BOUNCES++;
            thisRuntime.GAS = initialGas;
            for(var i = e.stack.length - 1; i >= 0; i--) {
//              CONSOLE.error(e.stack[i].vars.length + " width;" + e.stack[i].vars + "; from " + e.stack[i].from + "; frame " + theOneTrueStackHeight);
              theOneTrueStack[theOneTrueStackHeight++] = e.stack[i];
            }
            // CONSOLE.log("The new stack height is ", theOneTrueStackHeight);
            // CONSOLE.log("theOneTrueStack = ", theOneTrueStack.slice(0, theOneTrueStackHeight).map(function(f) {
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
                util.suspend(iter);
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

  var TRACE_DEPTH = 0;
  var SHOW_TRACE = true;
  var TOTAL_VARS = 0;
  function traceEnter(name, vars) {
    if (!SHOW_TRACE) return;
    TRACE_DEPTH++;
    TOTAL_VARS += vars;
    CONSOLE.log("%s %s, Num vars: %d, Total vars: %d",
                Array(TRACE_DEPTH).join(" ") + "--> ",
                name, vars, TOTAL_VARS);
  }
  function traceExit(name, vars) {
    if (!SHOW_TRACE) return;
    TOTAL_VARS -= vars;
    CONSOLE.log("%s %s, Num vars: %d, Total vars: %d",
                Array(TRACE_DEPTH).join(" ") + "<-- ",
                name, vars, TOTAL_VARS);
    TRACE_DEPTH = TRACE_DEPTH > 0 ? TRACE_DEPTH - 1 : 0;
  }
  function traceErrExit(name, vars) {
    if (!SHOW_TRACE) return;
    TOTAL_VARS -= vars;
    CONSOLE.log("%s %s, Num vars: %d, Total vars: %d",
                Array(TRACE_DEPTH).join(" ") + "<XX ",
                name, vars, TOTAL_VARS);
    TRACE_DEPTH = TRACE_DEPTH > 0 ? TRACE_DEPTH - 1 : 0;
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

  // we can set verbose to true to include the <builtin> srcloc positions
  // and the "safecall for ..." internal frames
  // but by default, it's now terser
  function printPyretStack(stack, verbose) {
    if (stack === undefined) return "  undefined";
    if (!verbose) {
      stack = stack.filter(function(val) { return val instanceof Array && val.length == 7; });
    }
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
//      CONSOLE.log("Pausing stack: ", RUN_ACTIVE, new Error().stack);
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

    function runThunk(f, then) {
      thisRuntime.run(f, thisRuntime.namespace, {}, then);
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
          CONSOLE.error("Bad execThunk result: ", res);
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

    function runWhileRunning(thunk) {
      thisRuntime.pauseStack(function(restarter) {
        thisRuntime.run(function(_, __) {
            return thunk.app();
          }, thisRuntime.namespace, {
            sync: false
          }, function(result) {
            restarter.resume(result);
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
      if(DEBUGLOG) { CONSOLE.log.apply(CONSOLE, arguments); }
    }

    var plus = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["_plus"], 2, $a); }
      if (thisRuntime.isNumber(l) && thisRuntime.isNumber(r)) {
        return thisRuntime.makeNumberBig(jsnums.add(l, r));
      } else if (thisRuntime.isString(l) && thisRuntime.isString(r)) {
        return thisRuntime.makeString(l.concat(r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_plus")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_plus").app(r);
          });
      } else {
        ffi.throwNumStringBinopError(l, r, "+", "Plus", "_plus");
      }
    };

    var minus = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["_minus"], 2, $a); }
      if (thisRuntime.isNumber(l) && thisRuntime.isNumber(r)) {
        return thisRuntime.makeNumberBig(jsnums.subtract(l, r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_minus")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_minus").app(r);
          });
      } else {
        ffi.throwNumericBinopError(l, r, "-", "Minus", "_minus");
      }
    };

    var times = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["_times"], 2, $a); }
      if (thisRuntime.isNumber(l) && thisRuntime.isNumber(r)) {
        return thisRuntime.makeNumberBig(jsnums.multiply(l, r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_times")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_times").app(r);
          });
      } else {
        ffi.throwNumericBinopError(l, r, "*", "Times", "_times");
      }
    };

    var divide = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["_divide"], 2, $a); }
      if (thisRuntime.isNumber(l) && thisRuntime.isNumber(r)) {
        if (jsnums.equalsAnyZero(r)) {
          throw makeMessageException("Division by zero");
        }
        return thisRuntime.makeNumberBig(jsnums.divide(l, r));
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_divide")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_divide").app(r);
          });
      } else {
        ffi.throwNumericBinopError(l, r, "/", "Divide", "_divide");
      }
    };

    var lessthan = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["_lessthan"], 2, $a); }
      if (thisRuntime.isNumber(l) && thisRuntime.isNumber(r)) {
        return thisRuntime.makeBoolean(jsnums.lessThan(l, r));
      } else if (thisRuntime.isString(l) && thisRuntime.isString(r)) {
        return thisRuntime.makeBoolean(l < r);
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_lessthan")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_lessthan").app(r);
          });
      } else {
        ffi.throwNumStringBinopError(l, r, "<", "Less-than", "_lessthan");
      }
    };

    var greaterthan = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["_greaterthan"], 2, $a); }
      if (thisRuntime.isNumber(l) && thisRuntime.isNumber(r)) {
        return thisRuntime.makeBoolean(jsnums.greaterThan(l, r));
      } else if (thisRuntime.isString(l) && thisRuntime.isString(r)) {
        return thisRuntime.makeBoolean(l > r);
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_greaterthan")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_greaterthan").app(r);
          });
      } else {
        ffi.throwNumStringBinopError(l, r, ">", "Greater-than", "_greaterthan");
      }
    };

    var lessequal = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["_lessequal"], 2, $a); }
      if (thisRuntime.isNumber(l) && thisRuntime.isNumber(r)) {
        return thisRuntime.makeBoolean(jsnums.lessThanOrEqual(l, r));
      } else if (thisRuntime.isString(l) && thisRuntime.isString(r)) {
        return thisRuntime.makeBoolean(l <= r);
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_lessequal")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_lessequal").app(r);
          });
      } else {
        ffi.throwNumStringBinopError(l, r, "<=", "Less-than-or-equal", "_lessequal");
      }
    };

    var greaterequal = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["_greaterequal"], 2, $a); }
      if (thisRuntime.isNumber(l) && thisRuntime.isNumber(r)) {
        return thisRuntime.makeBoolean(jsnums.greaterThanOrEqual(l, r));
      } else if (thisRuntime.isString(l) && thisRuntime.isString(r)) {
        return thisRuntime.makeBoolean(l >= r);
      } else if (thisRuntime.isObject(l) && hasProperty(l.dict, "_greaterequal")) {
        return safeTail(function() {
            return thisRuntime.getField(l, "_greaterequal").app(r);
          });
      } else {
        ffi.throwNumStringBinopError(l, r, ">=", "Greater-than-or-equal", "_greaterequal");
      }
    };

    var checkArrayIndex = function(methodName, arr, ix) {
      var throwErr = function(reason) {
        ffi.throwInvalidArrayIndex(methodName, arr, ix, reason);
      };
      if(ix >= arr.length) {
        throwErr("is too large; the array length is " + arr.length);
      }
      if(ix < 0) {
        throwErr("is a negative number.");
      }
      if(!(num_is_integer(ix))) {
        throwErr("is not an integer.");
      }
    }

    var raw_array_of = function(val, len) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-of"], 2, $a); }
      thisRuntime.checkNumber(len);
      var arr = new Array(len);
      var i = 0;
      while(i < len) {
        arr[i++] = val;
      }
      return arr;
    }

    var raw_array_get = function(arr, ix) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-get"], 2, $a); }
      thisRuntime.checkArray(arr);
      thisRuntime.checkNumber(ix);
      checkArrayIndex("raw-array-get", arr, ix);
      return arr[ix];
    };

    var raw_array_set = function(arr, ix, newVal) {
      if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-set"], 3, $a); }
      thisRuntime.checkArray(arr);
      thisRuntime.checkNumber(ix);
      checkArrayIndex("raw-array-set", arr, ix);
      arr[ix] = newVal;
      return arr;
    };

    var raw_array_length = function(arr) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-length"], 1, $a); }
      thisRuntime.checkArray(arr);
      return makeNumber(arr.length);
    };

    var raw_array_to_list = function(arr) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-to-list"], 1, $a); }
      thisRuntime.checkArray(arr);
      return thisRuntime.ffi.makeList(arr);
    };

    var raw_array_constructor = function(arr) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array"], 1, $a); }
      thisRuntime.checkArray(arr);
      return arr;
    };

    var raw_array_maker = makeObject({
      make:  makeFunction(raw_array_constructor),
      make0: makeFunction(function() { return []; }),
      make1: makeFunction(function(a) { return [a]; }),
      make2: makeFunction(function(a, b) { return [a, b]; }),
      make3: makeFunction(function(a, b, c) { return [a, b, c]; }),
      make4: makeFunction(function(a, b, c, d) { return [a, b, c, d]; }),
      make5: makeFunction(function(a, b, c, d, e) { return [a, b, c, d, e]; }),
    });

    var raw_array_fold = function(f, init, arr, start) {
      if (arguments.length !== 4) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-fold"], 4, $a); }
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
      if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-substring"], 3, $a); }
      thisRuntime.checkString(s);
      thisRuntime.checkNumber(min);
      thisRuntime.checkNumber(max);
      function exactCheck(name, val) {
        if(!jsnums.isInteger(val)) {
          ffi.throwMessageException("substring: expected a positive integer for " + name + " index, but got " + String(val));
        }
      }
      exactCheck("start", min);
      exactCheck("end", max);
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
      if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-replace"], 3, $a); }
      thisRuntime.checkString(s);
      thisRuntime.checkString(find);
      thisRuntime.checkString(replace);
      return thisRuntime.makeString(s.split(find).join(replace));
    }

    var string_equals = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-equals"], 2, $a); }
      thisRuntime.checkString(l);
      thisRuntime.checkString(r);
      return thisRuntime.makeBoolean(l === r);
    }
    var string_append = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-append"], 2, $a); }
      thisRuntime.checkString(l);
      thisRuntime.checkString(r);
      return thisRuntime.makeString(l.concat(r));
    }
    var string_contains = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-contains"], 2, $a); }
      thisRuntime.checkString(l);
      thisRuntime.checkString(r);
      return thisRuntime.makeBoolean(l.indexOf(r) !== -1);
    }
    var string_length = function(s) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-length"], 1, $a); }
      thisRuntime.checkString(s);
      return thisRuntime.makeNumber(s.length);
    }
    var string_isnumber = function(s) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-isnumber"], 1, $a); }
      checkString(s);
      var num = jsnums.fromString(s);
      if(num !== false) { return true; }
      else { return false; }
    }
    var string_tonumber = function(s) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-tonumber"], 1, $a); }
      thisRuntime.checkString(s);
      var num = jsnums.fromString(s);
      if(num !== false) {
        return makeNumberBig(/**@type {Bignum}*/ (num));
      }
      else {
        return makeNothing();
      }
    }
    var string_to_number = function(s) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-to-number"], 1, $a); }
      thisRuntime.checkString(s);
      var num = jsnums.fromString(s);
      if(num !== false) {
        return ffi.makeSome(makeNumberBig(/**@type {Bignum}*/ (num)));
      }
      else {
        return ffi.makeNone();
      }
    }
    var string_repeat = function(s, n) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-repeat"], 2, $a); }
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
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-split-all"], 2, $a); }
      thisRuntime.checkString(s);
      thisRuntime.checkString(splitstr);

      return ffi.makeList(s.split(splitstr).map(thisRuntime.makeString));
    }
    var string_split = function(s, splitstr) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-split"], 2, $a); }
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
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-char-at"], 2, $a); }
      thisRuntime.checkString(s);
      thisRuntime.checkNumber(n);
      if(!jsnums.isInteger(n) || n < 0) {
        ffi.throwMessageException("string-char-at: expected a positive integer for the index, but got " + n);
      }
      if(n > (s.length - 1)) { ffi.throwMessageException("string-char-at: index " + n + " is greater than the largest index the string " + s); }

      //TODO: Handle bignums that are beyond javascript
      return thisRuntime.makeString(String(s.charAt(jsnums.toFixnum(n))));
    }
    var string_toupper = function(s) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-toupper"], 1, $a); }
      thisRuntime.checkString(s);
      return thisRuntime.makeString(s.toUpperCase());
    }
    var string_tolower = function(s) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-tolower"], 1, $a); }
      thisRuntime.checkString(s);
      return thisRuntime.makeString(s.toLowerCase());
    }
    var string_explode = function(s) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-explode"], 1, $a); }
      thisRuntime.checkString(s);
      return ffi.makeList(s.split("").map(thisRuntime.makeString));
    }
    var string_indexOf = function(s, find) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-index-of"], 2, $a); }
      thisRuntime.checkString(s);
      thisRuntime.checkString(find);
      return thisRuntime.makeNumberBig(s.indexOf(find));
    }
    var string_to_code_point = function(s) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-to-code-point"], 1, $a); }
      thisRuntime.checkString(s);
      if(s.length !== 1) {
        ffi.throwMessageException("Expected a string of length exactly one, got " + s);
      }
      var charCode = codePointAt(s, 0);
      if(!(typeof charCode === "number") || (isNaN(charCode))) {
        ffi.throwMessageException("Could not find code for character: ", s);
      }
      else {
        return charCode;
      }
    }
    var checkNatural = makeCheckType(function(val) {
        return thisRuntime.isNumber(val) && jsnums.isInteger(val) && jsnums.greaterThanOrEqual(val, 0);
      }, "Natural Number");
    var string_from_code_point = function(c) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-from-code-point"], 1, $a); }
      checkNatural(c);
      var c = jsnums.toFixnum(c);
      var ASTRAL_CUTOFF = 65535;
      if(c > ASTRAL_CUTOFF) {
        ffi.throwMessageException("Invalid code point: " + c);
      }
      try {
        var s = fromCodePoint(c);
        if(typeof s === "string") { return s; }
        else {
          ffi.throwMessageException("Invalid code point: " + c);
        }
      }
      catch(e) {
        ffi.throwMessageException("Invalid code point: " + c);
      }
    }
    var string_to_code_points = function(s) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-to-code-points"], 1, $a); }
      thisRuntime.checkString(s);
      var returnArray = [];
      for(var i = 0; i < s.length; i++) {
        var charCode = string_to_code_point(s[i]);
        returnArray[i] = charCode;
      }
      return ffi.makeList(returnArray);
    }
    var string_from_code_points = function(l) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["string-from-code-points"], 1, $a); }
      thisRuntime.checkList(l);
      var arr = ffi.toArray(l);
      var retStr = "";
      for(var i = 0; i < arr.length; i++) {
        var c = arr[i];
        var s = string_from_code_point(c);
        retStr += s;
      }
      return retStr;
    }

    var bool_not = function(l) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["not"], 1, $a); }
      thisRuntime.checkBoolean(l);
      return thisRuntime.makeBoolean(!l);
    };

    var rng = seedrandom("ahoy, world!");

    var num_random = function(max) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-random"], 1, $a); }
      checkNumber(max);
      var f = rng();
      return makeNumber(Math.floor(jsnums.toFixnum(max) * f));
    };

    var num_random_seed = function(seed) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-random-seed"], 1, $a); }
      checkNumber(seed);
      rng = seedrandom(String(seed));
      return nothing;
    }

    var num_equal = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-equals"], 2, $a); }
      thisRuntime.checkNumber(l);
      thisRuntime.checkNumber(r);
      return thisRuntime.makeBoolean(jsnums.equals(l, r));
    };

    var num_within_abs = function(delta) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within"], 1, $a); }
      thisRuntime.checkNumber(delta);
      if (jsnums.lessThan(delta, 0)) {
        throw makeMessageException('negative tolerance ' + delta);
      }
      return makeFunction(function(l, r) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["from within"], 2, $a); }
        thisRuntime.checkNumber(l);
        thisRuntime.checkNumber(r);
        return makeBoolean(jsnums.roughlyEquals(l, r, delta));
      });
    }

    var num_within_rel = function(relTol) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["within-rel"], 1, $a); }
      thisRuntime.checkNumber(relTol);
      return makeFunction(function(l, r) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["from within-rel"], 2, $a); }
        thisRuntime.checkNumber(l);
        thisRuntime.checkNumber(r);
        return makeBoolean(jsnums.roughlyEqualsRel(l, r, relTol));
      });
    }

    var num_max = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-max"], 2, $a); }
      thisRuntime.checkNumber(l);
      thisRuntime.checkNumber(r);
      if (jsnums.greaterThanOrEqual(l, r)) { return l; } else { return r; }
    }

    var num_min = function(l, r) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-min"], 2, $a); }
      thisRuntime.checkNumber(l);
      thisRuntime.checkNumber(r);
      if (jsnums.lessThanOrEqual(l, r)) { return l; } else { return r; }
    }

    var num_abs = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-abs"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.abs(n));
    }

    var num_sin = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-sin"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.sin(n));
    }
    var num_cos = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-cos"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.cos(n));
    }
    var num_tan = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-tan"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.tan(n));
    }
    var num_asin = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-asin"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.asin(n));
    }
    var num_acos = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-acos"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.acos(n));
    }
    var num_atan = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-atan"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.atan(n));
    }

    var num_modulo = function(n, mod) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-modulo"], 2, $a); }
      thisRuntime.checkNumber(n);
      thisRuntime.checkNumber(mod);
      return thisRuntime.makeNumberBig(jsnums.modulo(n, mod));
    }
    var num_truncate = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-truncate"], 1, $a); }
      thisRuntime.checkNumber(n);
      if (jsnums.greaterThanOrEqual(n, 0)) {
        return thisRuntime.makeNumberBig(jsnums.floor(n));
      } else {
        return thisRuntime.makeNumberBig(jsnums.ceiling(n));
      }
    }
    var num_sqrt = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-sqrt"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.sqrt(n));
    }
    var num_sqr = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-sqr"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.sqr(n));
    }
    var num_ceiling = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-ceiling"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.ceiling(n));
    }
    var num_floor = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-floor"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.floor(n));
    }
    var num_round = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-round"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.round(n));
    }
    var num_round_even = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-round-even"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.roundEven(n));
    }
    var num_log = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-log"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.log(n));
    }
    var num_exp = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-exp"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.exp(n));
    }
    var num_exact = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-exact"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.toRational(n));
    }
    var num_to_rational = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-to-rational"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.toRational(n));
    }
    var num_to_roughnum = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-to-roughnum"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.toRoughnum(n));
    }
    var num_to_fixnum = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-to-fixnum"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeNumberBig(jsnums.toFixnum(n));
    }
    var num_is_integer = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-is-integer"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeBoolean(jsnums.isInteger(n))
    }
    var num_is_rational = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-is-rational"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeBoolean(jsnums.isRational(n))
    }
    var num_is_roughnum = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-is-roughnum"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeBoolean(jsnums.isRoughnum(n))
    }
    var num_is_positive = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-is-positive"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeBoolean(jsnums.isPositive(n))
    }
    var num_is_negative = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-is-negative"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeBoolean(jsnums.isNegative(n))
    }
    var num_is_non_positive = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-is-non-positive"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeBoolean(jsnums.isNonPositive(n))
    }
    var num_is_non_negative = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-is-non-negative"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeBoolean(jsnums.isNonNegative(n))
    }
    var num_is_fixnum = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-is-fixnum"], 1, $a); }
      thisRuntime.checkNumber(n);
      return thisRuntime.makeBoolean(typeof n === "number");
    }
    var num_expt = function(n, pow) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-expt"], 2, $a); }
      thisRuntime.checkNumber(n);
      thisRuntime.checkNumber(pow);
      return thisRuntime.makeNumberBig(jsnums.expt(n, pow));
    }
    var num_tostring = function(n) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-tostring"], 1, $a); }
      thisRuntime.checkNumber(n);
      return makeString(String(n));
    }
    var num_tostring_digits = function(n, digits) {
      if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["num-tostring-digits"], 2, $a); }
      thisRuntime.checkNumber(n);
      thisRuntime.checkNumber(digits);
      var d = jsnums.toFixnum(digits);
      var tenDigits = jsnums.expt(10, digits);
      if (n != n) { return thisRuntime.makeString("NaN"); }
      else if (jsnums.equals(n, Number.POSITIVE_INFINITY)) { return thisRuntime.makeString("+inf"); }
      else if (jsnums.equals(n, Number.NEGATIVE_INFINITY)) { return thisRuntime.makeString("-inf"); }
      else if (jsnums.isReal(n)) {
        n = jsnums.divide(jsnums.round(jsnums.multiply(n, tenDigits)), tenDigits)
        var s = jsnums.toFixnum(n).toString().split(".");
        s[1] = (s[1] || "").substring(0, d);
        for (var i = s[1].length; i < d; i++)
          s[1] += "0";
        return thisRuntime.makeString(s[0] + "." + s[1]);
      }
    }
    function random(max) {
      if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["random"], 1, $a); }
      thisRuntime.checkNumber(max);
      return num_random(max);
    }

    var time_now = function() {
      if (arguments.length !== 0) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["time-now"], 0, $a); }
      return new Date().getTime();
    }

    function loadBuiltinModules(modules, startName, withModules) {
      function loadWorklist(startMod) {
        function addMod(curMod, curPath, curName) {
          if (curPath.filter(function(b) { return b.name === curMod.name; }).length > 0) {
            CONSOLE.error("Module cycle: ", curMod, curPath);
            throw new Error("Module cycle in loadBuiltinModules");
          }
          if (typeof curMod === "function") {
            return [{mod: {
                theModule: curMod,
                name: curName,
                dependencies: []
              },
              path: curPath
            }];
          }
          var curDeps = curMod.dependencies;
          var depMods = curDeps.map(function(d) {
            if(d.protocol === "legacy-path") {
              return { dname: d.args[0], modinfo: require(d.args[0]) };
            }
            else {
              return { dname: d.name, modinfo: require("trove/" + d.name) };
            }
          });
          var tocomp = {mod: curMod, path: curPath};
          return depMods.reduce(function(acc, elt) {
            return addMod(elt.modinfo, curPath.concat([tocomp]), elt.dname).concat(acc);
          }, [tocomp])
        }
        return addMod(startMod, []);
      }
      var wl = loadWorklist({name: startName, dependencies: modules });
      var finalModMap = {};
      function getName(d) {
        if(d.protocol === "legacy-path") {
          return d.args[0];
        }
        else {
          return d.name;
        }
      }
      var rawModules = wl.forEach(function(m) {
        if(m.mod.name === startName) { return; }
        if(m.mod.theModule.length == 2) { // Already a runtime/namespace function
          var thisRawMod = m.mod.theModule;
        }
        else {
          var rawDeps = m.mod.dependencies.map(function(d) {
            return finalModMap[getName(d)];
          });
          var thisRawMod = m.mod.theModule.apply(null, rawDeps);
        }
        finalModMap[m.mod.name] = thisRawMod;
      });
      var originalOrderRawModules = modules.map(function(m) {
        return finalModMap[getName(m)];
      });
      return loadModulesNew(thisRuntime.namespace, originalOrderRawModules, withModules);
    }

    function loadModule(module, runtime, namespace, withModule) {
      var modstring = String(module).substring(0, 500);
      return thisRuntime.safeCall(function() {
          if(typeof module === "function") {
            return module(thisRuntime, namespace);
          }
          else if (typeof module === "object") {
              if(module.dependencies === undefined) {
                // NOTE(joe): Catches already-initialized modules.  Needs to
                // be tracked down.  Putting the log back in detects them.
//                CONSOLE.error("Undefined dependencies remain: ", module);
                return module;
              }
              return loadBuiltinModules(module.dependencies, module.name,
                function(/* varargs */) {
                  var innerModule = module.theModule.apply(null, Array.prototype.slice.call(arguments));
                  return innerModule(thisRuntime, namespace);
                });
          }
          else {
            CONSOLE.log("Unkown module type: ", module);
          }
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
        var ms = new Array(arguments.length);
        for (var i = 0; i < arguments.length; i++) ms[i] = arguments[i];
        function wrapMod(m) {
          if (typeof m === 'undefined') {
            CONSOLE.error("Undefined module in this list: ", modules, String(withModules).slice(0, 500));
          }
          if (hasField(m, "provide-plus-types")) {
            return getField(m, "provide-plus-types");
          }
          else if (hasField(m, "values")) {
            return m;
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
        var ms = new Array(arguments.length);
        for (var i = 0; i < arguments.length; i++) ms[i] = arguments[i];
        return safeTail(function() {
          return withModules.apply(null, ms.map(function(m) { return getField(m, "values"); }));
        });
      });
    }

    function makeBrandPredicate(loc, brand, predName) {
      return makeFunction(function(val) {
        checkArityC(loc, 1, arguments);
        return hasBrand(val, brand);
      });
    }
    function makeVariantConstructor(
        loc,
        checkAnnsThunk,
        checkArgs,
        checkLocs,
        checkMuts,
        allArgs,
        allMuts,
        base,
        brands,
        reflName,
        reflRefFields,
        reflFields,
        constructor) {
      function quote(s) { if (typeof s === "string") { return "'" + s + "'"; } else { return s; } }
      function constArr(arr) { return "[" + arr.map(quote).join(",") + "]"; }

      function makeConstructor() {
        var argNames = constructor.$fieldNames;
        var hasRefinement = false;
        var checkAnns = checkAnnsThunk();
        checkAnns.forEach(function(a) {
          if(!isCheapAnnotation(a)) {
            hasRefinement = true;
          }
        });
        var constructorBody =
          "var dict = thisRuntime.create(base);\n";
        allArgs.forEach(function(a, i) {
          if(allMuts[i]) {
            var checkIndex = checkArgs.indexOf(a);
            if(checkIndex >= 0) {
              constructorBody += "dict['" + argNames[i] + "'] = thisRuntime.makeUnsafeSetRef(checkAnns[" + checkIndex + "], " + a + ", checkLocs[" + checkIndex + "]);\n";
            }
            else {
              constructorBody += "dict['" + argNames[i] + "'] = thisRuntime.makeUnsafeSetRef(thisRuntime.Any, " + a + ", " + constArr(loc) + ");\n";
            }
          }
          else {
            constructorBody += "dict['" + argNames[i] + "'] = " + a + ";\n";
          }
        });
        constructorBody +=
          "return thisRuntime.makeDataValue(dict, brands, " + quote(reflName) + ", reflRefFields, reflFields,"  + allArgs.length + ", " + constArr(allMuts) + ", constructor);"

        //var arityCheck = "thisRuntime.checkArityC(loc, " + allArgs.length + ", arguments);";
        var arityCheck = "var $l = arguments.length; if($l !== 1) { var $t = new Array($l); for(var $i = 0;$i < $l;++$i) { $t[$i] = arguments[$i]; } thisRuntime.checkArityC(L[7],1,$t); }";

        var checksPlusBody = "";
        if(hasRefinement) {
          checksPlusBody = "return thisRuntime.checkConstructorArgs2(checkAnns, [" + checkArgs.join(",") + "], checkLocs, " + constArr(checkMuts) + ", function() {\n" +
            constructorBody + "\n" +
          "});";
        }
        else {
          checkArgs.forEach(function(a, i) {
            if(checkMuts[i]) {
              checksPlusBody += "thisRuntime.isGraphableRef(" + checkArgs[i] + ") || thisRuntime._checkAnn(checkLocs[" + i + "], checkAnns[" + i + "], " + checkArgs[i] + ");";
            }
            else {
              checksPlusBody += "thisRuntime._checkAnn(checkLocs[" + i + "], checkAnns[" + i + "], " + checkArgs[i] + ");";
            }
          });
          checksPlusBody += constructorBody;
        }

        var constrFun = "return function(" + allArgs.join(",") + ") {\n" +
          "if(arguments.length !== " + allArgs.length + ") {\n" +
            "thisRuntime.checkArityC(" + constArr(loc) + ", " + allArgs.length + ", thisRuntime.cloneArgs.apply(null, arguments));\n" +
          "}\n" +
          checksPlusBody + "\n" +
        "}";
        //CONSOLE.log(constrFun);

        var outerArgs = ["thisRuntime", "checkAnns", "checkLocs", "brands", "reflRefFields", "reflFields", "constructor", "base"];
        var outerFun = Function.apply(null, outerArgs.concat(["\"use strict\";\n" + constrFun]));
        return outerFun(thisRuntime, checkAnns, checkLocs, brands, reflRefFields, reflFields, constructor, base);
      }

      //CONSOLE.log(String(outerFun));

      var funToReturn = makeFunction(function() {
        var theFun = makeConstructor();
        funToReturn.app = theFun;
        //CONSOLE.log("Calling constructor ", quote(reflName), arguments);
        //CONSOLE.trace();
        var res = theFun.apply(null, arguments)
        //CONSOLE.log("got ", res);
        return res;
      });
      return funToReturn;
    }

    function cloneArgs(/*arguments*/) {
      var args = new Array(arguments.length);
      for(var i = 0; i < args.length; ++i) {
                  //i is always valid index in the arguments object
          args[i] = arguments[i];
      }
      return args;
    }

    function addModuleToNamespace(namespace, valFields, typeFields, moduleObj) {
      var newns = Namespace.namespace({});
      valFields.forEach(function(vf) {
        newns = newns.set(vf, getField(getField(moduleObj, "values"), vf));
      });
      typeFields.forEach(function(tf) {
        newns = newns.setType(tf, getField(moduleObj, "types")[tf]);
      });
      return namespace.merge(newns);
    }

    var runtimeNamespaceBindings = {
          'torepr': torepr,
          'tostring': tostring,
          'test-print': print,
          'print': print,
          'display': display,
          'print-error': print_error,
          'display-error': display_error,
          'brander': brander,
          'raise': makeFunction(raiseJSJS), //raiseUserException),
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

          'num-random': makeFunction(num_random),
          'num-random-seed': makeFunction(num_random_seed),
          'num-max': makeFunction(num_max),
          'num-min': makeFunction(num_min),
          'num-equal': makeFunction(num_equal),
          'num-within-abs': makeFunction(num_within_abs),
          'num-within-rel': makeFunction(num_within_rel),
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
          'num-round': makeFunction(num_round),
          'num-round-even': makeFunction(num_round_even),
          'num-log': makeFunction(num_log),
          'num-exp': makeFunction(num_exp),
          'num-exact': makeFunction(num_exact),
          'num-to-rational': makeFunction(num_to_rational),
          'num-to-roughnum': makeFunction(num_to_roughnum),
          'num-to-fixnum': makeFunction(num_to_fixnum),
          'num-is-integer': makeFunction(num_is_integer),
          'num-is-rational': makeFunction(num_is_rational),
          'num-is-roughnum': makeFunction(num_is_roughnum),
          'num-is-positive': makeFunction(num_is_positive),
          'num-is-negative': makeFunction(num_is_negative),
          'num-is-non-positive': makeFunction(num_is_non_positive),
          'num-is-non-negative': makeFunction(num_is_non_negative),
          'num-is-fixnum': makeFunction(num_is_fixnum),
          'num-expt': makeFunction(num_expt),
          'num-tostring': makeFunction(num_tostring),
          'num-to-string': makeFunction(num_tostring),
          'num-to-string-digits': makeFunction(num_tostring_digits),

          'string-equal': makeFunction(string_equals),
          'string-contains': makeFunction(string_contains),
          'string-append': makeFunction(string_append),
          'string-length': makeFunction(string_length),
          'string-isnumber': makeFunction(string_isnumber),
          'string-tonumber': makeFunction(string_tonumber),
          'string-to-number': makeFunction(string_to_number),
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
          'string-to-code-point': makeFunction(string_to_code_point),
          'string-from-code-point': makeFunction(string_from_code_point),
          'string-to-code-points': makeFunction(string_to_code_points),
          'string-from-code-points': makeFunction(string_from_code_points),

          'time-now': makeFunction(time_now),

          'raw-array-of': makeFunction(raw_array_of),
          'raw-array-get': makeFunction(raw_array_get),
          'raw-array-set': makeFunction(raw_array_set),
          'raw-array-length': makeFunction(raw_array_length),
          'raw-array-to-list': makeFunction(raw_array_to_list),
          'raw-array-fold': makeFunction(raw_array_fold),
          'raw-array': raw_array_maker,

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

          'within-abs-now3' : makeFunction(equalWithinAbsNow3),
          'within-rel-now3' : makeFunction(equalWithinRelNow3),
          'within-abs3' : makeFunction(equalWithinAbs3),
          'within-rel3' : makeFunction(equalWithinRel3),
          'within-abs-now': equalWithinAbsNowPy,
          'within-rel-now': equalWithinRelNowPy,
          'within-abs': equalWithinAbsPy,
          'within-rel': equalWithinRelPy,

          'num-within': makeFunction(num_within_rel),
          'within-now3' : makeFunction(equalWithinRelNow3),
          'within3' : makeFunction(equalWithinRel3),
          'within-now': equalWithinRelNowPy,
          'within': equalWithinRelPy,

          'exn-unwrap': makeFunction(getExnValue)

        };

    //Export the runtime
    //String keys should be used to prevent renaming
    var thisRuntime = {
        'run': run,
        'runThunk': runThunk,
        'execThunk': execThunk,
        'safeCall': safeCall,
        'safeTail': safeTail,
        'printPyretStack': printPyretStack,

        'traceEnter': traceEnter,
        'traceExit': traceExit,
        'traceErrExit': traceErrExit,

        'isActivationRecord'   : isActivationRecord,
        'makeActivationRecord' : makeActivationRecord,

        'GAS': INITIAL_GAS,
        'INITIAL_GAS': INITIAL_GAS,

        'namedBrander': namedBrander,

        'checkAnn': checkAnn,
        '_checkAnn': _checkAnn,
        'checkAnnArgs': checkAnnArgs,
        'checkConstructorArgs': checkConstructorArgs,
        'checkConstructorArgs2': checkConstructorArgs2,
        'getDotAnn': getDotAnn,
        'makePredAnn': makePredAnn,
        'makePrimitiveAnn': makePrimitiveAnn,
        'makeBranderAnn': makeBranderAnn,
        'makeRecordAnn': makeRecordAnn,

        'makeCont'    : makeCont,
        'isCont'      : isCont,
        'makePause'   : makePause,
        'isPause'     : isPause,

        'pauseStack'  : pauseStack,
        'schedulePause'  : schedulePause,
        'breakAll' : breakAll,

        'getField'         : getField,
        'getFieldLoc'      : getFieldLoc,
        'getFieldRef'      : getFieldRef,
        'getFields'        : getFields,
        'getColonField'    : getColonField,
        'getColonFieldLoc' : getColonFieldLoc,
        'extendObj'        : extendObj,

        'hasBrand' : hasBrand,

        'isPyretTrue' : isPyretTrue,
        'isPyretFalse' : isPyretFalse,

        'isBase'      : isBase,
        'isNothing'   : isNothing,
        'isNumber'    : isNumber,
        'isRoughnum'  : jsnums.isRoughnum,
        'isString'    : isString,
        'isBoolean'   : isBoolean,
        'isFunction'  : isFunction,
        'isMethod'    : isMethod,
        'isObject'    : isObject,
        'isDataValue' : isDataValue,
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
        'makeMethod0'   : makeMethod0,
        'makeMethod1'   : makeMethod1,
        'makeMethod2'   : makeMethod2,
        'makeMethod3'   : makeMethod3,
        'makeMethod4'   : makeMethod4,
        'makeMethod5'   : makeMethod5,
        'makeMethod6'   : makeMethod6,
        'makeMethod7'   : makeMethod7,
        'makeMethod8'   : makeMethod8,
        'makeMethodN'   : makeMethodN,
        'makeMethodFromFun' : makeMethodFromFun,
        'callIfPossible0' : callIfPossible0,
        'callIfPossible1' : callIfPossible1,
        'callIfPossible2' : callIfPossible2,
        'callIfPossible3' : callIfPossible3,
        'callIfPossible4' : callIfPossible4,
        'callIfPossible5' : callIfPossible5,
        'callIfPossible6' : callIfPossible6,
        'callIfPossible7' : callIfPossible7,
        'callIfPossible8' : callIfPossible8,
        'makeObject'   : makeObject,
        'makeArray' : makeArray,
        'makeBrandedObject'   : makeBrandedObject,
        'makeGraphableRef' : makeGraphableRef,
        'makeRef' : makeRef,
        'makeUnsafeSetRef' : makeUnsafeSetRef,
        'makeVariantConstructor': makeVariantConstructor,
        'makeDataValue': makeDataValue,
        'makeMatch': makeMatch,
        'makeOpaque'   : makeOpaque,

        'derefField': derefField,

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
        'num_to_string': num_tostring,

        'string_contains': string_contains,
        'string_append': string_append,
        'string_length': string_length,
        'string_isnumber': string_isnumber,
        'string_to_number': string_to_number,
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
        'combineEquality': combineEquality,

        'within': equalWithin, //?

        'raise': raiseJSJS,

        'pyretTrue': pyretTrue,
        'pyretFalse': pyretFalse,

        'undefined': undefined,
        'create': Object.create,
        'cloneArgs': cloneArgs,

        'hasField' : hasField,

        'toReprJS' : toReprJS,
        'toRepr' : function(val) { return toReprJS(val, ReprMethods._torepr); },
        'ReprMethods' : ReprMethods,

        'wrap' : wrap,
        'unwrap' : unwrap,

        'checkWrapBoolean' : checkWrapBoolean,

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
        'checkArityC': checkArityC,
        'makeCheckType' : makeCheckType,
        'confirm'      : confirm,
        'makeMessageException'      : makeMessageException,
        'serial' : Math.random(),
        'log': log,

        'nothing': nothing,

        'makeSrcloc': makeSrcloc,
        //'_link': function(f, r) { return getField(list, "link").app(f, r); },

        'loadModule' : loadModule,
        'loadModules' : loadModules,
        'loadModulesNew' : loadModulesNew,
        'loadBuiltinModules' : loadBuiltinModules,
        'loadJSModules' : loadJSModules,

        'addModuleToNamespace' : addModuleToNamespace,

        'modules' : Object.create(null),
        'setStdout': function(newStdout) {
          theOutsideWorld.stdout = newStdout;
        },
        'getParam' : getParam,
        'setParam' : setParam,
        'hasParam' : hasParam,
        'stdout' : theOutsideWorld.stdout,
        'console' : CONSOLE
    };

    makePrimAnn("Number", isNumber);
    makePrimAnn("Exactnum", jsnums.isRational);
    makePrimAnn("Roughnum", jsnums.isRoughnum);
    makePrimAnn("NumInteger", jsnums.isInteger);
    makePrimAnn("NumRational", jsnums.isRational);
    makePrimAnn("NumPositive", jsnums.isPositive);
    makePrimAnn("NumNegative", jsnums.isNegative);
    makePrimAnn("NumNonPositive", jsnums.isNonPositive);
    makePrimAnn("NumNonNegative", jsnums.isNonNegative);
    makePrimAnn("String", isString);
    makePrimAnn("Boolean", isBoolean);
    makePrimAnn("RawArray", isArray);
    makePrimAnn("Function", function(v) { return isFunction(v) || isMethod(v) });
    makePrimAnn("Method", isMethod);
    makePrimAnn("Nothing", isNothing);
    makePrimAnn("Object", isObject);
    makePrimAnn("Any", function() { return true; });

    thisRuntime.namespace = Namespace.namespace(runtimeNamespaceBindings);

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
    loadJSModules(thisRuntime.namespace, [require("js/ffi-helpers"), require("trove/image-lib")], function(f, i) {
      ffi = f;
      thisRuntime["ffi"] = ffi;
      thisRuntime["imageLib"] = i;
    });

    // NOTE(joe): set a few of these explicitly to work with s-prim-app
    thisRuntime["throwMessageException"] = ffi.throwMessageException;
    thisRuntime["throwNoBranchesMatched"] = ffi.throwNoBranchesMatched;
    thisRuntime["throwNoCasesMatched"] = ffi.throwNoCasesMatched;

    var ns = thisRuntime.namespace;
    var nsWithList = ns;//.set("_link", getField(list, "link"))
                       //.set("_empty", getField(list, "empty"));
    thisRuntime.namespace = nsWithList;

    var checkList = makeCheckType(ffi.isList, "List");
    thisRuntime["checkList"] = checkList;

    var checkEQ = makeCheckType(ffi.isEqualityResult, "EqualityResult");

    return thisRuntime;
}

return  {'makeRuntime' : makeRuntime};

       });
