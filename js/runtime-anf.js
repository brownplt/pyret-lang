/***
This is the runtime for the ANF'd version of pyret
*/
"use strict";
/** @typedef {!Object} */
var Bignum;


define(["./namespace", "./number-dict", "./string-dict", "./boolean-dict", "./js-numbers/src/js-numbers"],
       function (Namespace, NumberDictIn, StringDictIn, BooleanDictIn, jsnumsIn) {



  //var Namespace = require('./namespace.js').Namespace;

  /**@type {{getBaseNumberDict : function(!Object) : !Object}}*/
  var NumberDict = NumberDictIn;

  /**@type {{getBaseStringDict : function(!Object) : !Object}}*/
  var StringDict = StringDictIn;

  /**@type {{getBaseBooleanDict : function(!Object) : !Object}}*/
  var BooleanDict = BooleanDictIn;


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
    /**@type {!PBase}*/
    var newObj = this.clone();
    /**@type {!boolean}*/
    var allNewFields = true;

    for(var field in fields) {
        if(allNewFields && hasOwnProperty(newObj.dict, field)) {
            allNewFields = false;
        }

        newObj.dict[field] = fields[field];
    } 
        
        newObj.brands = (allNewFields ? this.brands.slice(0) : []);

        return newObj;
}

    /**
      The base of all pyret values
      @constructor
    */
    function PBase() {
        /**@type {!Array.<number>}*/
        this.brands = [];
        /**@type {!Object.<string, !PBase>}*/
        this.dict   = makeEmptyDict();
    }

    /**@type {!Object.<string, !PBase>}*/
    PBase.prototype.dict = makeEmptyDict();
    /**@type {!Array.<number>}*/
    PBase.prototype.brands = [];
    /**@type {!function(!Object.<string, !PBase>) : !PBase}*/
    PBase.prototype.extendWith = extendWith;
    /**@type {!function() : !PBase}*/
    PBase.prototype.clone = (function() {return new PBase();});

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
inherits(PNumber, PBase);
inherits(PNothing, PBase);
inherits(PObject, PBase);
inherits(PString, PBase);
inherits(PBoolean, PBase);
inherits(PFunction, PBase);
inherits(PMethod, PBase);
inherits(POpaque, PBase);


/**
    Tests whether a JS Object has a property
    Useful for objects that lack the .hasOwnProperty method

    @param {!Object} obj the object to test
    @param {!string} p the property to look for
    @return {boolean} true if obj has property p, false otherwise
*/
function hasOwnProperty(obj, p) {
    return Object.prototype.hasOwnProperty.call(obj, p);
}

/**
    Get the brands on an object

    @param {!PBase} obj the object to get the brands of
    @return {Array.<number>}
*/
function getBrands(obj) {
  return obj.brands;
}

/**
    Get the fields in an object.

    @param {!PBase} obj the object to get the fields of
    @return {Array.<string>}

*/
function getFields(obj) {
  return Object.keys(obj.dict);
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
    var newDict = makeEmptyDict();
    for(var field in dict) {
        newDict[field] = dict[field];
    }

    return newDict;
}

/**
  @param {Array.<number>} brands
  @return Array.<number>
*/
function copyBrands(brands) {
  return brands.slice(0);
}

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
    var fieldVal = val.dict[field];
    if(fieldVal === undefined) {
        //TODO: Throw field not found error
        throw makeMessageException("field " + field + " not found.");
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
  this.brands = [];
}
POpaque.prototype = Object.create(PBase.prototype);

POpaque.prototype.extendWith = function() {
  throw makeMessageException("Cannot extend opaque values");
};
POpaque.prototype.clone = function() {
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
    this.dict   = makeEmptyDict();
    /**@type {Array.<number>}*/
    this.brands = [];
}
PNothing.prototype = Object.create(PBase.prototype);

/**Clones the nothing
  @return {!PNothing} With same dict
*/
PNothing.prototype.clone = function() { 
    var newNoth = makeNothing(); 
    newNoth.dict = copyDict(this.dict);
    newNoth.brands = copyBrands(this.brands);
    return newNoth;
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

/*********************
        Number
**********************/
/**The representation of all numerical values
    @constructor
    @param {Bignum} n the number to store in this pyret number
    @extends {PBase}
*/
function PNumber(n) { 
    /**@type {Bignum}*/
    this.n = n;

    /**@type {!Object.<string, !PBase>}*/
    this.dict = createNumberDict(); 

    /**@type {Array.<number>}*/
    this.brands = [];
}
//PNumber.prototype = Object.create(PBase.prototype); 
inherits(PNumber, PBase);

/**Clones the number
  @return {!PNumber} With same n and dict
*/
PNumber.prototype.clone = function() { 
    var newNum = makeNumberBig(this.n); 
    newNum.dict = copyDict(this.dict);
    newNum.brands = copyBrands(this.brands);
    return newNum;
};



/**Tests whether an object is a PNumber
    @param {Object} obj the item to test
    @return {boolean} true if object is a PNumber
*/
function isNumber(obj) { return obj instanceof PNumber; }

var baseNumberDict = {}; //Holder

/**Creates a copy of the common dictionary all objects have
  @return {!Object.<string, !PBase>} the dictionary for a number
*/
function createNumberDict() {
    return baseNumberDict;
}
/**Makes a PNumber using the given bignum

  @param {Bignum} n the number the PNumber will contain
  @return {!PNumber} with value n
*/
function makeNumberBig(n) {
   return new PNumber(n); 
}

/**Makes a PNumber using the given JSNum

  @param {number} n the number the PNumber will contain
  @return {!PNumber} with value n
*/
function makeNumber(n) {
   return new PNumber(jsnums.fromFixnum(n)); 
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
   return new PNumber(/**@type {Bignum}*/ (result)); 
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

    /**@type {!Object.<string, !PBase>}*/
    this.dict = createStringDict(); 

    /**@type {Array.<number>}*/
    this.brands = [];
}
//PString.prototype = Object.create(PBase.prototype); 

/**Clones the string
  @return {!PString} With same n and dict
*/
PString.prototype.clone = function() { 
    var newStr = makeString(this.s); 
    newStr.dict = copyDict(this.dict);
    newStr.brands = copyBrands(this.brands);
    return newStr;
};


/**Tests whether an object is a PString
    @param {Object} obj the item to test
    @return {boolean} true if object is a PString
*/
function isString(obj) { return obj instanceof PString; }

/**@type !Object.<string, !PBase>*/
var baseStringDict = {}; //Holder

/**Creates a copy of the common dictionary all objects have
  @return {!Object.<string, !PBase>} the dictionary for a number
*/
function createStringDict() {
    return baseStringDict;
}

/**Makes a PString using the given s

  @param {string} s the string the PString will contain
  @return {!PString} with value s
*/
function makeString(s) {
  if(typeof s !== "string") { throw Error("Non-string given to makeString"); }
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

    /**@type {!Object.<string, !PBase>}*/
    this.dict = createBooleanDict(); 

    /**@type {Array.<number>}*/
    this.brands = [];
}
//PBoolean.prototype = Object.create(PBase.prototype); 

/**Clones the Boolean
  @return {!PBoolean} With same b and dict
*/
PBoolean.prototype.clone = function() { 
    var newBool = new PBoolean(this.b); 
    newBool.dict = copyDict(this.dict);
    newBool.brands = copyBrands(this.brands);
    return newBool;
};

//The inherit methods on all booleans
/**@type !Object.<string, !PBase>*/
var baseBooleanDict = {}; //Holder

/**Creates a copy of the common dictionary all boolean have
  @return {!Object.<string, !PBase>} the dictionary for a boolean
*/
function createBooleanDict() {
    return baseBooleanDict;
}

/**Tests whether an object is a PBoolean
    @param {Object} obj the item to test
    @return {boolean} true if object is a PBoolean
*/
function isBoolean(obj) { return obj instanceof PBoolean; }


//Boolean Singletons
var pyretTrue =  null;//new PBoolean(true);
var pyretFalse = null; //new PBoolean(false);

/**Makes a PBoolean using the given s

  @param {boolean} b the Boolean the PBoolean will contain
  @return {!PBoolean} with value b
*/
function makeBoolean(b) {
    return (b ? pyretTrue : pyretFalse);
}


/**Tests whether the boolean is equal to the singleton true value

  @param {PBoolean} b
  @return {boolean}
*/
function isPyretTrue(b) {
    return b === pyretTrue;
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

    /**@type {Array.<number>}*/
    this.brands = [];
}
//PFunction.prototype = Object.create(PBase.prototype); 

/**Clones the function
  @return {!PFunction} With same app and dict
*/
PFunction.prototype.clone = function() { 
    var newFun = makeFunction(this.app); 
    newFun.dict = copyDict(this.dict);
    newFun.brands = copyBrands(this.brands);
    return newFun;
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
    return makeEmptyDict();
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

    /**@type {Array.<number>}*/
    this.brands = [];

}
//PMethod.prototype = Object.create(PBase.prototype); 

/**Clones the method
  @return {!PMethod} With same meth and dict
*/
PMethod.prototype.clone = function() { 
    var newMeth = makeMethod(this['meth'], this['full_meth']); 
    newMeth.dict = copyDict(this.dict);
    newMeth.brands = copyBrands(this.brands);
    return newMeth;
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
    return makeEmptyDict();
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
    function PObject(dict) { 
        /**@type {!Object.<string, !PBase>}*/
        this.dict = copyDict(dict); //Copies the dict to ensure the proto is null

        /**@type {Array.<number>}*/
        this.brands = [];
    }
    //PObject.prototype = Object.create(PBase.prototype); 

    /**Clones the object
      @return {!PObject} With same dict
    */
    PObject.prototype.clone = function() { 
        var newObj = makeObject({}); 
        newObj.dict = copyDict(this.dict);
        newObj.brands = copyBrands(this.brands);
        return newObj;
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
       return new PObject(dict); 
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
            throw makeMessageException("Pyret Type Error: " + test)
        }
        return true;
    }


    /************************
       Builtin Functions
    ************************/

    function hasBrand(obj, brand) {
      return obj.brands.indexOf(brand) !== -1;
    }

    var brandCounter = 0;
    /**@type {PFunction} */
    var brander = makeFunction(
    /**
      @return {!PBase}
    */
    function() {
      var thisBrand = brandCounter++;
      return makeObject({
          'test': makeFunction(function(obj) {
              return makeBoolean(hasBrand(obj, thisBrand));
            }),
          'brand': makeFunction(function(obj) {
              var newObj = obj.clone();
              newObj.brands.push(thisBrand);
              return newObj;
            })
        });
    }
    );

    /**
      Creates the js string representation for the value
      @param {!PBase} val

      @return {!string} the value given in
    */
    function toReprJS(val) {
      var str = '';
      if (isNumber(val)) {
        str = String(/**@type {!PNumber}*/ (val).n);
      } else if (isBoolean(val)) {
        str = String(/**@type {!PBoolean}*/ (val).b);
      } else if (isString(val)) {
        str = String(/**@type {!PString}*/ (val).s);
        str = '"' + str + '"';
      } else if (isObject(val)) {
        if (val.dict._torepr) {
          return getField(val, "_torepr").app().s;
        }
        //todo: invoke a tostring if exists
        str = "";
        var toprint = [];
        toprint.push(0);
        for(var field in val.dict){
            toprint.push({name : field, value : val.dict[field]});
            toprint.push(2);
        }
        if(toprint.length > 1) {toprint.pop();};
        toprint.push(1);

        while(toprint.length !== 0) {
            var next = toprint.shift();
            if(next === 0) {
                str += "{";
            }
            else if(next === 1) {
                str += "}";
            }
            else if(next === 2) {
                str += ", ";
            }
            else if(isObject(next.value)) {
                str += next.name + ": "; 
                toprint.unshift(1);
                for(var field in next.value.dict){
                    toprint.unshift({name : field, value : next.value.dict[field]});
                    toprint.unshift(2);
                }
                if(Object.keys(next.value.dict).length > 0) {
                    toprint.shift(); //Remove extra comma token
                }
                toprint.unshift(0);
            }
            else {
                str += next.name + ": " + toReprJS(next.value);
            }
        }
      } else {
        str = String(val);
      }

      return str;
    };

    /**@type {PFunction} */
    var torepr = makeFunction(function(val) {return makeString(toReprJS(val));});

    var print = makeFunction(
    /**
      Prints the value to the world by passing the repr to stdout
      @param {!PBase} val

      @return {!PBase} the value given in
    */
       function(val){
        if (isString(val)) {
          var repr = val.s;
        }
        else {
          var repr = toReprJS(val);
        }
        theOutsideWorld.stdout(repr + "\n");
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

    /**
      Raises a PyretFailException with the given string
      @param {!string} str
      @return {!PyretFailException}
    */
    function makeMessageException(str) {
       return new PyretFailException(makeString(str));
    }

    /** type {!PFunction} */
    var raise = makeFunction(
      /**
        Raises any Pyret value as an exception
        @param {!PBase} val the value to raise
      */
      function(val) {
        throw new PyretFailException(val);
      }
    );

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
          return makeBoolean(hasOwnProperty(obj.dict, str.s));
        }
      );

    function same(left, right) {
      if (left === right) { return true; }

      if (isNumber(left) && isNumber(right)) {
        return jsnums.equals(left.n, right.n);
      }
      else if (isString(left) && isString(right)) {
        return left.s === right.s;
      }
      else if (isBoolean(left) && isBoolean(right)) {
        return left.b === right.b;
      }
      else if (isFunction(left) && isFunction(right)) {
        return left === right;
      }
      else if (isMethod(left) && isMethod(right)) {
        return left === right;
      }
      else if (isObject(left) && isObject(right)) {
        var brands1 = getBrands(left);
        var brands2 = getBrands(right);
        for(var i = 0; i < brands1.length; i++) {
          if (!hasBrand(right, brands1[i])) { return false; }
        }
        for(var j = 0; i < brands2.length; i++) {
          if (!hasBrand(left, brands2[i])) { return false; }
        }

        var fields1 = getFields(left);
        var fields2 = getFields(right);
        if(fields1.length !== fields2.length) { return false; }
        for(var k = 0; k < fields1.length; k++) {
          if(!same(left.dict[fields1[k]], right.dict[fields1[k]])) {
            return false;
          }
        }
        return true;
      }
      else if (isOpaque(left) && isOpaque(right)) {
        return left.equals(left.val, right.val);
      }
      else {
        throw makeMessageException("Cannot compare " + left + " " + right + " with ==");
      }
    };
    var sameP = makeFunction(function(v1, v2) { return makeBoolean(same(v1, v2)); });

    var gensymCounter = Math.floor(Math.random() * 1000);
    var gensym = makeFunction(function(base) {
        checkIf(base, isString);
        return RUNTIME.makeString(unwrap(base) + String(gensymCounter++))
      });

    /** type {!PBase} */
    var builtins = makeObject({
        'has-field': hasField,
        'equiv': sameP
      });


    function unwrap(v) {
      if(isNumber(v)) { return v.n; }
      else if(isString(v)) { return v.s; }
      else if(isBoolean(v)) { return v.b; }
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
    function SuccessResult(r) {
      this.result = r;
    }

    /**
      Tests if result is a successResult
      @param {Object} val the value to test
      @return {boolean} true if it is a SuccessResult
    */
    function isSuccessResult(val) { return val instanceof SuccessResult; }

    /**
      Result containing the exception of a failed evaluation

      @constructor
      @param {!Error} e exception's value
    */
    function FailureResult(e) {
      this.exn = e;
    }
    /**
      Tests if result is a FailueResult
      @param {Object} val the value to test
      @return {boolean} true if it is a FailueResult
    */
    function isFailureResult(val) { return val instanceof FailureResult; }

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


    /**@type {function(function(Object, Object) : !PBase, Object, function(Object))}*/
    function run(program, namespace, onDone) {
      var kickoff = {
          go: function(ignored) {
            return program(thisRuntime, namespace);
          },
          captureExn: function(e) {
            e.pyretStack.push(theOneTrueStackTop);
          }
        };
      var theOneTrueStack = [kickoff];
      var theOneTrueStart = {};
      var val = theOneTrueStart;
      var theOneTrueStackTop = {}
      var theOneTrueStackHeight = 1;
      var BOUNCES = 0;

      function iter() {
        var loop = true;
        while (loop) {
          loop = false;
          try {
            while(theOneTrueStackHeight > 0) {
              var next = theOneTrueStack[--theOneTrueStackHeight];
              theOneTrueStack[theOneTrueStackHeight] = undefined;
              val = next.go(val);
            }
            onDone(new SuccessResult(val));
          } catch(e) {
            if(isCont(e)) {
              BOUNCES++;
              thisRuntime.GAS = INITIAL_GAS;
              for(var i = e.stack.length - 1; i >= 0; i--) {
                theOneTrueStack[theOneTrueStackHeight++] = e.stack[i];
              }

              theOneTrueStack[theOneTrueStackHeight++] = e.bottom;
              val = theOneTrueStart;
              loop = true;
              //            iter();
              //            setTimeout(iter, 0);
            }
            else if(isPyretException(e)) {
              while(theOneTrueStackHeight > 0) {
                var next = theOneTrueStack[--theOneTrueStackHeight];
                theOneTrueStack[theOneTrueStackHeight] = undefined;
                next.captureExn(e);
              }
              onDone(new FailureResult(e));
            } else {
              onDone(new FailureResult(e));
            }
          }
        }
      }
      thisRuntime.GAS = INITIAL_GAS;
      setTimeout(iter, 0);
    }

    var INITIAL_GAS = theOutsideWorld.initialGas || 1000;

    //Export the runtime
    //String keys should be used to prevent renaming
    var thisRuntime = {
        'namespace': Namespace({
          'torepr': torepr,
          'tostring': torepr,
          'test-print': print,
          'print': print,
          'brander': brander,
          'raise': raise,
          'builtins': builtins,
          'nothing': makeNothing(),
          'is-nothing': mkPred(isNothing),
          'is-number': mkPred(isNumber),
          'is-boolean': mkPred(isNumber),
          'is-string': mkPred(isString),
          'is-function': mkPred(isFunction),
          'is-object': mkPred(isObject),
          'gensym': gensym
        }),
        'run': run,

        'GAS': 0,

        'makeCont'    : makeCont,
        'isCont'      : isCont,

        'getField'    : getField,
        'getFields'    : getFields,
        'getColonField'    : getColonField,

        'isPyretTrue' : isPyretTrue,

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
        'isFailureResult' : isFailureResult,
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
        'makeOpaque'   : makeOpaque,

        'toReprJS' : toReprJS,

        'same' : same,
        'wrap' : wrap,
        'unwrap' : unwrap,

        'checkIf'      : checkIf,
        'makeMessageException'      : makeMessageException,
        'serial' : Math.random()
    };

    //Create the dictionaries 
    //Note: Order is important
    baseNumberDict = NumberDict.getBaseNumberDict(thisRuntime);
    baseStringDict = StringDict.getBaseStringDict(thisRuntime);
    baseBooleanDict = BooleanDict.getBaseBooleanDict(thisRuntime);

    //Boolean Singletons, creating now that boolean dict exists
    //Todo: Ensure no one has any copies of the old ones
    pyretTrue = new PBoolean(true);
    pyretFalse = new PBoolean(false);
    
    thisRuntime['pyretTrue'] = pyretTrue;
    thisRuntime['pyretFalse'] = pyretFalse;

    return thisRuntime;
}

return  {'makeRuntime' : makeRuntime};

       });
