/***
This is the runtime for the ANF'd version of pyret
*/
if(typeof require !== 'undefined') {
  var Namespace = require('./namespace.js').Namespace;
  var jsnums = require('./js-numbers/src/js-numbers.js');
}
"use strict";

/**
  @type {{makeRuntime : function(*)}}
*/
var PYRET_ANF = (function() {

/**
    Creates a Pyret runtime
    @param {{stdout : function(string)}} theOutsideWorld contains the hooks into the environment

    @return {Object} that contains all the necessary components of a runtime
*/
function makeRuntime(theOutsideWorld) {
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
    

    /**
        Extends an object with the new fields in fields
        If all the fields are new, the brands are kept,
        otherwise, the extended object has no brands

        The original object is not mutated, instead it is cloned and the clone is mutated

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
            throw makeMessageException("field " + val + " not found.");
        }
        /*else if(isMutable(fieldVal)){
            //TODO: Implement mutables then throw an error here
        }*/
        /*else if(isPlaceholder(fieldVal)){
            //TODO: Implement placeholders then call get here
            //Be wary of guards blowing up stack
        }*/
        else if(isMethod(fieldVal)){
            //TODO: Bind self properly
            return makeFunction((fieldVal).meth);
        }
        else {
            return fieldVal;
        }
    }

    
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
    
    /**Clones the number
      @return {!PNothing} With same dict
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
       @return {!PNothing}
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
        var newNum = makeNumber(this.n); 
        newNum.dict = copyDict(this.dict);
        return newNum;
    };


    /**Tests whether an object is a PNumber
        @param {Object} obj the item to test
        @return {boolean} true if object is a PNumber
    */
    function isNumber(obj) { return obj instanceof PNumber; }

    var baseNumberDict = {
        /**@type {PMethod}*/
     // '_plus' : mkmethod(left, right) { 
     // 
     // //TODO: Remove blah
     // /**
     //   @param {!PNumber} left
     //   @param {!PNumber} right
     //   @return {!PNumber}
     // */
     //     checkIf(left, isNumber);
     //     checkIf(right, isNumber);
     //     return makeNumber(left.n + right.n);
     // } {

     //     checkIf(left, isNumber);
     //     checkIf(right, isNumber);
     //     return makeNumber(left.n + right.n);
     // },
        

        /**@type {PMethod}*/
        '_minus' : makeMethod(
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PNumber}
        */
        function(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeNumberBig(jsnums.subtract(left.n, right.n));
        }), 

        /**@type {PMethod}*/
        '_times' : makeMethod(
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PNumber}
        */
        function(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeNumberBig((left.n * right.n));
        }),

        /**@type {PMethod}*/
        '_divide' : makeMethod(
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PNumber}
        */
        function(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);
            if(right.n === 0) {
                throw makeMessageException("Division by zero");
            }
            return makeNumber(left.n / right.n);
        }),

        /**@type {PMethod}*/
        '_equals' : makeMethod(
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PBoolean}
        */
        function(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(left.n === right.n);
        }),

        /**@type {PMethod}*/
        '_lessthan' : makeMethod(
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PBoolean}
        */
        function(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(left.n < right.n);
        }),

        /**@type {PMethod}*/
        '_greaterthan' : makeMethod(
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PBoolean}
        */
        function(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(left.n > right.n);
        }),
        /**@type {PMethod}*/
        '_lessequal' : makeMethod(
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PBoolean}
        */
        function(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(left.n <= right.n);
        }),
        /**@type {PMethod}*/
        '_greaterequal' : makeMethod(
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PBoolean}
        */
        function(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(left.n >= right.n);
        }),

        /**@type {PMethod}*/
        'tostring' : makeMethod(
        /**
          @param {!PNumber} me
          @return {!PString}
        */
        function(me) {
            checkIf(me, isNumber);
            return makeString(me.n.toString());
        }),

        /**@type {PMethod}*/
        'sin' : makeMethod(
        /**
          @param {!PNumber} me
          @return {!PNumber}
        */
        function(me) {
            checkIf(me, isNumber);
            return makeNumber(Math.sin(me.n));
        }),

        /**@type {PMethod}*/
        'cos' : makeMethod(
        /**
          @param {!PNumber} me
          @return {!PNumber}
        */
        function(me) {
            checkIf(me, isNumber);
            return makeNumber(Math.cos(me.n));
        }),

        /**@type {PMethod}*/
        'tan' : makeMethod(
        /**
          @param {!PNumber} me
          @return {!PNumber}
        */
        function(me) {
            checkIf(me, isNumber);
            return makeNumber(Math.tan(me.n));
        }),

        /**@type {PMethod}*/
        'asin' : makeMethod(
        /**
          @param {!PNumber} me
          @return {!PNumber}
        */
        function(me) {
            checkIf(me, isNumber);
            return makeNumber(Math.asin(me.n));
        }),

        /**@type {PMethod}*/
        'acos' : makeMethod(
        /**
          @param {!PNumber} me
          @return {!PNumber}
        */
        function(me) {
            checkIf(me, isNumber);
            return makeNumber(Math.acos(me.n));
        }),

        /**@type {PMethod}*/
        'atan' : makeMethod(
        /**
          @param {!PNumber} me
          @return {!PNumber}
        */
        function(me) {
            checkIf(me, isNumber);
            return makeNumber(Math.atan(me.n));
        })

    };

    /**Creates a copy of the common dictionary all objects have
      @return {!Object.<string, !PBase>} the dictionary for a number
    */
    function createNumberDict() {
        return copyDict(baseNumberDict);
    }
    /**Makes a PNumber using the given n

      @param {number} n the number the PNumber will contain
      @return {!PNumber} with value n
    */
    function makeNumberBig(n) {
       return new PNumber(n); 
    }

    function makeNumber(n) {
       return new PNumber(jsnums.fromFixnum(n)); 
    }
    //TODO: for BIG numbers, we'llneed to compile them in as strings and use jsnums.fromString(_) to get the value

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
        return newStr;
    };


    /**Tests whether an object is a PString
        @param {Object} obj the item to test
        @return {boolean} true if object is a PString
    */
    function isString(obj) { return obj instanceof PString; }

    var baseStringDict = {

        /**@type {PMethod}*/
        '_plus' : makeMethod(
        /**
          @param {!PString} left
          @param {!PString} right
          @return {!PString}
        */
        function(left, right) {
            checkIf(left, isString);
            checkIf(right, isString);

            return makeString(left.s.concat(right.s));
        }),

        /**@type {PMethod}*/
        'append' : makeMethod(
        /**
          @param {!PString} left
          @param {!PString} right
          @return {!PString}
        */
        function(left, right) {
            checkIf(left, isString);
            checkIf(right, isString);

            return makeString(left.s.concat(right.s));
        }),

        /**@type {PMethod}*/
        'contains' : makeMethod(
        /**
          @param {!PString} left
          @param {!PString} right
          @return {!PBoolean}
        */
        function(left, right) {
            checkIf(left, isString);
            checkIf(right, isString);

            return makeBoolean(left.s.indexOf(right.s) !== -1);
        }),

        /**@type {PMethod}*/
        'char-at' : makeMethod(
        /**
          @param {!PString} str
          @param {!PNumber} n
          @return {!PString}
        */
        function(str, n) {
            checkIf(str, isString);
            checkIf(n, isNumber);

            return makeString(String(str.s.charAt(n.n)));
        }),


        /**@type {PMethod}*/
        'tostring' : makeMethod(
        /**
          @param {!PString} me
          @return {!PString}
        */
        function(me) {
            checkIf(me, isString);
            return makeString(me.s);
        }),

        /**@type {PMethod}*/
        'to-upper' : makeMethod(
        /**
          @param {!PString} me
          @return {!PString}
        */
        function(me) {
            checkIf(me, isString);
            return makeString(me.s.toUpperCase());
        }),

        /**@type {PMethod}*/
        'to-lower' : makeMethod(
        /**
          @param {!PString} me
          @return {!PString}
        */
        function(me) {
            checkIf(me, isString);
            return makeString(me.s.toLowerCase());
        }),

        /**@type {PMethod}*/
        'tonumber' : makeMethod(
        /**
          @param {!PString} me
          @return {!PNumber | !PNothing}
        */
        function(me) {
            checkIf(me, isString);
            var num = jsnums.fromString(me.s);
            if(num !== false) {
                return makeNumberBig(num);
            }
            else {
                return makeNothing();
            }
        }),

        /**@type {PMethod}*/
        'length' : makeMethod(
        /**
          @param {!PString} me
          @return {!PNumber}
        */
        function(me) {
            checkIf(me, isString);
            return makeNumber(me.s.length);
        }), 

        /**@type {PMethod}*/
        '_lessthan' : makeMethod(
        /**
          @param {!PString} left
          @param {!PString} right
          @return {!PBoolean}
        */
        function(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(left.s < right.s);
        }),

        /**@type {PMethod}*/
        '_greaterthan' : makeMethod(
        /**
          @param {!PString} left
          @param {!PString} right
          @return {!PBoolean}
        */
        function(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(left.s > right.s);
        }),
        /**@type {PMethod}*/
        '_lessequal' : makeMethod(
        /**
          @param {!PString} left
          @param {!PString} right
          @return {!PBoolean}
        */
        function(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(left.s <= right.s);
        }),
        /**@type {PMethod}*/
        '_greaterequal' : makeMethod(
        /**
          @param {!PString} left
          @param {!PString} right
          @return {!PBoolean}
        */
        function(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(left.s >= right.s);
        })
    }

    /**Creates a copy of the common dictionary all objects have
      @return {!Object} the dictionary for a number
    */
    function createStringDict() {
        return copyDict(baseStringDict);
    }

    /**Makes a PString using the given s

      @param {string} s the string the PString will contain
      @return {!PString} with value s
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
        var newBool = makeBoolean(this.b); 
        newBool.dict = copyDict(this.dict);
        return newBool;
    };


    /**Creates a copy of the common dictionary all boolean have
      @return {!Object} the dictionary for a boolean
    */
    function createBooleanDict() {
        return makeEmptyDict();
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
        return makeEmptyDict();
    }

    /**Makes a PBoolean using the given s

      @param {boolean} b the Boolean the PBoolean will contain
      @return {!PBoolean} with value b
    */
    function makeBoolean(b) {
       return new PBoolean(b); 
    }

    /*********************
            Function
    **********************/

    /**The representation of a function
        @constructor
        @extends {PBase}

        @param {Function} fun the function body
    */
    function PFunction(fun) { 
        /**@type {Function}*/
        this.app   = fun;

        /**@type {number}*/
        this.arity = fun.length;

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
        return newFun;
    };

    /**Tests whether an object is a PFunction
        @param {Object} obj the item to test
        @return {boolean} true if object is a PFunction
    */
    function isFunction(obj) { return obj instanceof PFunction; }

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
       return new PFunction(fun); 
    }


    /*********************
            Method
    **********************/

    /**The representation of a method
        @constructor
        @param {Function} meth 
        @extends {PBase}
    */
    function PMethod(meth) { 
        /**@type {Function}*/
        this.meth   = meth;
        /**@type {number}*/
        this.arity = meth.length;

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
      @return {!Object.<string, !PBase>} the dictionary for a method
    */
    function createMethodDict() {
        return makeEmptyDict();
    }

    /**Makes a PMethod using the given function
      The function first argument should be self

      @param {Function} meth The JS function that represents the body of the method
      @return {!PMethod} with app of fun
    */
    function makeMethod(meth) {
       return new PMethod(meth); 
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

    /**Clones the method
      @return {!PObject} With same dict
    */
    PObject.prototype.clone = function() { 
        var newObj = makeObject({}); 
        newObj.dict = copyDict(this.dict);
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
            throw makeMessageException("Pyret Type Error")
        }
        return true;
    }


    /************************
       Builtin Functions
    ************************/

    /**@type {PFunction} */
    var print = makeFunction(
    /**
      Prints the value to the world by passing the repr to stdout
      @param {!PBase} val

      @return {!PBase} the value given in
    */
    function(val) {
      var str = '';
      if (isNumber(val)) {
        str = String(/**@type {!PNumber}*/ (val).n);
      } else {
        str = String(val);
      }
      theOutsideWorld.stdout(str + "\n");

      //Returns the value it is given
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

    //Export the runtime
    //String keys should be used to prevent renaming
    var thisRuntime = {
        'namespace': Namespace({
          'test-print': print
        }),

        /**@type {function(function(Object, Object) : !PBase, Object, function(Object))}*/
        'run': function(program, namespace, onDone) {
          try {
            onDone(new SuccessResult(program(thisRuntime, namespace)));
          } catch(e) {
            onDone(new FailureResult(e));
          }
        },

        'isBase'      : isBase,
        'isNothing'   : isNothing,
        'isNumber'    : isNumber,
        'isString'    : isString,
        'isBoolean'   : isBoolean,
        'isFunction'  : isFunction,
        'isMethod'    : isMethod,
        'isObject'    : isObject,

        'isSuccessResult' : isSuccessResult,
        'isFailureResult' : isFailureResult,
        'isPyretException' : isPyretException,

        'makeNothing'  : makeNothing,
        'makeNumber'   : makeNumber,
        'makeBoolean'  : makeBoolean,
        'makeString'   : makeString,
        'makeFunction' : makeFunction,
        'makeMethod'   : makeMethod,
        'makeObject'   : makeObject
    };
    return thisRuntime;
}

return  {'makeRuntime' : makeRuntime};
})();

if (typeof exports !== 'undefined') {
  exports['PYRET_ANF'] = PYRET_ANF;
}

