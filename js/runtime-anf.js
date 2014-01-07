/***
This is the runtime for the ANF'd version of pyret
*/
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

    /*
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

    /**Tests whether an object is a PBase
        @param {Object} obj the item to test
        @return {boolean} true if object is a PBase
    */
    function isBase(obj) { return obj instanceof PBase; }


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
        var newNum = new PNumber(this.n); 
        newNum.dict = this.dict;
        return newNum;
    };


    /**Tests whether an object is a PBase
        @param {Object} obj the item to test
        @return {boolean} true if object is a PBase
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

    //Test for type Checking
    var a = makeNumber("HELLO");

    //Export the runtime
    //String keys should be used to prevent renaming
    return {
        'isBase'     : isBase,
        'isNumber'   : isNumber,

        'makeNumber' : makeNumber
    };
}

return  {'makeRuntime' : makeRuntime};
})();
