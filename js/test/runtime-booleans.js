_ = require('jasmine-node');
var path = require('path');
var addPyretMatchers = require(path.join(__dirname,'./matchers.js')).addPyretMatchers;



function performTest(useCompiled){

if(useCompiled) {
    R = require('../anf-comp.js').PYRET_ANF;
}
else {
    R = require('../runtime-anf.js').PYRET_ANF;
}
var jsnums = require('../js-numbers/src/js-numbers.js');

var output;
var rt;

/**@ param {string} str, output*/
function stdout(str) {
    output += str;
}

//Booleans for testing
var pTrue,
    pFalse,
    pTrueExt,
    pFalseExt,
    pNum,
//Thunks for testing
    thunkTrue,
    thunkFalse,
    thunkNum;


beforeEach(function(){
    addPyretMatchers(this);
    output = "";
    rt = R.makeRuntime({'stdout' : stdout});

   //Make Test Values

    pTrue = rt.makeBoolean(true);
    pFalse = rt.makeBoolean(false);
    pTrueExt = pTrue.extendWith({x : rt.makeNumber(32)});
    pFalseExt = pFalse.extendWith({y : rt.makeNumber(32)});
    pNum = rt.makeNumber(5);

    thunkTrue = rt.makeFunction(function() { return rt.makeBoolean(true); });
    thunkFalse = rt.makeFunction(function() { return rt.makeBoolean(false); });
    thunkNum  = rt.makeFunction(function() { return rt.makeNumber(3); });
});

  function checkForPyretError(thunk) {
    try{ 
        thunk();
    }
    catch(e) {
        expect(rt.isPyretException(e)).toBe(true);
        return;
    }

    throw new Error("Expression did not result in error");
  }

  describe("Boolean Dictionary", function() {
    it("should have correct _and", function() {

        var _and = rt.getColonField(pTrue,'_and');
        expect(_and).not.toBeUndefined();
        expect(rt.isMethod(_and)).toEqual(true);
        var and = _and.full_meth;
        
        //Base
        expect(and(pTrue, thunkTrue).b).toBe(   true); 
        expect(and(pFalse, thunkTrue).b).toBe(  false); 
        expect(and(pTrue, thunkFalse).b).toBe(  false); 
        expect(and(pFalse, thunkFalse).b).toBe( false); 

        //Extended Bools
        expect(and(pTrueExt, thunkTrue).b).toBe(   true); 
        expect(and(pFalseExt, thunkTrue).b).toBe(  false    ); 
        expect(and(pTrueExt, thunkFalse).b).toBe(  false); 
        expect(and(pFalseExt, thunkFalse).b).toBe( false); 

        //Short Ciruiting
        expect(and(pFalse, thunkNum).b).toBe(false);

        //Errors 
        checkForPyretError(function() {and(pNum, thunkTrue);});
        checkForPyretError(function() {and(pTrue, thunkNum);});
        
    });
    it("should have correct _or", function() {

        var _or = rt.getColonField(pTrue,'_or');
        expect(_or).not.toBeUndefined();
        expect(rt.isMethod(_or)).toEqual(true);
        var or = _or.full_meth;
        
        //Base
        expect(or(pTrue, thunkTrue).b).toBe(   true); 
        expect(or(pFalse, thunkTrue).b).toBe(  true); 
        expect(or(pTrue, thunkFalse).b).toBe(  true); 
        expect(or(pFalse, thunkFalse).b).toBe( false); 

        //Extended Bools
        expect(or(pTrueExt, thunkTrue).b).toBe(   true); 
        expect(or(pFalseExt, thunkTrue).b).toBe(  true); 
        expect(or(pTrueExt, thunkFalse).b).toBe(  true); 
        expect(or(pFalseExt, thunkFalse).b).toBe( false); 

        //Short Ciruiting
        expect(or(pTrue, thunkNum).b).toBe(true);

        //Errors 
        checkForPyretError(function() {or(pNum, thunkTrue);});
        checkForPyretError(function() {or(pFalse, thunkNum);});
    });
    it("should have correct _not", function() {

        var _not = rt.getColonField(pTrue,'_not');
        expect(_not).not.toBeUndefined();
        expect(rt.isMethod(_not)).toEqual(true);
        var not = _not.full_meth;
        
        //Base
        expect(not(pTrue).b).toBe(   false); 
        expect(not(pFalse).b).toBe(  true); 

        //Extended Bools
        expect(not(pTrueExt).b).toBe(   false); 
        expect(not(pFalseExt).b).toBe(  true); 

        //Errors 
        checkForPyretError(function() {not(pNum);});
    });
    it("should have correct tostring", function() {

        var _tostring = rt.getColonField(pTrue,'tostring');
        expect(_tostring).not.toBeUndefined();
        expect(rt.isMethod(_tostring)).toEqual(true);
        var tostring = _tostring.full_meth;
        
        //Base
        expect(tostring(pTrue).s).toBe(   "true"); 
        expect(tostring(pFalse).s).toBe(  "false"); 

        //Extended Bools
        expect(tostring(pTrueExt).s).toBe(   "true"); 
        expect(tostring(pFalseExt).s).toBe(  "false"); 

        //Errors 
        checkForPyretError(function() {tostring(pNum);});
    });
  });
}

exports['performTest'] = performTest;
