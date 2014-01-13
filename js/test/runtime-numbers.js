_ = require('jasmine-node');
R = require('../anf-comp.js').PYRET_ANF;
var jsnums = require('../js-numbers/src/js-numbers.js');

var output;
var rt;

/**@ param {string} str, output*/
function stdout(str) {
    output += str;
}

//Numbers for testing
var five;
var nfive;
var zero;
var one;
var none;
var two;
var four;
var twty;
var seven;
var six;
var twtyseven;
var half;
var big;

beforeEach(function(){
    this.addMatchers({
        toHaveEmptyDict : function() {
            return (this.actual.dict !== undefined) && (Object.keys(this.actual.dict).length === 0);
        },
        toHaveNoBrands : function() {
            return (this.actual.brands !== undefined) && (this.actual.brands.length === 0);
        },
        //Tests equality with ===, must be exact same
        toBeIdentical : function(expect) {
            return this.actual === expect;
        },

        //Tests equality of bignums and expectted js num
        toBigEqual : function(expect) {
            return jsnums['equals'](this.actual, jsnums.fromFixnum(expect));
        },

        //Tests equality of bignums
        toBeBig : function(expect) {
            return jsnums['equals'](this.actual, expect);
        }
    });

    output = "";
    rt = R.makeRuntime({'stdout' : stdout});

    //Make Some numbers
    five = rt.makeNumber(5);
    nfive = rt.makeNumber(-5);
    zero = rt.makeNumber(0);
    one = rt.makeNumber(1);
    none = rt.makeNumber(-1);
    two = rt.makeNumber(2);
    four = rt.makeNumber(4);
    six = rt.makeNumber(6);
    twty = rt.makeNumber(20);
    twtyseven = rt.makeNumber(27);
    seven = rt.makeNumber(7);
    half = rt.makeNumber(.5);
    big = rt.makeNumber(9000000000000000);
});


  describe("Number Dictionary", function() {
    it("should have correct _plus", function() {
        var _plus = five.dict['_plus'];
        expect(_plus).not.toBeUndefined();
        expect(rt.isMethod(_plus)).toEqual(true);

        
        var plus = _plus.meth;

        //Basic
        expect(plus(zero, zero).n).toEqual(0);
        expect(plus(one, zero).n).toEqual(1);
        expect(plus(two, two).n).toEqual(4);
        expect(plus(two, four).n).toEqual(6);
        expect(plus(four, two).n).toEqual(6);
        
        //Negative Numbers
        expect(plus(nfive, two).n).toEqual(-3);
        expect(plus(two, nfive).n).toEqual(-3);
       
        //Fractions
        expect(plus(two, half).n).toEqual(2.5);

        //Chaining
        expect(plus(plus(two, half), plus(two, half)).n).toEqual(5);
        expect(plus(plus(two, one), plus(twty, seven)).n).toEqual(30);

        //BIG
        expect(times(big, big).n).toBeBig(jsnums.fromString("18000000000000000"));
    });


    it("should have correct _minus", function() {
        var _minus = five.dict['_minus'];
        expect(_minus).not.toBeUndefined();
        expect(rt.isMethod(_minus)).toBe(true);

        
        var minus = _minus.meth;

        //Basic
        expect(minus(zero, zero).n).toBigEqual(0);
        expect(minus(one, zero).n).toBigEqual(1);
        expect(minus(two, two).n).toBigEqual(0);
        expect(minus(two, four).n).toBigEqual(-2);
        expect(minus(four, two).n).toBigEqual(2);
        
        //Negative Numbers
        expect(minus(nfive, two).n).toBigEqual(-7);
        expect(minus(two, nfive).n).toBigEqual(7);
       
        //Fractions
        expect(minus(two, half).n).toBigEqual(1.5);

        //Chaining
        expect(minus(minus(two, half), minus(two, half)).n).toBigEqual(0);
        expect(minus(minus(two, one), minus(twty, seven)).n).toBigEqual(-12);
        expect(minus(minus(zero, big), big).n).toBeBig(jsnums.fromString("-18000000000000000"));
    });

    it("should have correct _times", function() {
        var _times = five.dict['_times'];
        expect(_times).not.toBeUndefined();
        expect(rt.isMethod(_times)).toEqual(true);

        
        var times = _times.meth;

        //Basic
        expect(times(zero, zero).n).toBigEqual(0);
        expect(times(one, zero).n).toBigEqual(0);
        expect(times(two, two).n).toBigEqual(4);
        expect(times(two, four).n).toBigEqual(8);
        expect(times(four, two).n).toBigEqual(8);
        
        //Negative Numbers
        expect(times(nfive, two).n).toBigEqual(-10);
        expect(times(two, nfive).n).toBigEqual(-10);
       
        //Fractions
        expect(times(two, half).n).toBigEqual(1);

        //Chaining
        expect(times(times(two, half), times(two, half)).n).toBigEqual(1);
        expect(times(times(two, one), times(twty, seven)).n).toBigEqual(280);

        //BIG Numbers
        expect(times(big, big).n).toBeBig(jsnums.fromString("81000000000000000000000000000000"));
        expect(times(none, times(big, big)).n).toBeBig(jsnums.fromString("-81000000000000000000000000000000"));
    });

    it("should have correct _divide", function() {
        var _divide = five.dict['_divide'];
        expect(_divide).not.toBeUndefined();
        expect(rt.isMethod(_divide)).toEqual(true);

        
        var divide = _divide.meth;

        //Basic
        try{
            divide(one,zero);
            throw new Error("Did not error on divide by zero");
        }
        catch(e) {
           if(!rt.isPyretException(e)) {
            throw new Error("Divide by zero did not result in pyret error, but threw other error", e)
           }
        }

        try{
            divide(zero,zero);
            throw new Error("Did not error on divide by zero");
        }
        catch(e) {
           if(!rt.isPyretException(e)) {
            throw new Error("Divide by zero did not result in pyret error, but threw other error", e)
           }
        }

        expect(divide(two, two).n).toBigEqual(1);
        expect(divide(two, four).n).toBigEqual(.5);
        expect(divide(four, two).n).toBigEqual(2);
        
        //Negative Numbers
        expect(divide(nfive, two).n).toBigEqual(-2.5);
        expect(divide(two, nfive).n).toBigEqual(-2/5);
       
        //Fractions
        expect(divide(two, half).n).toBigEqual(4);

        //Chaining
        expect(divide(divide(two, half), divide(two, half)).n).toBigEqual(1);
        expect(divide(divide(two, one), divide(twty, seven)).n).toBigEqual(2 / (20/7));
    });

    //Comparison functions
    it("should have correct _lessthan", function() {
        var _lessthan = five.dict['_lessthan'];
        expect(_lessthan).not.toBeUndefined();
        expect(rt.isMethod(_lessthan)).toEqual(true);

        
        var lessthan = _lessthan.meth;

        //Equal
        expect(lessthan(zero, zero).b).toEqual(  false);
        expect(lessthan(one , one).b).toEqual(   false);
        expect(lessthan(four, four).b).toEqual(  false);
        expect(lessthan(half, half).b).toEqual(  false);
        expect(lessthan(nfive, nfive).b).toEqual(false);

        //Less than
        expect(lessthan(zero, one).b).toEqual(  true);
        expect(lessthan(one , two).b).toEqual(  true);
        expect(lessthan(four, seven).b).toEqual(true);
        expect(lessthan(half, seven).b).toEqual(true);
        expect(lessthan(zero, half).b).toEqual( true);
        expect(lessthan(nfive, zero).b).toEqual(true);

        //Greater than
        expect(lessthan(one, zero).b).toEqual(   false);
        expect(lessthan(one, half).b).toEqual(   false);
        expect(lessthan(seven, four).b).toEqual( false);
        expect(lessthan(twty, four).b).toEqual(  false);
        expect(lessthan(twty, seven).b).toEqual( false);
        expect(lessthan(half, zero).b).toEqual(  false);
        expect(lessthan(half, nfive).b).toEqual( false);
        expect(lessthan(six, nfive).b).toEqual(  false);
    });

    it("should have correct _lessequal", function() {
        var _lessequal = five.dict['_lessequal'];
        expect(_lessequal).not.toBeUndefined();
        expect(rt.isMethod(_lessequal)).toEqual(true);

        
        var lessequal = _lessequal.meth;

        //Equal
        expect(lessequal(zero, zero).b).toEqual(  true);
        expect(lessequal(one , one).b).toEqual(   true);
        expect(lessequal(four, four).b).toEqual(  true);
        expect(lessequal(half, half).b).toEqual(  true);
        expect(lessequal(nfive, nfive).b).toEqual(true);

        //Less than
        expect(lessequal(zero, one).b).toEqual(  true);
        expect(lessequal(one , two).b).toEqual(  true);
        expect(lessequal(four, seven).b).toEqual(true);
        expect(lessequal(half, seven).b).toEqual(true);
        expect(lessequal(zero, half).b).toEqual( true);
        expect(lessequal(nfive, zero).b).toEqual(true);

        //Greater than
        expect(lessequal(one, zero).b).toEqual(   false);
        expect(lessequal(one, half).b).toEqual(   false);
        expect(lessequal(seven, four).b).toEqual( false);
        expect(lessequal(twty, four).b).toEqual(  false);
        expect(lessequal(twty, seven).b).toEqual( false);
        expect(lessequal(half, zero).b).toEqual(  false);
        expect(lessequal(half, nfive).b).toEqual( false);
        expect(lessequal(six, nfive).b).toEqual(  false);
    });

    it("should have correct _greaterthan", function() {
        var _greaterthan = five.dict['_greaterthan'];
        expect(_greaterthan).not.toBeUndefined();
        expect(rt.isMethod(_greaterthan)).toEqual(true);

        
        var greaterthan = _greaterthan.meth;

        //Equal
        expect(greaterthan(zero, zero).b).toEqual(  false);
        expect(greaterthan(one , one).b).toEqual(   false);
        expect(greaterthan(four, four).b).toEqual(  false);
        expect(greaterthan(half, half).b).toEqual(  false);
        expect(greaterthan(nfive, nfive).b).toEqual(false);

        //Less than
        expect(greaterthan(zero, one).b).toEqual(   false);
        expect(greaterthan(one , two).b).toEqual(   false);
        expect(greaterthan(four, seven).b).toEqual( false);
        expect(greaterthan(half, seven).b).toEqual( false);
        expect(greaterthan(zero, half).b).toEqual(  false);
        expect(greaterthan(nfive, zero).b).toEqual( false);

        //Greater than
        expect(greaterthan(one, zero).b).toEqual(   true);
        expect(greaterthan(one, half).b).toEqual(   true);
        expect(greaterthan(seven, four).b).toEqual( true);
        expect(greaterthan(twty, four).b).toEqual(  true);
        expect(greaterthan(twty, seven).b).toEqual( true);
        expect(greaterthan(half, zero).b).toEqual(  true);
        expect(greaterthan(half, nfive).b).toEqual( true);
        expect(greaterthan(six, nfive).b).toEqual(  true);
    });

    it("should have correct _greaterequal", function() {
        var _greaterequal = five.dict['_greaterequal'];
        expect(_greaterequal).not.toBeUndefined();
        expect(rt.isMethod(_greaterequal)).toEqual(true);

        
        var greaterequal = _greaterequal.meth;

        //Equal
        expect(greaterequal(zero, zero).b).toEqual(  true);
        expect(greaterequal(one , one).b).toEqual(   true);
        expect(greaterequal(four, four).b).toEqual(  true);
        expect(greaterequal(half, half).b).toEqual(  true);
        expect(greaterequal(nfive, nfive).b).toEqual(true);

        //Less than
        expect(greaterequal(zero, one).b).toEqual(   false);
        expect(greaterequal(one , two).b).toEqual(   false);
        expect(greaterequal(four, seven).b).toEqual( false);
        expect(greaterequal(half, seven).b).toEqual( false);
        expect(greaterequal(zero, half).b).toEqual(  false);
        expect(greaterequal(nfive, zero).b).toEqual( false);

        //Greater than
        expect(greaterequal(one, zero).b).toEqual(   true);
        expect(greaterequal(one, half).b).toEqual(   true);
        expect(greaterequal(seven, four).b).toEqual( true);
        expect(greaterequal(twty, four).b).toEqual(  true);
        expect(greaterequal(twty, seven).b).toEqual( true);
        expect(greaterequal(half, zero).b).toEqual(  true);
        expect(greaterequal(half, nfive).b).toEqual( true);
        expect(greaterequal(six, nfive).b).toEqual(  true);
    });

    it("should have correct _equals", function() {

        var _equals = five.dict['_equals'];
        expect(_equals).not.toBeUndefined();
        expect(rt.isMethod(_equals)).toEqual(true);

        
        var equals = _equals.meth;

        //Equal
        expect(equals(zero, zero).b).toEqual(  true);
        expect(equals(one , one).b).toEqual(   true);
        expect(equals(four, four).b).toEqual(  true);
        expect(equals(half, half).b).toEqual(  true);
        expect(equals(nfive, nfive).b).toEqual(true);

        //Less than
        expect(equals(zero, one).b).toEqual(   false);
        expect(equals(one , two).b).toEqual(   false);
        expect(equals(four, seven).b).toEqual( false);
        expect(equals(half, seven).b).toEqual( false);
        expect(equals(zero, half).b).toEqual(  false);
        expect(equals(nfive, zero).b).toEqual( false);

        //Greater than
        expect(equals(one, zero).b).toEqual(   false);
        expect(equals(one, half).b).toEqual(   false);
        expect(equals(seven, four).b).toEqual( false);
        expect(equals(twty, four).b).toEqual(  false);
        expect(equals(twty, seven).b).toEqual( false);
        expect(equals(half, zero).b).toEqual(  false);
        expect(equals(half, nfive).b).toEqual( false);
        expect(equals(six, nfive).b).toEqual(  false);
    });

    it("should have correct tostring", function() {

        var _tostring= five.dict['tostring'];
        expect(_tostring).not.toBeUndefined();
        expect(rt.isMethod(_tostring)).toEqual(true);

        
        var tostring = _tostring.meth;

        expect(tostring(zero).s).toEqual("0");
        expect(tostring(one).s).toEqual("1");
        expect(tostring(two).s).toEqual("2");
        expect(tostring(seven).s).toEqual("7");
        expect(tostring(twty).s).toEqual("20");
        expect(tostring(half).s).toEqual("0.5");
    });

    });

