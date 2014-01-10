_ = require('jasmine-node');
R = require('../runtime-anf.js').PYRET_ANF;

var output;
var rt;

/**@ param {string} str, output*/
function stdout(str) {
    output += str;
}

//Strings for testing
var empty;
var a;
var b;
var hello;
var world;
var hello_world;
var UPPER;
var lower;
var hi;
var o;

var five;
var half;
var hun;
var four;

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
        }
    });

    output = "";
    rt = R.makeRuntime({'stdout' : stdout});

    //Make Some Strings
    empty = rt.makeString("");
    a = rt.makeString("a");
    b = rt.makeString("b");
    o = rt.makeString("o");
    hello = rt.makeString("hello");
    world = rt.makeString("world");
    hello_world = rt.makeString("hello world");
    UPPER = rt.makeString("UPPER");
    lower = rt.makeString("lower");
    hi = rt.makeString("hi");

    five = rt.makeString("5");
    four = rt.makeString("4");
    hun = rt.makeString("100");
    half = rt.makeString("0.5");
});


  describe("String Dictionary", function() {
    it("should have correct _plus method", function() {
        var _plus_method = empty.dict['_plus'];

        expect(_plus_method).not.toBeUndefined();
        expect(rt.isMethod(_plus_method)).toBe(true);

        var plus = _plus_method.meth;

        expect(plus(empty,empty).s).toEqual("");
        expect(plus(a,empty).s).toEqual("a");
        expect(plus(empty,b).s).toEqual("b");
        expect(plus(a,b).s).toEqual("ab");
        expect(plus(a,hello).s).toEqual("ahello");
    });

    it("should have correct append method", function() {
        var append_method = empty.dict['append'];

        expect(append_method).not.toBeUndefined();
        expect(rt.isMethod(append_method)).toBe(true);

        var append = append_method.meth;

        expect(append(empty,empty).s).toEqual("");
        expect(append(a,empty).s).toEqual("a");
        expect(append(empty,b).s).toEqual("b");
        expect(append(a,b).s).toEqual("ab");
        expect(append(a,hello).s).toEqual("ahello");
    });

    it("should have correct contains method", function() {
        var contains_method = empty.dict['contains'];

        expect(contains_method).not.toBeUndefined();
        expect(rt.isMethod(contains_method)).toBe(true);

        var contains = contains_method.meth;

        expect(contains(empty, empty).b).toBe(true);
        expect(contains(hello, empty).b).toBe(true);
        expect(contains(world, empty).b).toBe(true);
        expect(contains(hello_world, hello).b).toBe(true);
        expect(contains(hello_world, world).b).toBe(true);
        expect(contains(hello_world, o).b).toBe(true);
        expect(contains(o, o).b).toBe(true);

        expect(contains(empty, a).b).toEqual(false);
        expect(contains(hi, a).b).toEqual(false);
        expect(contains(hello, a).b).toEqual(false);
        expect(contains(world, a).b).toEqual(false);
        expect(contains(world, hello).b).toEqual(false);
    });

    it("should have correct length method", function() {
        var length_method = empty.dict['length'];

        expect(length_method).not.toBeUndefined();
        expect(rt.isMethod(length_method)).toBe(true);

        var length = length_method.meth;

        expect(length(empty).n).toEqual(0);
        expect(length(a).n).toEqual(1);
        expect(length(b).n).toEqual(1);
        expect(length(hello).n).toEqual(5);
        expect(length(hello_world).n).toEqual(11);
  });

    it("should have correct tonumber method", function() {
        var tonumber_method = empty.dict['tonumber'];

        expect(tonumber_method).not.toBeUndefined();
        expect(rt.isMethod(tonumber_method)).toBe(true);

        var tonumber = tonumber_method.meth;

        //Are Numbers
        expect(tonumber(five).n).toEqual(5);
        expect(tonumber(four).n).toEqual(4);
        expect(tonumber(hun).n).toEqual(100);
        expect(tonumber(half).n).toEqual(0.5);

        //Not numbers
        expect(rt.isNothing(tonumber(hello))).toBe(true);
        expect(rt.isNothing(tonumber(world))).toBe(true); 
        expect(rt.isNothing(tonumber(empty))).toBe(true); //Special case as Number("") = 0
    });

});
