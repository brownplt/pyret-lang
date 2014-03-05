var r = require("requirejs")

define(["./matchers", "lib/js-numbers/src/js-numbers"], function (matchers, jsnums) {

_ = require('jasmine-node');
var path = require('path');
var addPyretMatchers = matchers.addPyretMatchers;

function performTest(useCompiled) {
      var R = r('./build/phase1/js/runtime-anf');

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
    output = "";
    rt = R.makeRuntime({'stdout' : stdout});
    addPyretMatchers(this, rt);


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
        expect(empty.dict).toBeUndefined();
      
        var plus = rt.plus;

        expect(plus(empty,empty)).toEqual("");
        expect(plus(a,empty)).toEqual("a");
        expect(plus(empty,b)).toEqual("b");
        expect(plus(a,b)).toEqual("ab");
        expect(plus(a,hello)).toEqual("ahello");
    });

    it("should have correct append method", function() {
        var append = rt.string_append;

        expect(append(empty,empty)).toEqual("");
        expect(append(a,empty)).toEqual("a");
        expect(append(empty,b)).toEqual("b");
        expect(append(a,b)).toEqual("ab");
        expect(append(a,hello)).toEqual("ahello");
    });

    it("should have correct contains method", function() {
        var contains = rt.string_contains;

        expect(contains(empty, empty)).toBe(true);
        expect(contains(hello, empty)).toBe(true);
        expect(contains(world, empty)).toBe(true);
        expect(contains(hello_world, hello)).toBe(true);
        expect(contains(hello_world, world)).toBe(true);
        expect(contains(hello_world, o)).toBe(true);
        expect(contains(o, o)).toBe(true);

        expect(contains(empty, a)).toEqual(false);
        expect(contains(hi, a)).toEqual(false);
        expect(contains(hello, a)).toEqual(false);
        expect(contains(world, a)).toEqual(false);
        expect(contains(world, hello)).toEqual(false);
    });

    it("should have correct length method", function() {
        var length = rt.string_length;

        expect(length(empty)).toEqual(0);
        expect(length(a)).toEqual(1);
        expect(length(b)).toEqual(1);
        expect(length(hello)).toEqual(5);
        expect(length(hello_world)).toEqual(11);
  });

    it("should have correct tonumber method", function() {
        var tonumber = rt.string_tonumber;

        //Are Numbers
        expect(tonumber(five)).toBigEqual(5);
        expect(tonumber(four)).toBigEqual(4);
        expect(tonumber(hun)).toBigEqual(100);
        expect(tonumber(half)).toBigEqual(0.5);

        //Not numbers
        expect(rt.isNothing(tonumber(hello))).toBe(true);
        expect(rt.isNothing(tonumber(world))).toBe(true); 
        expect(rt.isNothing(tonumber(empty))).toBe(true); //Special case as Number("") = 0
    });

    it("should have a correct repeat method", function() {
      var s = rt.makeString("a");
      var n = rt.makeNumber(4);
      var zero = rt.makeNumber(0);

      var ssss = rt.string_repeat(s, n);
      expect(ssss).toEqual("aaaa");

      var mpty = rt.string_repeat(s, zero);
      expect(mpty).toEqual("");
    });

    it("should have a correct substring method", function() {
      var s = rt.makeString("file.arr");
      var n1 = rt.makeNumber(5);
      var n2 = rt.makeNumber(8);
      var sub = rt.string_substring(s, n1, n2);
      expect(sub).toEqual("arr");
    });

});
}

    return { performTest: performTest };

});
