var r = require("requirejs")

define(["./matchers", "lib/js-numbers/src/js-numbers"], function (matchers, jsnums) {

_ = require('jasmine-node');
var path = require('path');
var addPyretMatchers = matchers.addPyretMatchers;

function performTest(useCompiled) {
      var R = r('./build/phase2/js/runtime-anf');

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
        var _plus_method = empty.dict['_plus'];

        expect(_plus_method).not.toBeUndefined();
        expect(rt.isMethod(_plus_method)).toBe(true);

        var plus = _plus_method.full_meth;

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

        var append = append_method.full_meth;

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

        var contains = contains_method.full_meth;

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

        var length = length_method.full_meth;

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

        var tonumber = tonumber_method.full_meth;

        //Are Numbers
        expect(tonumber(five).n).toBigEqual(5);
        expect(tonumber(four).n).toBigEqual(4);
        expect(tonumber(hun).n).toBigEqual(100);
        expect(tonumber(half).n).toBigEqual(0.5);

        //Not numbers
        expect(rt.isNothing(tonumber(hello))).toBe(true);
        expect(rt.isNothing(tonumber(world))).toBe(true); 
        expect(rt.isNothing(tonumber(empty))).toBe(true); //Special case as Number("") = 0
    });

    it("should have a correct repeat method", function() {
      var s = rt.makeString("a");
      var n = rt.makeNumber(4);
      var zero = rt.makeNumber(0);

      var ssss = rt.getField(s, "repeat").app(n);
      expect(ssss.s).toEqual("aaaa");

      var mpty = rt.getField(s, "repeat").app(zero);
      expect(mpty.s).toEqual("");
    });

    it("should have a correct substring method", function() {
      var s = rt.makeString("file.arr");
      var n1 = rt.makeNumber(5);
      var n2 = rt.makeNumber(8);
      var sub = rt.getField(s, "substring").app(n1, n2);
      expect(sub.s).toEqual("arr");
    });

});
}

    return { performTest: performTest };

});
