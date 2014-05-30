
var r = require("requirejs")

define(["./matchers", "js/js-numbers"], function (matchers, jsnums) {

  _ = require('jasmine-node');
  var path = require('path');
  var addPyretMatchers = matchers.addPyretMatchers;

  function performTest(useCompiled){

    var R = r('js/runtime-anf');

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
        output = "";
        rt = R.makeRuntime({'stdout' : stdout});
        addPyretMatchers(this, rt);
    });

    describe("arrays", function() {
      it("should get an array value", function() {
        var a1 = rt.makeArray([1,2,3]);
        expect(rt.raw_array_get(a1, rt.makeNumber(2))).toEqual(3);
        expect(function() {
          rt.raw_array_get(a1, rt.makeNumber(3));
        }).toThrow();
        expect(function() {
          rt.raw_array_get(a1, rt.makeNumber(-1));
        }).toThrow();
        expect(function() {
          rt.raw_array_get(a1, rt.makeNumber(1.5));
        }).toThrow();
      });

      it("should set array values", function() {
        var a1 = rt.makeArray([1,2,3]);
        rt.raw_array_set(a1, 2, "foo");
        expect(rt.raw_array_get(a1, rt.makeNumber(2))).toEqual("foo");
        expect(function() {
          rt.raw_array_set(a1, rt.makeNumber(3), "a");
        }).toThrow();
        expect(function() {
          rt.raw_array_set(a1, rt.makeNumber(-1), "a");
        }).toThrow();
        expect(function() {
          rt.raw_array_set(a1, rt.makeNumber(1.5), "a");
        }).toThrow();
      });

      it("should make arrays of constants", function() {
        var a1 = rt.raw_array_of("v", 135);
        expect(rt.raw_array_get(a1, rt.makeNumber(134))).toEqual("v");
        expect(function() {
          rt.raw_array_get(a1, rt.makeNumber(135));
        }).toThrow();
        expect(rt.raw_array_get(a1, rt.makeNumber(0))).toEqual("v");
      });

      it("should get the array length", function() {
        var a1 = rt.raw_array_of("v", 135);
        expect(rt.raw_array_length(a1)).toEqual(rt.makeNumber(135));
      });
    });
  }
  return { performTest: performTest };
})
