var r = require("requirejs")
define(["js/runtime-anf", "js/ffi-helpers", "./matchers"], function(rtLib, ffiLib, matchers) {

    _ = require('jasmine-node');
    var path = require('path');
    var addPyretMatchers = matchers.addPyretMatchers;

    function performTest(useCompiled) {
      var output;
      var rt;
      var ffi;

      /**@ param {string} str, output*/
      function stdout(str) {
          output += str;
      }

      beforeEach(function(){
        output = "";
        rt = rtLib.makeRuntime({'stdout' : stdout});
        ffi = ffiLib(rt, rt.namespace);
        addPyretMatchers(this, rt);
      });

      describe("Equality", function() {
        it("should distinguish different tags", function() {
          var zero = rt.makeNumber(0);
          var f = rt.makeFunction(function() { return true; });
          var t = rt.makeBoolean(true);
          var a = rt.makeString("a");

          expect(rt.identical(zero, f)).toBe(false);
          expect(rt.equal_always(zero, f)).toBe(false);
          expect(rt.equal_now(zero, f)).toBe(false);
          expect(rt.identical(zero, t)).toBe(false);
          expect(rt.equal_always(zero, t)).toBe(false);
          expect(rt.equal_now(zero, t)).toBe(false);
          expect(rt.identical(zero, a)).toBe(false);
          expect(rt.equal_always(zero, a)).toBe(false);
          expect(rt.equal_now(zero, a)).toBe(false);
          expect(rt.identical(f, t)).toBe(false);
          expect(rt.equal_always(f, t)).toBe(false);
          expect(rt.equal_now(f, t)).toBe(false);
        });

        it("should distinguish numbers", function() {
          var zero = rt.makeNumber(0);
          var one = rt.makeNumber(1);
          expect(rt.identical(zero, zero)).toBe(true);
          expect(rt.identical(zero, one)).toBe(false);
          expect(rt.equal_always(zero, zero)).toBe(true);
          expect(rt.equal_always(zero, one)).toBe(false);
          expect(rt.equal_now(zero, zero)).toBe(true);
          expect(rt.equal_now(zero, one)).toBe(false);
        });

        it("should distinguish strings", function() {
          var a = rt.makeString("a");
          var b = rt.makeString("b");
          var c = rt.makeString("a");
          expect(rt.identical(a, a)).toBe(true);
          expect(rt.identical(a, b)).toBe(false);
          expect(rt.identical(a, c)).toBe(true);
          expect(rt.equal_always(a, a)).toBe(true);
          expect(rt.equal_always(a, b)).toBe(false);
          expect(rt.equal_always(a, c)).toBe(true);
          expect(rt.equal_now(a, a)).toBe(true);
          expect(rt.equal_now(a, b)).toBe(false);
          expect(rt.equal_now(a, c)).toBe(true);
        });

        it("should distinguish refs", function() {
          var a = rt.makeRef(rt.Number);
          var b = rt.makeRef(rt.Number);
          rt.setRef(a, rt.makeNumber(3));
          rt.setRef(b, rt.makeNumber(3));
          expect(rt.identical(a, a)).toBe(true);
          expect(rt.identical(a, b)).toBe(false);
          expect(rt.equal_always(a, a)).toBe(true);
          expect(rt.equal_always(a, b)).toBe(false);
          expect(rt.equal_now(a, a)).toBe(true);
          expect(rt.equal_now(a, b)).toBe(true);
        });

        it("should throw on functions", function() {
          var f = rt.makeFunction(function() { return true; });
          expect(function() { rt.identical(f, f) }).toThrow();
          expect(function() { rt.equal_always(f, f) }).toThrow();
          expect(function() { rt.equal_now(f, f) }).toThrow();
        });
      });

    }
    return { performTest: performTest };
});
