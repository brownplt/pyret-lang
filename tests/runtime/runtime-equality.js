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

        it("should distinguish arrays", function() {
          var a1 = rt.makeArray([1,2,3]);
          var a2 = rt.makeArray([1,2,3]);
          var a3 = rt.makeArray([4,5,6]);
          expect(rt.identical(a1, a1)).toBe(true);
          expect(rt.identical(a1, a2)).toBe(false);
          expect(rt.identical(a1, a3)).toBe(false);
          expect(rt.equal_always(a1, a1)).toBe(true);
          expect(rt.equal_always(a1, a2)).toBe(false);
          expect(rt.equal_always(a1, a3)).toBe(false);
          expect(rt.equal_now(a1, a1)).toBe(true);
          expect(rt.equal_now(a1, a2)).toBe(true);
          expect(rt.equal_now(a1, a3)).toBe(false);
        });

        it("should distinguish objects (no _equals)", function() {
          var r1 = rt.makeRef(rt.Number);
          var r2 = rt.makeRef(rt.Number);
          var o1 = rt.makeObject({});
          var o2 = rt.makeObject({});
          var o3 = rt.makeObject({s: rt.makeString("foo")});
          var o4 = rt.makeObject({s: rt.makeString("foo")});
          var o5 = rt.makeObject({s: rt.makeString("bar")});
          var o6 = rt.makeObject({t: rt.makeString("bar")});
          var o7 = rt.makeObject({r: r1});
          var o8 = rt.makeObject({r: r2});

          rt.setRef(r1, rt.makeNumber(0));
          rt.setRef(r2, rt.makeNumber(0));

          expect(rt.identical(o1, o1)).toBe(true);
          expect(rt.identical(o1, o2)).toBe(false);
          expect(rt.identical(o1, o3)).toBe(false);
          expect(rt.identical(o3, o4)).toBe(false);
          expect(rt.identical(o3, o5)).toBe(false);
          expect(rt.identical(o3, o6)).toBe(false);
          expect(rt.identical(o7, o8)).toBe(false);

          expect(rt.equal_always(o1, o1)).toBe(true);
          expect(rt.equal_always(o1, o2)).toBe(true);
          expect(rt.equal_always(o1, o3)).toBe(false);
          expect(rt.equal_always(o3, o4)).toBe(true);
          expect(rt.equal_always(o3, o5)).toBe(false);
          expect(rt.equal_always(o3, o6)).toBe(false);
          expect(rt.equal_always(o7, o8)).toBe(false);

          expect(rt.equal_now(o1, o1)).toBe(true);
          expect(rt.equal_now(o1, o2)).toBe(true);
          expect(rt.equal_now(o1, o3)).toBe(false);
          expect(rt.equal_now(o3, o4)).toBe(true);
          expect(rt.equal_now(o3, o5)).toBe(false);
          expect(rt.equal_now(o3, o6)).toBe(false);
          expect(rt.equal_now(o7, o8)).toBe(true);
        });

        it("should throw on functions", function() {
          var f = rt.makeFunction(function() { return true; });
          var g = rt.makeFunction(function() { return true; });
          expect(function() { rt.identical(f, f) }).toThrow();
          expect(function() { rt.equal_always(f, f) }).toThrow();
          expect(function() { rt.equal_now(f, f) }).toThrow();
          expect(function() { rt.identical(f, g) }).toThrow();
          expect(function() { rt.equal_always(f, g) }).toThrow();
          expect(function() { rt.equal_now(f, g) }).toThrow();
        });

        it("should throw on methods", function() {
          function y(a) { return makeNumber(0); };
          function y_curry(a) { return function() { return makeNumber(0); } };
          function z(a) { return makeNumber(0); };
          function z_curry(a) { return function() { return makeNumber(0); } };
          var m = rt.makeMethod(y_curry, y);
          var n = rt.makeMethod(z_curry, z);
          expect(function() { rt.identical(m, m) }).toThrow();
          expect(function() { rt.equal_always(m, m) }).toThrow();
          expect(function() { rt.equal_now(m, m) }).toThrow();
          expect(function() { rt.identical(m, n) }).toThrow();
          expect(function() { rt.equal_always(m, n) }).toThrow();
          expect(function() { rt.equal_now(m, n) }).toThrow();
        });
      });

    }
    return { performTest: performTest };
});
