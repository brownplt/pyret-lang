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
        it("should distinguish numbers", function() {
          var zero = rt.makeNumber(0);
          var one = rt.makeNumber(1);
          var f = rt.makeFunction(function() { return true; });
          expect(rt.identical(zero, zero)).toBe(true);
          expect(rt.identical(zero, one)).toBe(false);
          expect(rt.equal_always(zero, zero)).toBe(true);
          expect(rt.equal_always(zero, one)).toBe(false);
          expect(rt.equal_now(zero, zero)).toBe(true);
          expect(rt.equal_now(zero, one)).toBe(false);
        });
      });

    }
    return { performTest: performTest };
});
