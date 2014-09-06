var r = require("requirejs")
define(["js/runtime-anf", "./matchers"], function(rtLib, matchers) {

    _ = require('jasmine-node');
    var path = require('path');
    var addPyretMatchers = matchers.addPyretMatchers;

    function performTest(useCompiled) {
      var output;
      var rt;

      /**@ param {string} str, output*/
      function stdout(str) {
          output += str;
      }

      var r1, r2;
      beforeEach(function(){
        output = "";
        rt = rtLib.makeRuntime({'stdout' : stdout});
        r1 = rt.makeGraphableRef();
        r2 = rt.makeRef(rt.Number);
        addPyretMatchers(this, rt);
      });

      describe("References", function() {
        it("should create unset references", function() {
          expect(r1).toPassPredicate(rt.isRef);
          expect(rt.isRefSet(r1)).toBe(false);
          expect(rt.isRefFrozen(r1)).toBe(false);
          expect(function() { rt.getRef(r1) }).toThrow();
        });

        it("should not allow setting graphable refs", function() {
          expect(r1).toPassPredicate(rt.isRef);
          expect(rt.isRefGraphable(r1)).toBe(true);
          expect(rt.isRefSet(r1)).toBe(false);
          expect(rt.isRefFrozen(r1)).toBe(false);
          expect(function() { rt.getRef(r1) }).toThrow();

          var ttwo = rt.makeNumber(22)
          expect(function() { rt.setRef(r1, ttwo);}).toThrow();
          expect(rt.isRefGraphable(r1)).toBe(true);
          expect(rt.isRefSet(r1)).toBe(false);
          expect(rt.isRefFrozen(r1)).toBe(false);
        });

        it("should allow setting references", function() {
          expect(r1).toPassPredicate(rt.isRef);
          expect(rt.isRefGraphable(r1)).toBe(true);
          expect(rt.isRefSet(r1)).toBe(false);
          expect(rt.isRefFrozen(r1)).toBe(false);
          expect(function() { rt.getRef(r1) }).toThrow();

          rt.addRefAnn(r1, rt.Number);
          expect(rt.isRefSet(r1)).toBe(false);
          expect(rt.isRefFrozen(r1)).toBe(false);

          rt.refEndGraph(r1);
          expect(rt.isRefGraphable(r1)).toBe(false);
          expect(rt.isRefSet(r1)).toBe(false);
          expect(rt.isRefFrozen(r1)).toBe(false);

          var ttwo = rt.makeNumber(22)
          rt.setRef(r1, ttwo);
          expect(rt.isRefSet(r1)).toBe(true);
          expect(rt.isRefFrozen(r1)).toBe(false);
          expect(rt.getRef(r1)).toBe(ttwo);
        });

        it("should throw an error if the wrong value is set initially", function() {
          var str = rt.makeString("a");
          expect(function() { rt.setRef(str); }).toThrow();
          expect(rt.isRefSet(r1)).toBe(false);
          expect(function() { rt.setRef(str); }).toThrow();
          expect(rt.isRefSet(r1)).toBe(false);
        });

        it("should throw an error if the wrong value is set later", function() {
          var ttwo = rt.makeNumber(22)
          rt.setRef(r2, ttwo);
          var str = rt.makeString("a");
          expect(function() { rt.setRef(r2, str); }).toThrow();
          expect(rt.getRef(r2)).toBe(ttwo);
        });

        it("should still allow correct sets after a failed set", function() {
          var str = rt.makeString("a");
          expect(function() { rt.setRef(r2, str); }).toThrow();
          expect(function() { rt.setRef(r2, str); }).toThrow();
          var ttwo = rt.makeNumber(22)
          rt.setRef(r2, ttwo);
          expect(rt.getRef(r2)).toBe(ttwo);
        });

        it("should not allow freezing an unset ref", function() {
          expect(function() { rt.freezeRef(r1); }).toThrow();
          expect(function() { rt.freezeRef(r2); }).toThrow();
        });

        it("should not allow any sets after a freeze", function() {
          var ttwo = rt.makeNumber(22);
          rt.setRef(r2, ttwo);
          rt.freezeRef(r2);
          expect(rt.isRefSet(r2)).toBe(true);
          expect(rt.isRefFrozen(r2)).toBe(true);
          expect(function() { rt.setRef(r2, ttwo); }).toThrow();
        });

        it("should allow refreezing a frozen reference", function() {
          var ttwo = rt.makeNumber(22);
          rt.setRef(r2, ttwo);
          rt.freezeRef(r2);
          expect(rt.isRefSet(r2)).toBe(true);
          expect(rt.isRefFrozen(r2)).toBe(true);
          rt.freezeRef(r2);
          expect(rt.isRefSet(r2)).toBe(true);
          expect(rt.isRefFrozen(r2)).toBe(true);
        });
        
      });

    }
    return { performTest: performTest };
});
