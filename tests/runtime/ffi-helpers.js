var r = require("requirejs")

define(["./matchers", "../evaluator/eval-matchers", "js/ffi-helpers", "trove/srcloc", "trove/render-error-display"], function (matchers, e, ffiLib, srclocLib, rendererrorLib) {

  _ = require('jasmine-node');
  var path = require('path');
  var addPyretMatchers = matchers.addPyretMatchers;

  function performTest(useCompiled){

    var R = r('js/runtime-anf');

    var output;
    var rt;
    var ffi;
    var get;
    var str;

    /**@ param {string} str, output*/
    function stdout(str) {
        output += str;
    }

    beforeEach(function(){
        output = "";
        rt = R.makeRuntime({'stdout' : stdout});
        get = rt.getField;
        str = rt.makeString;
        P = e.makeEvalCheckers(this, rt);
        ffi = ffiLib(rt, rt.namespace);
        addPyretMatchers(this, rt);
    });

    describe("FFI", function() {
      it("should make lists and come back", function() {
        expect(ffi.toArray(ffi.makeList([1,2,3]))).toEqual([1,2,3]);
      });

      it("should work with cases", function(done) {
        rt.loadModules(rt.namespace, [srclocLib], function(srcloc) {
          ffi.cases(get(srcloc, "Srcloc"), "Srcloc", get(srcloc, "builtin").app("some-module"), {
            "builtin": function(m) {
              expect(m).toEqual("some-module");
            },
            "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
              fail();
            }
          });
        });
        P.wait(done);
      });

      it("should catch type/arity errors in runtime functions", function(done) {
        rt.loadModulesNew(rt.namespace, [rendererrorLib], function(rendererrorLib) {
          var rendererror = rt.getField(rendererrorLib, "values");
          // Can't use toThrow because of generative structs
          expect(function() { ffi.throwFieldNotFound("not a srcloc", undefined, undefined); })
            .toThrowRuntimeExn(rt, rendererror, "Expected \"Srcloc\", but got \"not a srcloc\"");
          expect(function() { rt.confirm(str("too"), str("many"), str("arguments")); })
            .toThrowRuntimeExn(rt, rendererror, "Expected to get 2 arguments");
          P.wait(done);
        });
      });
    });
  }

  return { performTest : performTest };
});
