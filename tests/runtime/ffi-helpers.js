var r = require("requirejs")

define(["./matchers", "js/js-numbers", "js/ffi-helpers"], function (matchers, jsnums, ffiLib) {

  _ = require('jasmine-node');
  var path = require('path');
  var addPyretMatchers = matchers.addPyretMatchers;

  function performTest(useCompiled){

    var R = r('js/runtime-anf');

    var output;
    var rt;
    var ffi;

    /**@ param {string} str, output*/
    function stdout(str) {
        output += str;
    }


    beforeEach(function(){
        output = "";
        rt = R.makeRuntime({'stdout' : stdout});
        ffi = ffiLib(rt, rt.namespace);
        addPyretMatchers(this, rt);
    });


    describe("FFI", function() {
      it("should make lists and come back", function() {
        expect(ffi.toArray(ffi.makeList([1,2,3]))).toEqual([1,2,3]);
      });
    });
  }

  return { performTest : performTest };
});
