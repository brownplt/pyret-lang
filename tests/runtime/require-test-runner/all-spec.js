var r = require("requirejs");

r.config({
  baseUrl: "../..",
  paths: {
    trove: "build/phase2/trove",
    js: "build/phase2/js"
  }
});

r(["./tests/runtime/runtime-test", "./tests/runtime/runtime-numbers", "./tests/runtime/runtime-strings", "./tests/runtime/runtime-booleans", "./tests/runtime/runtime-brands", "./tests/runtime/test-namespaces", "./tests/runtime/ffi-helpers"],
       function (base, numbers, strings, booleans, brands, namespaces, ffi) {
	   var USE_COMPILED = false;

     base.performTest(USE_COMPILED);
	   numbers.performTest(USE_COMPILED);
     strings.performTest(USE_COMPILED);
	   booleans.performTest(USE_COMPILED);
     brands.performTest(USE_COMPILED);
	   namespaces.performTest(USE_COMPILED);
     ffi.performTest();
       });
