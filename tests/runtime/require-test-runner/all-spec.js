var r = require("requirejs");

r.config({
  paths: {
    trove: "../../../build/phase1/trove",
    js: "../../../build/phase1/js",
    compiler: "../../../build/phase1/arr/compiler"
  }
});

r(["../runtime-test",
   "../runtime-numbers", 
   "../runtime-strings", 
   "../runtime-booleans",
   "../runtime-brands",
   "../test-namespaces",
   "../ffi-helpers"],
  function (base, numbers, strings, booleans, brands, namespaces, ffi) {
    var USE_COMPILED = false;
    
    base.performTest(USE_COMPILED);
    // numbers.performTest(USE_COMPILED);
    // strings.performTest(USE_COMPILED);
    // //       booleans.performTest(USE_COMPILED);
    // brands.performTest(USE_COMPILED);
    // namespaces.performTest(USE_COMPILED);
    // ffi.performTest();
  });
