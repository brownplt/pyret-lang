var r = require("requirejs");

r.config({
  waitSeconds: 15000,
  paths: {
    trove: "../../../build/phase1/trove",
    js: "../../../build/phase1/js",
    compiler: "../../../build/phase1/arr/compiler"
  }
});

console.log("Starting runtime tests");

r(["../runtime-test",
   "../runtime-numbers", 
   "../runtime-strings", 
   "../runtime-booleans",
   "../runtime-brands",
   "../runtime-arrays",
   "../test-namespaces",
   "../ffi-helpers"
  ],
  function (base, numbers, strings, booleans, brands, arrays, namespaces, ffi
           ) {
    var USE_COMPILED = false;
    
    base.performTest(USE_COMPILED);
    numbers.performTest(USE_COMPILED);
    strings.performTest(USE_COMPILED);
//    booleans.performTest(USE_COMPILED);
    brands.performTest(USE_COMPILED);
    arrays.performTest(USE_COMPILED);
    namespaces.performTest(USE_COMPILED);
    ffi.performTest();
  }, function(err) {
  console.log("Require failed! ", err);

});
