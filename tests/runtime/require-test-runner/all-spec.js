var r = require("requirejs");

var build = process.env["PHASE"] || "build/phase1";

r.config({
  waitSeconds: 15000,
  paths: {
    trove: "../../../" + build + "/trove",
    js: "../../../" + build + "/js",
    compiler: "../../../" + build + "/arr/compiler"
  }
});

console.log("Starting runtime tests");

r(["../runtime-test",
   "../runtime-numbers", 
   "../runtime-strings", 
   "../runtime-booleans",
   "../runtime-brands",
   "../runtime-arrays",
   "../runtime-refs",
   "../runtime-equality",
   "../test-namespaces",
   "../ffi-helpers"
  ],
  function (base, numbers, strings, booleans, brands, arrays, refs, equality, namespaces, ffi
           ) {
    var USE_COMPILED = false;
    
    base.performTest(USE_COMPILED);
    numbers.performTest(USE_COMPILED);
    strings.performTest(USE_COMPILED);
//    booleans.performTest(USE_COMPILED);
    brands.performTest(USE_COMPILED);
    arrays.performTest(USE_COMPILED);
    refs.performTest(USE_COMPILED);
    equality.performTest(USE_COMPILED);
    namespaces.performTest(USE_COMPILED);
    ffi.performTest();
  }, function(err) {
  console.log("Require failed! ", err);

});
