var r = require("requirejs");

r(["../test/runtime-test", "../test/runtime-numbers", "../test/runtime-strings", "../test/runtime-booleans", "../test/runtime-brands"],
       function (base, numbers, strings, booleans, brands) {
	   var USE_COMPILED = false;

	   base.performTest(USE_COMPILED);
	   numbers.performTest(USE_COMPILED);
	   strings.performTest(USE_COMPILED);
	   booleans.performTest(USE_COMPILED);
	   brands.performTest(USE_COMPILED);
       });