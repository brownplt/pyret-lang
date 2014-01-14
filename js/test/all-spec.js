var USE_COMPILED = true;

var base = require("./runtime-test.js");
base.performTest(USE_COMPILED);

var numbers = require("./runtime-numbers.js");
numbers.performTest(USE_COMPILED);

var strings = require("./runtime-strings.js");
strings.performTest(USE_COMPILED);

var booleans = require("./runtime-booleans.js");
booleans.performTest(USE_COMPILED);

