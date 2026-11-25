var tester = require("../test-util/util.js");

describe("Running chart programs", function() {
  beforeEach(tester.setup);
  afterEach(tester.teardown);

  var chartTestsBase = "./test-util/pyret-programs/charts/";
  tester.doForEachPyretFile(it, "chart", chartTestsBase, function(programText, testObj) {
    tester.runAndCheckAllTestsPassed(programText, testObj.browser, testObj.test, 900000);
  }, 900000);

});
