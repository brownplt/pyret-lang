var tester = require("../test-util/util.js");

describe("Running Tables programs", function() {
  beforeEach(tester.setup);
  afterEach(tester.teardown);

  var tablesTestsBase = "./test-util/pyret-programs/tables/";
  tester.doForEachPyretFile(it, "tables", tablesTestsBase, function(programText, testObj) {
    tester.checkTableRendersCorrectly(programText, testObj.browser, testObj.test, 900000);
  }, 900000);
});
