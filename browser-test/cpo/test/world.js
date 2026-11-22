var tester = require("../test-util/util.js");

describe("Running world programs", function() {
  beforeEach(tester.setup);
  afterEach(tester.teardown);

  var worldTestsBase = "./test-util/pyret-programs/world/";
  tester.doForEachPyretFile(it, "world", worldTestsBase, function(programText, testObj) {
    tester.checkWorldProgramRunsCleanly(programText, testObj.browser, testObj.test, 900000);
  }, 900000);

});
