var tester = require("../test-util/util.js");

describe("Running Image programs", function() {
  beforeEach(tester.setup);
  afterEach(tester.teardown);

  var imageTestsBase = "./test-util/pyret-programs/images/";
  tester.doForEachPyretFile(it, "image", imageTestsBase, function(programText, testObj) {
    programText = programText.replace("BASE_URL", "\"" + process.env["BASE_URL"] + "\"");
    tester.runAndCheckAllTestsPassed(programText, testObj.browser, testObj.test, 900000);
  }, 900000);

});

