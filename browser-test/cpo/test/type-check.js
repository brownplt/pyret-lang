var tester = require("../test-util/util.js");
var fs = require("fs");

describe("Running with type-check mode", function() {
  before(tester.setupMulti("Type checking"));
  after(tester.teardownMulti);

  function fileTest(name, expected) {
    var path = "test-util/pyret-programs/types/" + name + ".arr";
    return [name, fs.readFileSync(path), expected];
  }

  var replTest = [
    ["string-dict-import", "import string-dict as SD",
      [["x = SD.make-mutable-string-dict()", ""],
       ["x.set-now('a', 5)", ""],
       ["x", "5"]]],
    ["string-dict-include", "include string-dict",
      [["x = make-mutable-string-dict()", ""],
       ["x.set-now('a', 12)", ""],
       ["x", "12"]]],
    ["beside-list", "include image",
      [["a = beside-list([list: circle(50, 'solid', 'red')])", ""],
       ["image-width(a)", "100"]]],
  ];

  replTest.forEach(function(t) {
    tester.testRunAndUseRepl(it, t[0], t[1], t[2], { typeCheck: true });
  });

});
