var tester = require("../test-util/util.js");
var fs = require("fs");

describe("Rendering check blocks", function() {
  before(tester.setupMulti("Rendering check blocks"));
  after(tester.teardownMulti);

  function fileTest(name, expected) {
    var path = "test-util/pyret-programs/check-blocks/" + name + ".arr";
    return [name, fs.readFileSync(path), expected];
  }

  var tests = [
    ["simple", "check: 1 is 2 end", [[["failed"]]]],
    ["simple2", "check: 1 is 2\n3 is 4 end", [[["failed"], ["failed"]]]],
    ["simple3", "check: 1 is 2 end\ncheck: 3 is 4 end", [[["failed"]],[["failed"]]]],

    ["pass-then-fail", "check: 1 is 1\n1 is 2 end", [[["Passed"], ["failed"]]]],

    ["satisfies0", "check: 5 satisfies {(x): raise('nope')} end", [[["nope"]]]],
    
    ["doesNotRaise0", "check: raise('nope') does-not-raise end", [[["nope"]]]],

    ["is-pass",               "check: 2 is 2 end",                                [[["Passed"]]]],
    ["is-fail",               "check: 2 is 3 end",                                [[["failed"]]]],
    ["is-not-fail",           "check: 2 is-not 2 end",                            [[["failed"]]]],
    ["is-not-pass",           "check: 2 is-not 3 end",                            [[["Passed"]]]],
    ["is-op-fail",            "check: 2 is%(_ < _) 2 end",                        [[["failed"]]]],
    ["is-op-pass",            "check: 2 is%(_ < _) 3 end",                        [[["Passed"]]]],
    ["is-not-op-pass",        "check: 2 is-not%(_ < _) 2 end",                    [[["Passed"]]]],
    ["is-not-op-fail",        "check: 2 is-not%(_ < _) 3 end",                    [[["failed"]]]],
    ["satisfies-pass",        "check: 2 satisfies _ < 3 end",                     [[["Passed"]]]],
    ["satisfies-fail",        "check: 2 satisfies _ < 2 end",                     [[["failed"]]]],
    ["violates-fail",         "check: 2 violates  _ < 3 end",                     [[["failed"]]]],
    ["violates-pass",         "check: 2 violates  _ < 2 end",                     [[["Passed"]]]],
    ["raises-pass",           "check: raise('ab') raises 'b' end",                [[["Passed"]]]],
    ["raises-fail",           "check: raise('ab') raises 'c' end",                [[["unexpected exception"]]]],
    ["raises-other-than-fail","check: raise('ab') raises-other-than 'b' end",     [[["Got exception"]]]],
    ["raises-other-than-pass","check: raise('ab') raises-other-than 'c' end",     [[["Passed"]]]],
    ["does-not-raise-fail",   "check: raise('') does-not-raise end",              [[["\"\""]]]],
    ["does-not-raise-pass",   "check: '' does-not-raise end",                     [[["Passed"]]]],
    ["raises-satisfies-pass", "check: raise('a') raises-satisfies _ == 'a' end",  [[["Passed"]]]],
    ["raises-satisfies-fail", "check: raise('a') raises-satisfies _ == 'b' end",  [[["failed"]]]],
    ["raises-violates-fail",  "check: raise('a') raises-violates _ == 'a' end",   [[["failed"]]]],
    ["raises-violates-pass",  "check: raise('a') raises-violates _ == 'b' end",   [[["Passed"]]]],

    fileTest("deep-recursion-in-first-test", [
      [["2001"], ["failed"], ["failed"]]
    ])
  ];

  tests.forEach(function(t) {
    tester.testRunsAndHasCheckBlocks(it, t[0], t[1], t[2]);
  });

});
