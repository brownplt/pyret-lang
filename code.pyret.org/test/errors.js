var tester = require("../test-util/util.js");
var fs = require("fs");

describe("Rendering errors", function() {
  before(tester.setupMulti("Rendering errors"));
  after(tester.teardownMulti);

  function fileTest(name, expected) {
    var path = "test-util/pyret-programs/errors/" + name + ".arr";
    return [name, fs.readFileSync(path), expected];
  }

  var tests = [

// NOTE(joe Sep 2017): Removing these _plus tests because it's not clear what
// should happen for errors on this semi-internal interface. It would be good
// to have an answer for this, though

//    ["renderSimpleReasonSimple", "_plus(1, 'x')", "(definitions://, 1, 0, 0, 1, 13, 13)"],
//    ["renderSimpleReasonError",  "_plus(1, 'x')", "An error occurred rendering the reason"],

    ["field-not-found", "{}.x", "did not have a field"],
    ["lookup-non-object", "5.x", "not an object"],
    ["lookup-constructor-not-object", "data D: d() end\n d.x", "was a constructor"],
    ["update-non-obj", "5!{x : 10}", "reference update expression"],
    ["update-non-ref", "{x:5}!{x : 10}", "is not a mutable reference"],
    ["update-non-existent-field", "{x:5}!{y : 10}", "does not exist"],
    ["no-cases-matched", "cases(List) link(1, empty): | empty => true end", "matched the value"],
    ["no-branches-matched", "if 1 == 2: 5 else if 3 == 4: 6 end", "the condition of at least one branch be satisfied"],
    ["template-not-finished", "fun f(): ... end\nf()", "Template expressions cannot be evaluated."],
    ["template-not-finished", "fun f(): ... end\nf()", "Template expressions cannot be evaluated."],
    ["lookup-non-tuple", "5.{1}", "was not a tuple value"],
    ["lookup-large-index", "{1;2}.{3}", "given position"],

    ["type-id-used-as-value", "data D: d(x) end\nmy-x = D.x", "But it is defined as a type"],

    ["images-equal-preds", "include image\nimages-equal(5, 'a')", "failed because the 1ˢᵗ argument evaluated to an unexpected value"],
    ["text-preds", "include image\ntext('a', 'a', 'blue')", "failed because the 2ⁿᵈ argument evaluated to an unexpected value"],
    ["text-font-preds", "include image\ninclude image-structs\ntext-font('a', 5, blue, 'times', true, 'italic', 'bold', true)", "failed because the 5ᵗʰ argument evaluated to an unexpected value"],

    ["regular-polygon", "include image\nregular-polygon(20, 1/2, 'solid', 'blue')", "failed because the 2ⁿᵈ argument evaluated to an unexpected value"],

    // TODO(joe): Need a better way to close the world window when this is done
//    ["arity-on-world-callback", "import world as W\nW.big-bang('a', [list: W.on-tick(lam(x, y): x end)])", "defined accepting 2 arguments"],

    fileTest("deeply-recursive-field-not-found", "did not have a field"),

    ["is-pass1",                       "check: 3 is 3 end", [[["Passed"]]]],
    ["is-pass2",                       "check: 3 is 3 because 3 end", [[["Passed"]]]],
    ["is-fail1",                       "check: 3 is 4 end", [[["The test operator is failed", "left side", "right side", "3", "4"]]]],
    ["is-fail2",                       "check: 3 is 5 because 5 end", [[["The test operator is failed", "left side", "right side", "3", "5"]]]],
    ["is-fail3",                       "check: is-number is is-number end", [[["The test operator is failed", "two Functions for equality", "left side", "right side", "<function:is-number>", "call them first?"]]]],
    ["is-fail4",                       "check: ~3 is ~4 end", [[["The test operator is failed", "two Roughnums for equality", "left side", "right side", "~3", "~4"]]]],
    ["is-fail5",                       "check: ~3 is ~4 because ~4 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~4", "~4"]]]],
    ["is-because-fail1",               "check: 3 is 3 because 5 end", [[["The test was inconsistent", "right side", "explanation", "5", "3"]]]],
    ["is-because-fail2",               "check: 3 is 4 because 5 end", [[["The test was inconsistent", "right side", "explanation", "5", "4"]]]],
    ["is-because-fail3",               "check: 3 is 5 because is-number end", [[["The test was inconsistent", "explanation", "right side", "<function:is-number>", "5"]]]],
    ["is-because-fail4",               "check: 3 is is-number because 5 end", [[["The test was inconsistent", "explanation", "right side", "<function:is-number>", "5"]]]],
    ["is-because-fail5",               "check: 3 is is-string because is-number end", [[["The test was inconsistent", "two Functions for equality", "explanation", "right side", "<function:is-number>", "<function:is-string>", "call them first?"]]]],
    ["is-because-fail6",               "check: ~3 is ~4 because ~5 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~5", "~4"]]]],
                                       
    ["is==-pass1",                     "check: 3 is== 3 end", [[["Passed"]]]],
    ["is==-pass2",                     "check: 3 is== 3 because 3 end", [[["Passed"]]]],
    ["is==-fail1",                     "check: 3 is== 4 end", [[["The test operator is== failed", "left side", "right side", "3", "4"]]]],
    ["is==-fail2",                     "check: 3 is== 5 because 5 end", [[["The test operator is== failed", "left side", "right side", "3", "5"]]]],
    ["is==-fail3",                     "check: is-number is== is-number end", [[["The test operator is== failed", "two Functions for equality", "left side", "right side", "<function:is-number>", "call them first?"]]]],
    ["is==-fail4",                     "check: ~3 is== ~4 end", [[["The test operator is== failed", "two Roughnums for equality", "left side", "right side", "~3", "~4"]]]],
    ["is==-fail5",                     "check: ~3 is== ~4 because ~4 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~4", "~4"]]]],
    ["is==-because-fail1",             "check: 3 is== 3 because 5 end", [[["The test was inconsistent", "right side", "explanation", "5", "3"]]]],
    ["is==-because-fail2",             "check: 3 is== 4 because 5 end", [[["The test was inconsistent", "right side", "explanation", "5", "4"]]]],
    ["is==-because-fail3",             "check: 3 is== 5 because is-number end", [[["The test was inconsistent", "explanation", "right side", "<function:is-number>", "5"]]]],
    ["is==-because-fail4",             "check: 3 is== is-number because 5 end", [[["The test was inconsistent", "explanation", "right side", "<function:is-number>", "5"]]]],
    ["is==-because-fail5",             "check: 3 is== is-string because is-number end", [[["The test was inconsistent", "two Functions for equality", "explanation", "right side", "<function:is-number>", "<function:is-string>", "call them first?"]]]],
    ["is==-because-fail6",             "check: ~3 is== ~4 because ~5 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~5", "~4"]]]],
                                       
    ["is=~-pass1",                     "check: 3 is=~ 3 end", [[["Passed"]]]],
    ["is=~-pass2",                     "check: 3 is=~ 3 because 3 end", [[["Passed"]]]],
    ["is=~-fail1",                     "check: 3 is=~ 4 end", [[["The test operator is=~ failed", "left side", "right side", "3", "4"]]]],
    ["is=~-fail2",                     "check: 3 is=~ 5 because 5 end", [[["The test operator is=~ failed", "left side", "right side", "3", "5"]]]],
    ["is=~-fail3",                     "check: is-number is=~ is-number end", [[["The test operator is=~ failed", "two Functions for equality", "left side", "right side", "<function:is-number>", "call them first?"]]]],
    ["is=~-fail4",                     "check: ~3 is=~ ~4 end", [[["The test operator is=~ failed", "two Roughnums for equality", "left side", "right side", "~3", "~4"]]]],
    ["is=~-fail5",                     "check: ~3 is=~ ~4 because ~4 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~4", "~4"]]]],
    ["is=~-because-fail1",             "check: 3 is=~ 3 because 5 end", [[["The test was inconsistent", "right side", "explanation", "5", "3"]]]],
    ["is=~-because-fail2",             "check: 3 is=~ 4 because 5 end", [[["The test was inconsistent", "right side", "explanation", "5", "4"]]]],
    ["is=~-because-fail3",             "check: 3 is=~ 5 because is-number end", [[["The test was inconsistent", "explanation", "right side", "<function:is-number>", "5"]]]],
    ["is=~-because-fail4",             "check: 3 is=~ is-number because 5 end", [[["The test was inconsistent", "explanation", "right side", "<function:is-number>", "5"]]]],
    ["is=~-because-fail5",             "check: 3 is=~ is-string because is-number end", [[["The test was inconsistent", "two Functions for equality", "explanation", "right side", "<function:is-number>", "<function:is-string>", "call them first?"]]]],
    ["is=~-because-fail6",             "check: ~3 is=~ ~4 because ~5 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~5", "~4"]]]],
                                       
    ["is<=>-pass1",                    "check: 3 is<=> 3 end", [[["Passed"]]]],
    ["is<=>-pass2",                    "check: 3 is<=> 3 because 3 end", [[["Passed"]]]],
    ["is<=>-fail1",                    "check: 3 is<=> 4 end", [[["The test operator is<=> failed", "left side", "right side", "3", "4"]]]],
    ["is<=>-fail2",                    "check: 3 is<=> 5 because 5 end", [[["The test operator is<=> failed", "left side", "right side", "3", "5"]]]],
    ["is<=>-fail3",                    "check: is-number is<=> is-number end", [[["The test operator is<=> failed", "two Functions for equality", "left side", "right side", "<function:is-number>", "call them first?"]]]],
    ["is<=>-fail4",                    "check: ~3 is<=> ~4 end", [[["The test operator is<=> failed", "two Roughnums for equality", "left side", "right side", "~3", "~4"]]]],
    ["is<=>-fail5",                    "check: ~3 is<=> ~4 because ~4 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~4", "~4"]]]],
    ["is<=>-because-fail1",            "check: 3 is<=> 3 because 5 end", [[["The test was inconsistent", "right side", "explanation", "5", "3"]]]],
    ["is<=>-because-fail2",            "check: 3 is<=> 4 because 5 end", [[["The test was inconsistent", "right side", "explanation", "5", "4"]]]],
    ["is<=>-because-fail3",            "check: 3 is<=> 5 because is-number end", [[["The test was inconsistent", "explanation", "right side", "<function:is-number>", "5"]]]],
    ["is<=>-because-fail4",            "check: 3 is<=> is-number because 5 end", [[["The test was inconsistent", "explanation", "right side", "<function:is-number>", "5"]]]],
    ["is<=>-because-fail5",            "check: 3 is<=> is-string because is-number end", [[["The test was inconsistent", "two Functions for equality", "explanation", "right side", "<function:is-number>", "<function:is-string>", "call them first?"]]]],
    ["is<=>-because-fail6",            "check: ~3 is<=> ~4 because ~5 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~5", "~4"]]]],
                                       
                                       
    ["is-roughly-pass1",               "check: 3 is-roughly ~3 end", [[["Passed"]]]],
    ["is-roughly-pass2",               "check: 3 is-roughly 3 because 3 end", [[["Passed"]]]],
    ["is-roughly-pass3",               "check: 3 is-roughly ~3 end", [[["Passed"]]]],
    ["is-roughly-pass4",               "check: 3 is-roughly 3 because ~3 end", [[["Passed"]]]],
    ["is-roughly-fail1",               "check: 3 is-roughly 4 end", [[["The test operator is-roughly failed", "left side", "right side", "3", "4", "(allowing for rough equality)"]]]],
    ["is-roughly-fail2",               "check: 3 is-roughly 5 because 5 end", [[["The test operator is-roughly failed", "left side", "right side", "3", "5"]]]],
    ["is-roughly-fail3",               "check: is-string is-roughly is-number end", [[["The test operator is-roughly failed", "two Functions for equality", "left side", "right side", "<function:is-number>", "<function:is-string>", "call them first?"]]]],
    ["is-roughly-because-fail1",       "check: 3 is-roughly 3 because 5 end", [[["The test was inconsistent", "right side", "explanation", "5", "3"]]]],
    ["is-roughly-because-fail2",       "check: 3 is-roughly 4 because 5 end", [[["The test was inconsistent", "right side", "explanation", "5", "4"]]]],
    ["is-roughly-because-fail3",       "check: 3 is-roughly 5 because is-number end", [[["The test was inconsistent", "explanation", "right side", "<function:is-number>", "5"]]]],
    ["is-roughly-because-fail4",       "check: 3 is-roughly is-number because 5 end", [[["The test was inconsistent", "explanation", "right side", "<function:is-number>", "5"]]]],
    ["is-roughly-because-fail5",       "check: 3 is-roughly is-string because is-number end", [[["The test was inconsistent", "two Functions for equality", "explanation", "right side", "<function:is-number>", "<function:is-string>", "call them first?"]]]],
                                       
    ["is-not-pass1",                   "check: 3 is-not 4 end", [[["Passed"]]]],
    ["is-not-pass2",                   "check: 3 is-not 4 because 5 end", [[["Passed"]]]],
    ["is-not-pass3",                   "check: 3 is-not 5 because is-number end", [[["Passed"]]]],
    ["is-not-pass4",                   "check: 3 is-not is-number because 5 end", [[["Passed"]]]],
    ["is-not-fail1",                   "check: 3 is-not 3 end", [[["The test operator is-not failed", "left side", "right side", "3"]]]],
    ["is-not-fail2",                   "check: 3 is-not 3 because 5 end", [[["The test operator is-not failed", "left side", "right side", "3"]]]],
    ["is-not-because-fail1",           "check: 3 is-not 5 because 5 end", [[["The test was inconsistent", "right side", "explanation", "5"]]]],
                                       
                                       
    ["is-not==-pass",                  "check: 3 is-not== 4 end", [[["Passed"]]]],
    ["is-not==-fail1",                 "check: 3 is-not== 3 end", [[["The test operator is-not== failed", "left side", "right side", "3"]]]],
    ["is-not==-fail2",                 "check: 3 is-not== 3 because 5 end", [[["The test operator is-not== failed", "left side", "right side", "3"]]]],
    ["is-not==-fail3",                 "check: is-number is-not== is-number end", [[["The test operator is-not== failed", "two Functions for equality", "left side", "right side", "<function:is-number>", "call them first?"]]]],
    ["is-not==-fail4",                 "check: ~3 is-not== ~4 end", [[["The test operator is-not== failed", "two Roughnums for equality", "left side", "right side", "~3", "~4"]]]],
    ["is-not==-fail5",                 "check: ~3 is-not== ~4 because ~4 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~4", "~4"]]]],
    ["is-not==-because-fail1",         "check: 3 is-not== 3 because 3 end", [[["The test was inconsistent", "right side", "explanation", "3"]]]],
    ["is-not==-because-fail2",         "check: 3 is-not== 4 because 4 end", [[["The test was inconsistent", "right side", "explanation", "4"]]]],
    ["is-not==-because-fail3",         "check: 3 is-not== is-string because is-number end", [[["The test was inconsistent", "two Functions for equality", "explanation", "right side", "<function:is-number>", "<function:is-string>", "call them first?"]]]],
    ["is-not==-because-fail4",         "check: ~3 is-not== ~4 because ~5 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~5", "~4"]]]],
                                       
    ["is-not=~-pass1",                 "check: 3 is-not=~ 4 end", [[["Passed"]]]],
    ["is-not=~-pass2",                 "check: 3 is-not=~ 4 because 5 end", [[["Passed"]]]],
    ["is-not=~-fail1",                 "check: 3 is-not=~ 3 end", [[["The test operator is-not=~ failed", "left side", "right side", "3"]]]],
    ["is-not=~-fail2",                 "check: 3 is-not=~ 3 because 4 end", [[["The test operator is-not=~ failed", "left side", "right side", "3"]]]],
    ["is-not=~-fail3",                 "check: is-number is-not=~ is-number end", [[["The test operator is-not=~ failed", "two Functions for equality", "left side", "right side", "<function:is-number>", "call them first?"]]]],
    ["is-not=~-fail4",                 "check: ~3 is-not=~ ~4 end", [[["The test operator is-not=~ failed", "two Roughnums for equality", "left side", "right side", "~3", "~4"]]]],
    ["is-not=~-fail5",                 "check: ~3 is-not=~ ~4 because ~4 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~4", "~4"]]]],
    ["is-not=~-because-fail1",         "check: 3 is-not=~ 3 because 3 end", [[["The test was inconsistent", "right side", "explanation", "3"]]]],
    ["is-not=~-because-fail2",         "check: 3 is-not=~ 4 because 4 end", [[["The test was inconsistent", "right side", "explanation", "4"]]]],
    ["is-not=~-because-fail3",         "check: 3 is-not=~ is-string because is-number end", [[["The test was inconsistent", "two Functions for equality", "explanation", "right side", "<function:is-number>", "<function:is-string>", "call them first?"]]]],
    ["is-not=~-because-fail4",         "check: ~3 is-not=~ ~4 because ~5 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~5", "~4"]]]],
                                       
    ["is-not<=>-pass1",                "check: 3 is-not<=> 4 end", [[["Passed"]]]],
    ["is-not<=>-pass2",                "check: 3 is-not<=> 4 because 5 end", [[["Passed"]]]],
    ["is-not<=>-fail1",                "check: 3 is-not<=> 3 end", [[["The test operator is-not<=> failed", "left side", "right side", "3"]]]],
    ["is-not<=>-fail2",                "check: 3 is-not<=> 3 because 5 end", [[["The test operator is-not<=> failed", "left side", "right side", "3"]]]],
    ["is-not<=>-fail3",                "check: is-number is-not<=> is-number end", [[["The test operator is-not<=> failed", "two Functions for equality", "left side", "right side", "<function:is-number>", "call them first?"]]]],
    ["is-not<=>-fail4",                "check: ~3 is-not<=> ~4 end", [[["The test operator is-not<=> failed", "two Roughnums for equality", "left side", "right side", "~3", "~4"]]]],
    ["is-not<=>-fail5",                "check: ~3 is-not<=> ~4 because ~4 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~4", "~4"]]]],
    ["is-not<=>-because-fail1",        "check: 3 is-not<=> 3 because 3 end", [[["The test was inconsistent", "right side", "explanation", "3"]]]],
    ["is-not<=>-because-fail2",        "check: 3 is-not<=> 4 because 4 end", [[["The test was inconsistent", "right side", "explanation", "4"]]]],
    ["is-not<=>-because-fail3",        "check: 3 is-not<=> is-string because is-number end", [[["The test was inconsistent", "two Functions for equality", "explanation", "right side", "<function:is-number>", "<function:is-string>", "call them first?"]]]],
    ["is-not<=>-because-fail4",        "check: ~3 is-not<=> ~4 because ~5 end", [[["The test was inconsistent", "two Roughnums for equality", "explanation", "right side", "~5", "~4"]]]],

    ["satisfies-pass1",                "check: 3 satisfies is-number end", [[["Passed"]]]],
    ["satisfies-pass2",                "check: 3 satisfies is-number because 5 end", [[["Passed"]]]],
    ["satisfies-fail1",                "check: 3 satisfies is-string because 'hi' end", [[["The test operator satisfies failed", "predicate", "left side", "3"]]]],
    ["satisfies-fail2",                "check: 3 satisfies 5 end", [[["1-argument function that returns a boolean", "5"]]]],
    ["satisfies-fail3",                "check: 3 satisfies 5 because 6 end", [[["1-argument function that returns a boolean", "5"]]]],
    ["satisfies-fail4",                "check: 3 satisfies num-sqr end", [[["test predicate", "must return a boolean", "9", "left side"]]]],
    ["satisfies-fail5",                "check: 3 satisfies lam(_): 1 / 0 end end", [[["The test operator satisfies failed", "predicate", "exception", "division by zero"]]]],
    ["satisfies-fail6",                "check: 3 satisfies lam(_, _): 1 / 0 end end", [[["test predicate", "1-argument function that returns a boolean", "<function:anonymous>"]]]],
    ["satisfies-because-fail1",        "check: 3 satisfies is-number because 'hi' end", [[["The test was inconsistent", "predicate", "explanation", "\"hi\""]]]],
    ["satisfies-because-fail2",        "check: 3 satisfies num-sqr because 6 end", [[["test predicate", "must return a boolean", "36", "explanation"]]]],
    ["satisfies-because-fail3",        "check: 3 satisfies lam(_): 1 / 0 end because 4 end", [[["The test operator satisfies failed", "exception", "predicate", "division by zero"]]]],
    ["satisfies-because-fail4",        "check: 3 satisfies lam(_, _): 1 / 0 end because 4 end", [[["test predicate", "1-argument function that returns a boolean", "<function:anonymous>"]]]],
                                       
    ["violates-pass1",                 "check: 3 violates is-string end", [[["Passed"]]]],
    ["violates-pass2",                 "check: 3 violates is-string because 5 end", [[["Passed"]]]],
    ["violates-fail1",                 "check: 3 violates is-number because 'hi' end", [[["The test operator violates failed", "predicate", "left side", "3"]]]],
    ["violates-fail2",                 "check: 3 violates 5 end", [[["1-argument function that returns a boolean", "5"]]]],
    ["violates-fail3",                 "check: 3 violates 5 because 6 end", [[["1-argument function that returns a boolean", "5"]]]],
    ["violates-fail4",                 "check: 3 violates num-sqr end", [[["test predicate", "must return a boolean", "left side"]]]],
    ["violates-fail5",                 "check: 3 violates lam(_): 1 / 0 end end", [[["The test operator violates failed", "exception", "predicate", "division by zero"]]]],
    ["violates-fail6",                 "check: 3 violates lam(_, _): 1 / 0 end end", [[["test predicate", "1-argument function that returns a boolean", "<function:anonymous>"]]]],
    ["violates-because-fail1",         "check: 3 violates is-string because 'hi' end", [[["The test was inconsistent", "predicate", "explanation", "\"hi\""]]]],
    ["violates-because-fail2",         "check: 3 violates num-sqr because 6 end", [[["test predicate", "must return a boolean", "explanation"]]]],
    ["violates-because-fail3",         "check: 3 violates lam(_): 1 / 0 end because 4 end", [[["The test operator violates failed", "exception", "predicate", "division by zero"]]]],
    ["violates-because-fail4",         "check: 3 violates lam(_, _): 1 / 0 end because 4 end", [[["test predicate", "1-argument function that returns a boolean", "<function:anonymous>"]]]],
                                       
    ["raises-pass1",                   "check: num-sqrt(-1) raises 'NumNonNegative' end", [[["Passed"]]]],
    ["raises-pass2",                   "check: num-sqrt(-1) raises 'NumNonNegative' because num-sqrt(true) end", [[["Passed"]]]],
    ["raises-fail1",                   "check: num-sqrt(-1) raises 'oops' end", [[["The test operator raises failed", "oops", "unexpected exception", "-1"]]]],
    ["raises-fail2",                   "check: num-sqrt(-1) raises 'oops' because num-sqrt(-1) end", [[["The test was inconsistent", "oops", "unexpected exception", "-1"]]]],
    ["raises-fail3",                   "check: num-sqrt(-1) raises 'NumNonNegative' because num-sqrt(4) end", [[["The test was inconsistent", "NumNonNegative", "No exception raised", "explanation"]]]],
    ["raises-fail4",                   "check: num-sqrt(4) raises 'NumNonNegative' because num-sqrt(-1) end", [[["The test operator raises failed", "NumNonNegative", "No exception raised", "left side"]]]],
                                       
    ["does-not-raise-pass1",           "check: num-sqrt(1) does-not-raise end", [[["Passed"]]]],
    ["does-not-raise-pass2",           "check: num-sqrt(1) does-not-raise because num-sqrt(1) end", [[["Passed"]]]],
    ["does-not-raise-fail1",           "check: num-sqrt(-1) does-not-raise end", [[["The test operator does-not-raise failed", "did not expect", "left side", "raise an exception", "-1"]]]],
    ["does-not-raise-fail2",           "check: num-sqrt(-1) does-not-raise because num-sqrt(-1) end", [[["The test was inconsistent", "did not expect" ,"explanation", "raise an exception", "-1"]]]],
                                       
    ["raises-other-than-pass1",        "check: num-sqrt(-1) raises-other-than 'oops' end", [[["Passed"]]]],
    ["raises-other-than-pass2",        "check: num-sqrt(-1) raises-other-than 'oops' because num-sqrt(true) end", [[["Passed"]]]],
    ["raises-other-than-fail1",        "check: num-sqrt(-1) raises-other-than 'NumNonNegative' end", [[["The test operator raises-other-than failed", "NumNonNegative", "expected it not to contain", "-1"]]]],
    ["raises-other-than-fail2",        "check: num-sqrt(-1) raises-other-than 'NumNonNegative' because num-sqrt(-1) end", [[["The test was inconsistent", "NumNonNegative", "expected it not to contain", "-1"]]]],
    ["raises-other-than-fail3",        "check: num-sqrt(-1) raises-other-than 'oops' because num-sqrt(4) end", [[["The test was inconsistent", "oops", "No exception raised", "explanation", "4"]]]],
    ["raises-other-than-fail4",        "check: num-sqrt(4) raises-other-than 'oops' because num-sqrt(-1) end", [[["The test operator raises-other-than failed", "oops", "No exception raised", "left side", "4"]]]],

    ["raises-satisfies-pass1",         "include error\ncheck: 1 / 0 raises-satisfies is-message-exception end", [[["Passed"]]]],
    ["raises-satisfies-pass2",         "include error\ncheck: 1 / 0 raises-satisfies is-message-exception because num-modulo(3, 0) end", [[["Passed"]]]],
    ["raises-satisfies-pass3",         "check: raise(true) raises-satisfies print end", [[["Passed"]]]],
    ["raises-satisfies-fail1",         "check: 3 raises-satisfies is-number end", [[["No exception raised"]]]],
    ["raises-satisfies-fail2",         "check: 1 / 0 raises-satisfies is-number end", [[["The test operator raises-satisfies failed", "predicate", "left side", "division by zero"]]]],
    ["raises-satisfies-fail3",         "check: raise('hi') raises-satisfies 5 end", [[["1-argument function that returns a boolean", "5"]]]],
    ["raises-satisfies-fail4",         "check: raise('hi') raises-satisfies print end", [[["test predicate", "must return a boolean", "left side", "\"hi\""]]]],
    ["raises-satisfies-fail5",         "check: raise('hi') raises-satisfies num-modulo end", [[["1-argument function that returns a boolean", "num-modulo"]]]],
    ["raises-satisfies-fail6",         "check: raise(false) raises-satisfies print end", [[["The test operator raises-satisfies failed", "predicate", "left side", "false"]]]],
    ["raises-satisfies-because-fail1", "check: 3 raises-satisfies is-number because 'hi' end", [[["The test was inconsistent", "No exception raised", "explanation"]]]],
    ["raises-satisfies-because-fail2", "check: 3 raises-satisfies is-number because 1 / 0 end", [[["The test was inconsistent", "predicate", "explanation", "division by zero"]]]],
    ["raises-satisfies-because-fail3", "check: 3 raises-satisfies num-sqr because 1 / 0 end", [[["The test operator raises-satisfies failed", "right side", "exception", "division by zero", "num-sqr"]]]],
    ["raises-satisfies-because-fail4", "check: 3 raises-satisfies 5 because raise('hi') end", [[["1-argument function that returns a boolean", "5"]]]],
    ["raises-satisfies-because-fail5", "check: 3 raises-satisfies print because raise('hi') end", [[["test predicate", "must return a boolean", "explanation", "\"hi\""]]]],
    ["raises-satisfies-because-fail6", "check: 3 raises-satisfies num-modulo because raise('hi') end", [[["1-argument function that returns a boolean", "num-modulo"]]]],
    ["raises-satisfies-because-fail7", "check: 3 raises-satisfies print because raise(false) end", [[["The test was inconsistent", "predicate", "explanation", "false"]]]],

    ["raises-violates-pass1",          "include error\ncheck: 1 / 0 raises-violates is-user-exception end", [[["Passed"]]]],
    ["raises-violates-pass2",          "include error\ncheck: 1 / 0 raises-violates is-user-exception because num-modulo(3, 0) end", [[["Passed"]]]],
    ["raises-violates-pass3",          "check: raise(false) raises-violates print end", [[["Passed"]]]],
    ["raises-violates-fail1",          "check: 3 raises-violates is-number end", [[["No exception raised"]]]],
    ["raises-violates-fail2",          "include error\ncheck: 1 / 0 raises-violates is-message-exception end", [[["The test operator raises-violates failed", "predicate", "left side", "division by zero"]]]],
    ["raises-violates-fail3",          "check: raise('hi') raises-violates 5 end", [[["1-argument function that returns a boolean", "5"]]]],
    ["raises-violates-fail4",          "check: raise('hi') raises-violates print end", [[["test predicate", "must return a boolean", "left side", "\"hi\""]]]],
    ["raises-violates-fail5",          "check: raise('hi') raises-violates num-modulo end", [[["1-argument function that returns a boolean", "num-modulo"]]]],
    ["raises-violates-fail6",          "check: raise(true) raises-violates print end", [[["The test operator raises-violates failed", "predicate", "left side", "true"]]]],
    ["raises-violates-because-fail1",  "check: 3 raises-violates is-number because 'hi' end", [[["The test was inconsistent", "No exception raised", "explanation"]]]],
    ["raises-violates-because-fail2",  "include error\ncheck: 3 raises-violates is-message-exception because 1 / 0 end", [[["The test was inconsistent", "predicate", "explanation", "division by zero"]]]],
    ["raises-violates-because-fail3",  "check: 3 raises-violates num-sqr because 1 / 0 end", [[["The test operator raises-violates failed", "right side", "exception", "division by zero"]]]],
    ["raises-violates-because-fail4",  "check: 3 raises-violates 5 because raise('hi') end", [[["1-argument function that returns a boolean", "5"]]]],
    ["raises-violates-because-fail5",  "check: 3 raises-violates print because raise('hi') end", [[["test predicate", "must return a boolean", "explanation", "\"hi\""]]]],
    ["raises-violates-because-fail6",  "check: 3 raises-violates num-modulo because raise('hi') end", [[["1-argument function that returns a boolean", "num-modulo"]]]],
    ["raises-violates-because-fail7",  "check: 3 raises-violates print because raise(true) end", [[["The test was inconsistent", "predicate", "explanation", "true"]]]],

    // Type-check error whose render-fancy-reason calls `self.variant.fields.count()`
    // on a List (compile-structs.arr:2464) -- List has no `count`, so the .arr
    // compiler's fancy renderer crashes. The editor renders compile errors fancy
    // (error-ui.js getFancyRenderer else-branch), so this exercises that path.
    // load-table duplicate-sanitizer: fancy reason renders the offending clause's
    // pretty source (was passing a list to `text`, crashing the fancy renderer in
    // both compilers). Asserting the pretty source appears proves it renders now.
    ["load-table-dup-sanitizer",
     "load-table: h1, h2\n  source: src1\n  sanitize h1 using s1\n  sanitize h2 using s2\n  sanitize h1 using s1\nend",
     "is already sanitized by the sanitizer sanitize h1 using s1"],

    ["incorrect-number-of-bindings",
     "data D:\n  | d(a :: Number, b :: Number)\nend\nfun f(x :: D) -> Number:\n  cases(D) x:\n    | d(a) => 1\n  end\nend",
     "same number of field bindings",
     {typeCheck: true}],
  ];
    
  tests.forEach(function(t) {
    if (t[2] instanceof Array)
      tester.testRunsAndHasCheckBlocks(it, t[0], t[1], t[2], t[3]);
    else
      tester.testErrorRendersString(it, t[0], t[1], t[2], t[3]);
  });

});
