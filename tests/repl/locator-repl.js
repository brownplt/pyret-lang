define([
    "q",
    "js/runtime-anf",
    "./../evaluator/eval-matchers",
    "../../src/js/base/locator-repl-lib",
    "compiler/compile-structs.arr",
    "trove/string-dict"
  ], function(q, rtLib, e, repl, csLib, stringDictLib) {

  require('jasmine-node');

  var rt, P, builtins, sd, emptyStringDict;

  function performTest() {

    beforeEach(function(done) {
      rt = rtLib.makeRuntime({});
      P = e.makeEvalCheckers(this, rt);

      rt.loadModules(rt.namespace, [csLib, stringDictLib], function(cs, stringDict) {
        builtins = rt.getField(cs, "standard-builtins");
        sd = stringDict;
        emptyStringDict = rt.getField(sd, "make-string-dict").app();
        done();
      });
    });

    describe("basic repl", function() {
      it("should run definitions", function(done) {
        var currentCode = "5";
        function getCurrentCode() {
          return currentCode;
        }
        var r = repl.create(rt, rt.namespace, builtins, getCurrentCode, "<compile-context>", emptyStringDict);
        r.then(function(r) {
          return r.restartInteractions();
        }).then(function(answer) {
          // TODO(joe): This is sort of a stupidly long way of getting at the answer
          var innerAnswer = rt.getField(answer.result, "result").val.result.dict.answer;
          console.log("Got an answer: ", innerAnswer);
          expect(innerAnswer).toBe(5);
          done();
        }).fail(function(e) {
          expect(false).toBe(true);
          console.error("Got an error: ", e, e.stack, rt.isCont(e));
        });
      });

    });

  }

  return { performTest: performTest };

});
