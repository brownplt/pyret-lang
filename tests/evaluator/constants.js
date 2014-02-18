var r = require("requirejs")
define(["js/runtime-anf", "./eval-matchers"], function(rtLib, e) {

  var _ = require('jasmine-node');
  var rt;
  var P;
  var same;

  function performTest() {

    beforeEach(function() {
      rt = rtLib.makeRuntime({ stdout: function(str) { process.stdout.write(str); } });
      P =  e.makeEvalCheckers(this, rt);
      same = P.checkEvalsTo;
    });
    describe("constants", function() {
      xit("should parse bignums correctly", function() {
        same("328746592836598324659284365928436522398429", rt.makeNumberFromString("328746592836598324659284365928436522398429"));
        P.wait(done);
      });

      it("should be equal to their value", function(done) {
        same("5", rt.makeNumber(5));
        same("5.5", rt.makeNumberFromString("11/2"));
        same("-1", rt.makeNumberFromString("-1"));
        same("-1.3", rt.makeNumberFromString("-13/10"));

        same("true", rt.makeBoolean(true));
        same("false", rt.makeBoolean(false));

        same("{x:5}", rt.makeObject({x: rt.makeNumber(5)}));
        same("{x:5, y:10}", rt.makeObject({y: rt.makeNumber(10), x: rt.makeNumber(5)}));

        same("{x:5, y:{z: {}}}", rt.makeObject({
            x: rt.makeNumber(5),
            y: rt.makeObject({
                z: rt.makeObject({}),
              })
          }));

        P.wait(done);
      });
    });
  }
  return { performTest: performTest };
});
