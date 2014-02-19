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
      pred = P.checkEvalPred;
    });
    describe("Data", function() {
      it("should define singletons", function(done) {
        var singleton = "data Foo:\n" +
                        "  | singleton\n" +
                        "end\n";
        same(
            singleton + "is-singleton(singleton) and Foo(singleton)",
            rt.pyretTrue
          );
        
        pred(singleton + "singleton", function(val) {
            var fields = rt.getFields(val).sort();
            expect(fields[0]).toEqual("_match");
            expect(fields[1]).toEqual("_torepr");
            expect(fields.length).toEqual(2);
            return true;
          });

        var singletonWithMeth = "data MyList:\n" +
                               "  | mt with:" +
                               "    length(self): 0 end\n" +
                               " end\n";
        same(singletonWithMeth + "mt.length()", rt.makeNumber(0));
        pred(singletonWithMeth + "mt", function(val) {
            var fields = rt.getFields(val).sort();
            expect(fields[0]).toEqual("_match");
            expect(fields[1]).toEqual("_torepr");
            expect(fields[2]).toEqual("length");
            expect(fields.length).toEqual(3);
            return true;
          });

        var singletonWithSharing = "data MyList:\n" +
                               "  | mt\n" +
                               " sharing:\n" +
                               "   length(self): 0 end\n" +
                               " end\n";
        same(singletonWithSharing + "mt.length()", rt.makeNumber(0));
        pred(singletonWithSharing + "mt", function(val) {
            var fields = rt.getFields(val).sort();
            expect(fields[0]).toEqual("_match");
            expect(fields[1]).toEqual("_torepr");
            expect(fields[2]).toEqual("length");
            expect(fields.length).toEqual(3);
            return true;
          });


        P.wait(done);
      });

      it("should handle examples", function(done) {
        same(
"data Color:\n" +
"  | red with: asRGB(_): { r: 256, g: 0, b: 0 } end\n" +
"  | green with: asRGB(_): { r: 0, g: 256, b: 0 } end\n" +
"  | blue with: asRGB(_): { r: 0, g: 0, b: 256 } end\n" +
"  | rgb(r :: Number, g :: Number, b :: Number) with:\n" +
"     asRGB(self): { r: self.r, g: self.g, b: self.b } end\n" +
" end\n" +
" fun asRGB(obj): obj.asRGB() end\n" +
" fun getR(obj): obj.r end\n" +
" [rgb(100,100,100), red, green].map(asRGB).map(getR)._equals([100, 256, 0])\n",
          rt.pyretTrue);

        P.wait(done);
      });
    });

  }
  return {performTest: performTest};
});
