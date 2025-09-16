// To test: Build Pyret, then cd to this directory, and type
// node jsnums-test.js

var Jasmine = require('jasmine');
var jazz = new Jasmine();
const R = require("requirejs");
var build = process.env["PHASE"] || "build/phaseA";
R.config({
  waitSeconds: 15000,
  paths: {
    "trove": "../../" + build + "/trove",
    "js": "../../" + build + "/js",
    "compiler": "../../" + build + "/arr/compiler",
    "jglr": "../../lib/jglr",
    "pyret-base": "../../" + build
  }
});
R(["pyret-base/js/js-numbers"], function(JN) {
  var sampleErrorBacks = {
    throwDomainError: function() { throw 'domainError'; },
    throwLogNonPositive: function() { throw 'logNonPositive'; },
  };
  function test(actual, expected, testname, toks) {
    if (actual === expected) {
      return true;
    } else {
      var allToks = "Str was " + JSON.stringify(testname) + "\n";
      for (var t = 0; t < toks.length; t++) {
        if (t > 0) allToks += "\n";
        allToks += "Tok[" + t + "] = " + toks[t].toString(true)
          + " at pos " + toks[t].pos.toString(true);
      }
      allToks += "Expected " + JSON.stringify(expected) + ", but got " + JSON.stringify(actual)
        + " in " + JSON.stringify(testname);
      expect(allToks).toBe("");
      return false;
    }
  }
  function testPos(tok, expected, str, toks) {
    if (tok.pos.endChar - tok.pos.startChar == expected.length) {
      return true;
    } else {
      test(str.slice(tok.pos.startChar, tok.pos.endChar), expected, str, toks);
      return false;
    }
  }
  describe("check functions that don't allow testing via Pyret programs", function() {

    it("fromString", function() {
      expect(JN.fromString("5", sampleErrorBacks)).toEqual(5);

      var bigIntStr = "1" + new Array(309 + 1).join("0"); // 1 followed by 309 0s
      expect(JN.fromString(bigIntStr, sampleErrorBacks)).toEqual(JN.makeBignum(bigIntStr));

      // console.log(JN.fromString("1e1", sampleErrorBacks));
      // console.log(JN.fromString("10", sampleErrorBacks));
      // console.log(JN.makeBignum("1e1", sampleErrorBacks));
      // console.log(JN.makeBignum("10", sampleErrorBacks).toFixnum());

      expect(JN.fromString("1e1", sampleErrorBacks)).toBe(10);
      expect(JN.fromString("1e30", sampleErrorBacks)).toEqual(JN.makeBignum("1e30"));
      expect(JN.fromString("1e140", sampleErrorBacks)).toEqual(JN.makeBignum("1e140"));

      // for large bignums (> 1e140 ?), fromString() and makeBignum() can give structurally
      // unequal results. so the following fail:
      // expect(JN.fromString("1e141", sampleErrorBacks)).toEqual(JN.makeBignum("1e141"));
      // expect(JN.fromString("1e307", sampleErrorBacks)).toEqual(JN.makeBignum("1e307"));
      // expect(JN.fromString("1e309", sampleErrorBacks)).toEqual(JN.makeBignum("1e309"));

      // but they're operationally equivalent!
      expect(JN.equals(JN.fromString("1e141", sampleErrorBacks),
        JN.makeBignum("1e141"),
        sampleErrorBacks)).toBe(true);

      // fromString() and makeBignum() give different but operationally same bignums
      // console.log('*************************');
      // console.log(JN.fromString("1e307", sampleErrorBacks));
      // console.log('-------------------------');
      // console.log(JN.makeBignum("1e307"));
      // console.log('-------------------------');
      // console.log(JN.multiply(1, JN.makeBignum("1e307"), sampleErrorBacks)['40']);
      // console.log('*************************');

      expect(JN.fromString("1e311", sampleErrorBacks)).toEqual(JN.makeBignum("1e311"));
      expect(JN.fromString("1/2", sampleErrorBacks)).toEqual(JN.makeRational(1, 2));
      expect(JN.fromString("355/113", sampleErrorBacks)).toEqual(JN.makeRational(355, 113));
      expect(JN.fromString("1.5e3", sampleErrorBacks)).toEqual(1500);
      expect(JN.fromString("~2.718281828", sampleErrorBacks)).toEqual(JN.makeRoughnum(2.718281828));
      expect(JN.fromString("not-a-string", sampleErrorBacks)).toBe(false);

    });

    it("fromFixnum", function() {

      expect(JN.fromFixnum(5, sampleErrorBacks)).toEqual(5);
      expect(JN.fromFixnum(1/2, sampleErrorBacks)).toEqual(JN.makeRational(1, 2));
      expect(JN.fromFixnum(1.5e3, sampleErrorBacks)).toEqual(1500);
      expect(JN.fromFixnum(1e311, sampleErrorBacks)).toBe(false);

    });

    it("bnpExp", function() {
      // BigInteger.*.expt calls bnPow, wch calls bnpExp
      // shd raise exc for too-large
      expect(function() { JN.makeBignum(2).expt(JN.makeBignum(0xffffffff + 1), sampleErrorBacks); }).toThrow('domainError');

      // BigInteger.*.log
      // shd raise exc for arg <= 0
      expect(function() { JN.makeBignum(-1).log(sampleErrorBacks); }).toThrow('logNonPositive');
    });

    it("arithmetic", function() {

    });

    it("trig functions", function() {
      // BigInteger.*asin
      // shd raise exception for arg outside [-1, +1]
      // but this is not testable via Pyret, because args are always sane
      // by the time this method is called
      expect(function() { JN.makeBignum(-1.5).asin(sampleErrorBacks); }).toThrow('domainError');
      expect(function() { JN.makeBignum(+1.5).asin(sampleErrorBacks); }).toThrow('domainError');

      // BigInteger.*acos
      // shd raise exc for arg < -1 or > 1
      expect(function() { JN.makeBignum(-1.5).acos(sampleErrorBacks); }).toThrow('domainError');
      expect(function() { JN.makeBignum(+1.5).acos(sampleErrorBacks); }).toThrow('domainError');

      // BigInteger.*.atan
      // should work
      expect(JN.makeBignum(0).atan(sampleErrorBacks)).toEqual(0);

      // atan2 (perhaps Pyret test is enough)
      expect(function () {
        JN.atan2(JN.makeBignum(0), JN.makeBignum(0), sampleErrorBacks);
      }).toThrow('domainError');

      // BigInteger.*.sin
      // should work
      expect(JN.makeBignum(0).sin(sampleErrorBacks)).toEqual(0);

      // BigInteger.*.cos
      // should work
      expect(JN.makeBignum(0).cos(sampleErrorBacks)).toEqual(1);

      // BigInteger.*.tan
      // should work
      expect(JN.makeBignum(0).tan(sampleErrorBacks)).toEqual(0);


    });
  });

  jazz.execute();

});
