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
  describe("check exceptions in js-numbers methods that can't be tested in Pyret", function() {
    it("bnpExp", function() {
      // BigInteger.*.expt calls bnPow, wch calls bnpExp
      // shd raise exc for too-large 
      expect(function() { JN.makeBignum(2).expt(JN.makeBignum(0xffffffff + 1), sampleErrorBacks); }).toThrow('domainError');

      // BigInteger.*.log 
      // shd raise exc for arg <= 0
      expect(function() { JN.makeBignum(-1).log(sampleErrorBacks); }).toThrow('logNonPositive');

      // BigInteger.*asin 
      // shd raise exc for arg < -1 or > 1
      expect(function() { JN.makeBignum(-1.5).asin(sampleErrorBacks); }).toThrow('domainError');
      expect(function() { JN.makeBignum(+1.5).asin(sampleErrorBacks); }).toThrow('domainError');

      // BigInteger.*acos 
      // shd raise exc for arg < -1 or > 1
      expect(function() { JN.makeBignum(-1.5).acos(sampleErrorBacks); }).toThrow('domainError');
      expect(function() { JN.makeBignum(+1.5).acos(sampleErrorBacks); }).toThrow('domainError');

      // BigInteger.*.atan
      // should work
      expect(JN.makeBignum(0).atan(sampleErrorBacks)).toEqual(0);

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
