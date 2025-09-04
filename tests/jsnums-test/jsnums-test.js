// To test: Build Pyret, then cd to this directory, and type
// node jsnums-test.js

var Jasmine = require('jasmine');
var jazz = new Jasmine();
const R = require("requirejs");
var build = process.env["PHASE"] || "build/phaseA";
R.config({
  waitSeconds: 15000,
  paths: {
    "pyret-base": "../../" + build
  }
});
R(["pyret-base/js/js-numbers"], function(JN) {
  var sampleErrorBacks = {
    throwDomainError: function() { throw 'domainError'; },
    throwLogNonPositive: function() { throw 'logNonPositive'; },
  };
  describe("check exceptions in js-numbers methods that can't be tested in Pyret", function() {
    it("bnpExp", function() {
      // BigInteger.*.expt calls bnPow, which calls bnpExp
      // shd raise exc for too-large
      expect(function() { JN.makeBignum(2).expt(JN.makeBignum(0xffffffff + 1), sampleErrorBacks); }).toThrow('domainError');

      // BigInteger.*.log
      // should raise exception for arg <= 0
      expect(function() { JN.makeBignum(-1).log(sampleErrorBacks); }).toThrow('logNonPositive');

      // BigInteger.*asin
      // should raise exception for arg ∉ [-1, +1]
      expect(function() { JN.makeBignum(-1.5).asin(sampleErrorBacks); }).toThrow('domainError');
      expect(function() { JN.makeBignum(+1.5).asin(sampleErrorBacks); }).toThrow('domainError');

      // BigInteger.*acos
      // should raise exception for arg ∉ [-1, +1]
      expect(function() { JN.makeBignum(-1.5).acos(sampleErrorBacks); }).toThrow('domainError');
      expect(function() { JN.makeBignum(+1.5).acos(sampleErrorBacks); }).toThrow('domainError');

    });
  });

  jazz.execute();

});
