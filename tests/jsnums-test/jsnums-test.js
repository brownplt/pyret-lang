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
R(["pyret-base/js/js-numbers"], function(JNlib) {
  var sampleErrbacks = {
    throwDomainError: function() { throw 'domainError'; },
    throwLogNonPositive: function() { throw 'logNonPositive'; },
  };
  // Build a library instance whose prototype methods close over the
  // throwing errbacks, so that expt/log/asin/acos report via this test's
  // sentinel strings rather than the module's default INITIAL_ERRBACKS.
  var JN = JNlib.MakeNumberLibrary(sampleErrbacks);
  describe("check exceptions in js-numbers methods that can't be tested in Pyret", function() {
    it("bnpExp", function() {
      // BigInteger.*.expt calls bnPow, which calls bnpExp
      // shd raise exc for too-large
      expect(function() { JN.makeBignum(2).expt(JN.makeBignum(0xffffffff + 1)); }).toThrow('domainError');

      // BigInteger.*.log
      // should raise exception for arg <= 0
      expect(function() { JN.makeBignum(-1).log(); }).toThrow('logNonPositive');

      // BigInteger.*asin
      // should raise exception for arg ∉ [-1, +1]
      expect(function() { JN.makeBignum(-1.5).asin(); }).toThrow('domainError');
      expect(function() { JN.makeBignum(+1.5).asin(); }).toThrow('domainError');

      // BigInteger.*acos
      // should raise exception for arg ∉ [-1, +1]
      expect(function() { JN.makeBignum(-1.5).acos(); }).toThrow('domainError');
      expect(function() { JN.makeBignum(+1.5).acos(); }).toThrow('domainError');

    });

    it("atan2 four quadrants and boundaries", function() {
      // atan2(y, x) takes y first, x second (math convention).
      // The x<0 and (x>0, y<0) branches historically leaked bare
      // Math.PI / 2*Math.PI through `add`, because JS-number+JS-number
      // short-circuits and returns its raw sum.

      // x > 0, y = 0: must be exact integer 0, NOT Roughnum 0.0.
      // Multiples of π are intrinsically irrational, but this case is
      // representable exactly and "atan2(0, 5) == 0" is the kind of
      // thing math teachers expect to be unambiguous. Implementation
      // routes through atan(divide(0, x)) -> atan(0) -> integer 0.
      expect(JN.isInteger(JN.atan2(0, 1))).toBe(true);
      expect(JN.equals(JN.atan2(0, 1), 0)).toBe(true);
      expect(JN.isInteger(JN.atan2(0, 5))).toBe(true);
      expect(JN.equals(JN.atan2(0, 5), 0)).toBe(true);
      expect(JN.isInteger(JN.atan2(0, JN.makeBignum(5)))).toBe(true);
      expect(JN.equals(JN.atan2(0, JN.makeBignum(5)), 0)).toBe(true);

      // x > 0, y > 0  (1st quadrant interior): atan(y/x), irrational.
      expect(JN.isRoughnum(JN.atan2(1, 1))).toBe(true);

      // x = 0, y > 0: π/2 (Roughnum)
      expect(JN.isRoughnum(JN.atan2(1, 0))).toBe(true);
      expect(JN.toFixnum(JN.atan2(1, 0))).toEqual(Math.PI / 2);

      // x < 0, y >= 0  (2nd quadrant): atan(y/x) + π
      expect(JN.isRoughnum(JN.atan2(1, -1))).toBe(true);
      expect(JN.isRoughnum(JN.atan2(0, -1))).toBe(true);
      // atan2(0, -1) = π exactly. This case used to leak Math.PI as a raw JS double.
      expect(JN.toFixnum(JN.atan2(0, -1))).toEqual(Math.PI);

      // x < 0, y < 0  (3rd quadrant): atan(y/x) + π
      expect(JN.isRoughnum(JN.atan2(-1, -1))).toBe(true);

      // x = 0, y < 0: 3π/2. Note: jsnums returns angles in [0, 2π) here,
      // unlike JS Math.atan2 which would give -π/2 for this case.
      expect(JN.isRoughnum(JN.atan2(-1, 0))).toBe(true);
      expect(JN.toFixnum(JN.atan2(-1, 0))).toEqual(3 * Math.PI / 2);

      // x > 0, y < 0  (4th quadrant): atan(y/x) + 2π. Used to leak 2*Math.PI.
      expect(JN.isRoughnum(JN.atan2(-1, 1))).toBe(true);

      // (0, 0) is out of domain.
      expect(function() { JN.atan2(0, 0); }).toThrow('domainError');
    });
  });

  jazz.execute();

});
