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
  var sampleErrbacks = {
    throwDomainError: function(x) { throw new Error('domainError ' + x); },
    throwLogNonPositive: function(x) { throw new Error('logNonPositive ' + x); },
    throwUndefinedValue: function(x) { throw new Error('undefinedValue ' + x); },
    throwGeneralError: function(x) { throw new Error('generalError ' + x); },
  };
  describe("check functions that don't allow testing via Pyret programs", function() {

    it("makeNumericBinop", function() {
      var bogusNumFun = JN._innards.makeNumericBinop(
        function(x, y, errbacks) {
          if (x <= 2 && y <= 2) return 1;
          if (x <= 2) return 2;
          if (y <= 2) return 3;
          errbacks.throwDomainError('makeNumericBinop first fail');
        },
        function(x, y, errbacks) {
          if (JN.lessThanOrEqual(x, 2, errbacks) && JN.lessThanOrEqual(y, 2, errbacks)) return 4;
          if (JN.lessThanOrEqual(x, 2, errbacks)) return 5;
          if (JN.lessThanOrEqual(y, 2, errbacks)) return 6;
          errbacks.throwDomainError('makeNumericBinop second fail');
        },
        {
          isXSpecialCase: function(x, errbacks) {
            return JN.lessThanOrEqual(x, 0, errbacks);
          },
          onXSpecialCase: function(x, y, errbacks) {
            return 7;
          },
          isYSpecialCase: function(y, errbacks) {
            return JN.lessThanOrEqual(y, 0, errbacks);
          },
          onYSpecialCase: function(x, y, errbacks) {
            return 8;
          }
        }
      );

      expect(bogusNumFun(1, 1, sampleErrbacks)).toEqual(1);
      expect(bogusNumFun(1, 3, sampleErrbacks)).toEqual(2);
      expect(bogusNumFun(3, 1, sampleErrbacks)).toEqual(3);
      expect(function() { bogusNumFun(3, 3, sampleErrbacks); }).toThrowError(/first fail/);

      expect(bogusNumFun(JN.makeRational(3, 2, sampleErrbacks), JN.makeRational(3, 2, sampleErrbacks), sampleErrbacks))
        .toEqual(4);
      expect(bogusNumFun(JN.makeRational(3, 2, sampleErrbacks), JN.makeRational(5, 2, sampleErrbacks), sampleErrbacks))
        .toEqual(5);
      expect(bogusNumFun(JN.makeRational(5, 2, sampleErrbacks), JN.makeRational(3, 2, sampleErrbacks), sampleErrbacks))
        .toEqual(6);
      expect(function() { bogusNumFun(JN.makeRational(5, 2, sampleErrbacks), JN.makeRational(5, 2, sampleErrbacks), sampleErrbacks); })
        .toThrowError(/second fail/);

      expect(bogusNumFun(-1, +1, sampleErrbacks)).toEqual(7);
      expect(bogusNumFun(+1, -1, sampleErrbacks)).toEqual(8);

    });

    it("fromString", function() {
      expect(JN.fromString("5", sampleErrbacks)).toEqual(5);

      var bigIntStr = "1" + new Array(309 + 1).join("0"); // 1 followed by 309 0s
      expect(JN.fromString(bigIntStr, sampleErrbacks)).toEqual(JN.makeBignum(bigIntStr));

      expect(JN.fromString("1e1", sampleErrbacks)).toBe(10);

      // for sci-not, fromString() and makeBignum() can give structurally
      // unequal but operationally equivalent results, so the following fails:
      // expect(JN.fromString("1e141", sampleErrbacks)).toEqual(JN.makeBignum("1e141"));
      // however you can refashion the test using JN.equals

      expect(JN.equals(JN.fromString("1e5", sampleErrbacks), JN.makeBignum("1e5"), sampleErrbacks))
        .toBe(true);
      expect(JN.equals(JN.fromString("1e30", sampleErrbacks), JN.makeBignum("1e30"), sampleErrbacks))
        .toBe(true);
      expect(JN.equals(JN.fromString("1e140", sampleErrbacks), JN.makeBignum("1e140"), sampleErrbacks))
        .toBe(true);
      expect(JN.equals(JN.fromString("1e141", sampleErrbacks), JN.makeBignum("1e141"), sampleErrbacks))
        .toBe(true);
      expect(JN.equals(JN.fromString("1e307", sampleErrbacks), JN.makeBignum("1e307"), sampleErrbacks))
        .toBe(true);
      expect(JN.equals(JN.fromString("1e309", sampleErrbacks), JN.makeBignum("1e309"), sampleErrbacks))
        .toBe(true);

      expect(JN.equals(JN.fromString("1e311", sampleErrbacks),
        JN.makeBignum("1e311"),
        sampleErrbacks))
        .toBe(true);
      expect(JN.equals(JN.fromString("1/2", sampleErrbacks),
        JN.makeRational(1, 2, sampleErrbacks),
        sampleErrbacks))
        .toBe(true);
      expect(JN.equals(JN.fromString("355/113", sampleErrbacks),
        JN.makeRational(355, 113, sampleErrbacks),
        sampleErrbacks))
        .toBe(true);
      expect(JN.equals(JN.fromString("1.5e3", sampleErrbacks), 1500)).toBe(true);
      expect(JN.roughlyEquals(JN.fromString("~2.71828", sampleErrbacks),
        JN.fromFixnum(2.71828, sampleErrbacks),
        0.001, sampleErrbacks))
        .toBe(true);
      expect(JN.fromString("not-a-string", sampleErrbacks)).toBe(false);

    });

    it("fromFixnum", function() {

      expect(JN.fromFixnum(5, sampleErrbacks)).toEqual(5);
      expect(JN.fromFixnum(1/2, sampleErrbacks)).toEqual(JN.makeRational(1, 2));
      expect(JN.fromFixnum(1.5e3, sampleErrbacks)).toEqual(1500);
      expect(JN.fromFixnum(1e311, sampleErrbacks)).toBe(false);

    });

    it("bnpExp", function() {
      // BigInteger.*.expt calls bnPow, wch calls bnpExp
      // shd raise exc for too-large
      expect(function() {
        JN.makeBignum(2).expt(JN.makeBignum(0xffffffff + 1), sampleErrbacks);
      }).toThrowError(/domainError/);

      // BigInteger.*.log
      // shd raise exc for arg <= 0
      expect(function() { JN.makeBignum(-1).log(sampleErrbacks); }).toThrowError(/logNonPositive/);
    });

    it("arithmetic", function() {

    });

    it("trig functions", function() {
      // BigInteger.*asin
      // shd raise exception for arg outside [-1, +1]
      // but this is not testable via Pyret, because args are always sane
      // by the time this method is called
      expect(function() { JN.makeBignum(-1.5).asin(sampleErrbacks); }).toThrowError(/domainError/);
      expect(function() { JN.makeBignum(+1.5).asin(sampleErrbacks); }).toThrowError(/domainError/);

      // BigInteger.*acos
      // shd raise exc for arg < -1 or > 1
      expect(function() { JN.makeBignum(-1.5).acos(sampleErrbacks); }).toThrowError(/domainError/);
      expect(function() { JN.makeBignum(+1.5).acos(sampleErrbacks); }).toThrowError(/domainError/);

      // BigInteger.*.atan
      // should work
      expect(JN.makeBignum(0).atan(sampleErrbacks)).toEqual(0);

      // atan2 (perhaps Pyret test is enough)
      expect(function () {
        JN.atan2(JN.makeBignum(0), JN.makeBignum(0), sampleErrbacks);
      }).toThrowError(/domainError/);

      // BigInteger.*.sin
      // should work
      expect(JN.makeBignum(0).sin(sampleErrbacks)).toEqual(0);

      // BigInteger.*.cos
      // should work
      expect(JN.makeBignum(0).cos(sampleErrbacks)).toEqual(1);

      // BigInteger.*.tan
      // should work
      expect(JN.makeBignum(0).tan(sampleErrbacks)).toEqual(0);

    });

    it("Rational methods", function() {
      expect(function () { JN.Rational.makeInstance(undefined, undefined, sampleErrbacks); })
        .toThrowError(/undefined/);
      expect(JN.equals(JN.Rational.makeInstance(1, undefined, sampleErrbacks), 1)).toBe(true);
      expect(JN.equals(JN.Rational.makeInstance(1, -1, sampleErrbacks), -1)).toBe(true);
      expect(JN.equals(JN.Rational.makeInstance(2, 1, sampleErrbacks), 2)).toBe(true);
      expect(JN.equals(JN.Rational.makeInstance(0, 1, sampleErrbacks), 0)).toBe(true);
      expect(JN.equals(JN.Rational.makeInstance(2, 3, sampleErrbacks), JN.fromString("2/3")))
        .toBe(true);

      expect(JN.Rational.makeInstance(1, 3, sampleErrbacks).equals(
        JN.Rational.makeInstance(1, 3, sampleErrbacks), sampleErrbacks))
        .toBe(true);

      expect(JN.Rational.makeInstance(2, 3, sampleErrbacks).toString()).toBe("2/3");
      expect(JN.Rational.makeInstance(2, 1, sampleErrbacks).toString()).toBe("2");

      expect(JN.equals(JN.Rational.makeInstance(1, 3, sampleErrbacks).add(
        JN.Rational.makeInstance(2, 3, sampleErrbacks), sampleErrbacks),
        1, sampleErrbacks))
        .toBe(true);

      expect(JN.equals(JN.Rational.makeInstance(4, 3, sampleErrbacks).subtract(
        JN.Rational.makeInstance(1, 3, sampleErrbacks), sampleErrbacks),
        1, sampleErrbacks))
        .toBe(true);

      expect(JN.equals(JN.Rational.makeInstance(-4, 3, sampleErrbacks).negate(sampleErrbacks),
        JN.fromString("4/3"), sampleErrbacks))
        .toBe(true);

      expect(JN.equals(JN.Rational.makeInstance(2, 3, sampleErrbacks).multiply(
        JN.Rational.makeInstance(3, 2, sampleErrbacks), sampleErrbacks),
        1, sampleErrbacks))
        .toBe(true);

      expect(JN.equals(JN.Rational.makeInstance(2, 3, sampleErrbacks).divide(
        JN.Rational.makeInstance(4, 6, sampleErrbacks), sampleErrbacks),
        1, sampleErrbacks))
        .toBe(true);

      // toRational?
      expect(JN.Rational.makeInstance(2, 3, sampleErrbacks).isRational()).toBe(true);

      expect(JN.Rational.makeInstance(2, 4, sampleErrbacks).toFixnum()).toEqual(0.5);

      expect(JN.Rational.makeInstance(4, 6, sampleErrbacks).numerator()).toEqual(2);
      expect(JN.Rational.makeInstance(4, 6, sampleErrbacks).denominator()).toEqual(3);

      expect(JN.Rational.makeInstance(2, 3, sampleErrbacks).greaterThan(
        JN.Rational.makeInstance(1, 3, sampleErrbacks), sampleErrbacks))
        .toBe(true);

      expect(JN.Rational.makeInstance(1, 3, sampleErrbacks).greaterThanOrEqual(
        JN.Rational.makeInstance(1, 3, sampleErrbacks), sampleErrbacks))
        .toBe(true);

      expect(JN.Rational.makeInstance(1, 3, sampleErrbacks).lessThan(
        JN.Rational.makeInstance(2, 3, sampleErrbacks), sampleErrbacks))
        .toBe(true);

      expect(JN.Rational.makeInstance(1, 3, sampleErrbacks).lessThanOrEqual(
        JN.Rational.makeInstance(1, 3, sampleErrbacks), sampleErrbacks))
        .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(101, 4, sampleErrbacks).integerSqrt(sampleErrbacks),
        5, sampleErrbacks))
        .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(100, 9, sampleErrbacks).sqrt(sampleErrbacks),
        JN.Rational.makeInstance(10, 3, sampleErrbacks),
        sampleErrbacks))
        .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(-4, 3, sampleErrbacks).abs(sampleErrbacks),
        JN.Rational.makeInstance(4, 3, sampleErrbacks),
        sampleErrbacks))
        .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(4, 3, sampleErrbacks).floor(sampleErrbacks),
        1,
        sampleErrbacks))
      .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(4, 3, sampleErrbacks).ceiling(sampleErrbacks),
        2,
        sampleErrbacks))
      .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(4, 3, sampleErrbacks).round(sampleErrbacks),
        1,
        sampleErrbacks))
      .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(7, 2, sampleErrbacks).roundEven(sampleErrbacks),
        4,
        sampleErrbacks))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(5, 2, sampleErrbacks).log(sampleErrbacks),
        JN.fromFixnum(0.91629),
        0.001, sampleErrbacks))
      .toBe(true);

      // tan(pi/4) == 1
      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(355, 4 * 113, sampleErrbacks).tan(sampleErrbacks),
        1, 0.001, sampleErrbacks))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(1000, 1732, sampleErrbacks).atan(sampleErrbacks),
        JN.makeRoughnum(355 / (6 * 113)),
        0.001, sampleErrbacks))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(1732, 1000, sampleErrbacks).atan(sampleErrbacks),
        JN.fromFixnum(355 / (3 * 113)),
        0.001, sampleErrbacks))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(355, 2 * 113, sampleErrbacks).cos(sampleErrbacks),
        0, 0.001, sampleErrbacks))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(355, 2 * 113, sampleErrbacks).sin(sampleErrbacks),
        1, 0.001, sampleErrbacks))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(9, 4, sampleErrbacks).expt(
          JN.Rational.makeInstance(3, 2, sampleErrbacks), sampleErrbacks),
        27 / 8, 0.001, sampleErrbacks))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(3, 2, sampleErrbacks).exp(sampleErrbacks),
        JN.fromFixnum(Math.exp(1.5)), 0.001, sampleErrbacks))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(1, 2, sampleErrbacks).acos(sampleErrbacks),
        JN.Rational.makeInstance(355, 3 * 113),
        0.001, sampleErrbacks))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(1, 2, sampleErrbacks).asin(sampleErrbacks),
        JN.Rational.makeInstance(355, 6 * 113),
        0.001, sampleErrbacks))
      .toBe(true);

    });

    it("Roughnum methods", function() {

      expect(function () { JN.Roughnum.makeInstance(undefined, sampleErrbacks); })
        .toThrowError(/unsuitable/);

      expect(JN.equals(JN.Roughnum.makeInstance(3.14, sampleErrbacks).toFixnum(), 3.14)).toBe(true);

      expect(JN.roughlyEquals(JN.Roughnum.makeInstance(3.14, sampleErrbacks), 3.14,
        0.0001, sampleErrbacks))
        .toBe(true);

      expect(JN.Roughnum.makeInstance(3.14, sampleErrbacks).isRoughnum()).toBe(true);

      expect(JN.equals(JN.Roughnum.makeInstance(3.14, sampleErrbacks).toFixnum(), 3.14)).toBe(true);

      // shouldn't roughnum's numerator method take errbacks?

      expect(JN.Roughnum.makeInstance(3.14, sampleErrbacks).numerator().toFixnum()).toEqual(157);
      expect(JN.Roughnum.makeInstance(3.14, sampleErrbacks).denominator().toFixnum()).toEqual(50);

      expect(JN.Roughnum.makeInstance(2.3, sampleErrbacks).greaterThan(
        JN.Roughnum.makeInstance(1.3, sampleErrbacks), sampleErrbacks))
        .toBe(true);

      expect(JN.Roughnum.makeInstance(1.3, sampleErrbacks).greaterThanOrEqual(
        JN.Roughnum.makeInstance(1.3, sampleErrbacks), sampleErrbacks))
        .toBe(true);

      expect(JN.Roughnum.makeInstance(1.3, sampleErrbacks).lessThan(
        JN.Roughnum.makeInstance(2.3, sampleErrbacks), sampleErrbacks))
        .toBe(true);

      expect(JN.Roughnum.makeInstance(1.3, sampleErrbacks).lessThanOrEqual(
        JN.Roughnum.makeInstance(1.3, sampleErrbacks), sampleErrbacks))
        .toBe(true);

      // why is roughnum integersqrt so different

      expect(function() {
        JN.Roughnum.makeInstance(101, sampleErrbacks).integerSqrt(sampleErrbacks);
      }).toThrowError(/can only be applied to an integer/);

      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(100, sampleErrbacks).sqrt(sampleErrbacks),
        JN.Roughnum.makeInstance(10, sampleErrbacks),
        0.0001, sampleErrbacks))
        .toBe(true);

      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(-3.14, sampleErrbacks).abs(sampleErrbacks),
        JN.Roughnum.makeInstance(3.14, sampleErrbacks),
        0.0001, sampleErrbacks))
        .toBe(true);

      expect(JN.equals(
        JN.Roughnum.makeInstance(3.14, sampleErrbacks).floor(sampleErrbacks),
        3, sampleErrbacks))
      .toBe(true);

      expect(JN.equals(
        JN.Roughnum.makeInstance(3.14, sampleErrbacks).ceiling(sampleErrbacks),
        4, sampleErrbacks))
      .toBe(true);

      expect(JN.equals(
        JN.Roughnum.makeInstance(3.14, sampleErrbacks).round(sampleErrbacks),
        3, sampleErrbacks))
      .toBe(true);

      expect(JN.equals(
        JN.Roughnum.makeInstance(3.5, sampleErrbacks).roundEven(sampleErrbacks),
        4, sampleErrbacks))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(2.5, sampleErrbacks).log(sampleErrbacks),
        JN.fromFixnum(0.91629),
        0.001, sampleErrbacks))
      .toBe(true);

      // tan(pi/4) == 1
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance((355 / (4 * 113)), sampleErrbacks).tan(sampleErrbacks),
        1, 0.001, sampleErrbacks))
      .toBe(true);

      // tan(pi/6) = 1/sqrt(3)
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(1/1.732, sampleErrbacks).atan(sampleErrbacks),
        JN.fromFixnum(355 / (6 * 113), sampleErrbacks),
        0.001, sampleErrbacks))
      .toBe(true);

      // tan(pi/3) = sqrt(3)
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(1.732, sampleErrbacks).atan(sampleErrbacks),
        JN.fromFixnum(355 / (3 * 113), sampleErrbacks),
        0.001, sampleErrbacks))
      .toBe(true);

      // cos(pi/2) = 0
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(355 / (2 * 113), sampleErrbacks).cos(sampleErrbacks),
        0, 0.001, sampleErrbacks))
      .toBe(true);

      // sin(pi/2) = 1
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(355 / (2 * 113), sampleErrbacks).sin(sampleErrbacks),
        1, 0.001, sampleErrbacks))
      .toBe(true);

      // (9/4)^(3/2) = 27/8
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(9/4, sampleErrbacks).expt(
          JN.Roughnum.makeInstance(3/2, sampleErrbacks), sampleErrbacks),
        27 / 8, 0.001, sampleErrbacks))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(3/2, sampleErrbacks).exp(sampleErrbacks),
        JN.fromFixnum(Math.exp(1.5)), 0.001, sampleErrbacks))
      .toBe(true);

      // cos(pi/3) = 1/2
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(1/2, sampleErrbacks).acos(sampleErrbacks),
        JN.Roughnum.makeInstance(355/(3 * 113), sampleErrbacks),
        0.001, sampleErrbacks))
      .toBe(true);

      // sin(pi/6) = 1/2
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(1/2, sampleErrbacks).asin(sampleErrbacks),
        JN.Roughnum.makeInstance(355/(6 * 113), sampleErrbacks),
        0.001, sampleErrbacks))
      .toBe(true);

    })


  });

  jazz.execute();

});
