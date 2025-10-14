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

    it("make*opFun", function() {

     var bogusIntegerUnOpFun = JN._innards.makeIntegerUnOp(
        function(x, errbacks) {
          if (x <= 2) return 1;
          errbacks.throwDomainError('makeIntegerUnOp first fail');
        },
        function(x, errbacks) {
          if (JN.lessThanOrEqual(x, 2, errbacks)) return 4;
          errbacks.throwDomainError('makeIntegerUnOp second fail');
        },
        {
          ignoreOverflow: true,
        }
      );

      expect(bogusIntegerUnOpFun(1, sampleErrbacks)).toEqual(1);
      expect(function() { bogusIntegerUnOpFun(3, sampleErrbacks); })
        .toThrowError(/first fail/);
      expect(bogusIntegerUnOpFun(JN.makeBignum(1), sampleErrbacks)).toEqual(4);

      var bogusIntegerBinopFun = JN._innards.makeIntegerBinop(
        function(x, y, errbacks) {
          if (x <= 2 && y <= 2) return 1;
          if (x <= 2) return 2;
          if (y <= 2) return 3;
          errbacks.throwDomainError('makeIntegerBinop first fail');
        },
        function(x, y, errbacks) {
          if (JN.lessThanOrEqual(x, 2, errbacks) && JN.lessThanOrEqual(y, 2, errbacks)) return 4;
          if (JN.lessThanOrEqual(x, 2, errbacks)) return 5;
          if (JN.lessThanOrEqual(y, 2, errbacks)) return 6;
          errbacks.throwDomainError('makeIntegerBinop second fail');
        },
        {
          isOverflow: true,
        }
      );

      expect(bogusIntegerBinopFun(1, 1, sampleErrbacks)).toEqual(1);
      expect(bogusIntegerBinopFun(1, 3, sampleErrbacks)).toEqual(2);
      expect(bogusIntegerBinopFun(3, 1, sampleErrbacks)).toEqual(3);
      expect(function() { bogusIntegerBinopFun(3, 3, sampleErrbacks); }).toThrowError(/first fail/);

      expect(bogusIntegerBinopFun(JN.makeBignum(1), JN.makeBignum(1), sampleErrbacks)).
        toEqual(4);
      expect(bogusIntegerBinopFun(JN.makeBignum(1), JN.makeBignum(3), sampleErrbacks)).
        toEqual(5);
      expect(bogusIntegerBinopFun(JN.makeBignum(3), JN.makeBignum(1), sampleErrbacks)).
        toEqual(6);
      expect(function() {
        bogusIntegerBinopFun(JN.makeBignum(3), JN.makeBignum(3), sampleErrbacks);
      }).toThrowError(/second fail/);

      var bogusNumericBinopFun = JN._innards.makeNumericBinop(
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

      expect(bogusNumericBinopFun(1, 1, sampleErrbacks)).toEqual(1);
      expect(bogusNumericBinopFun(1, 3, sampleErrbacks)).toEqual(2);
      expect(bogusNumericBinopFun(3, 1, sampleErrbacks)).toEqual(3);
      expect(function() { bogusNumericBinopFun(3, 3, sampleErrbacks); }).toThrowError(/first fail/);

      expect(bogusNumericBinopFun(JN.makeRational(3, 2, sampleErrbacks), JN.makeRational(3, 2, sampleErrbacks), sampleErrbacks))
        .toEqual(4);
      expect(bogusNumericBinopFun(JN.makeRational(3, 2, sampleErrbacks), JN.makeRational(5, 2, sampleErrbacks), sampleErrbacks))
        .toEqual(5);
      expect(bogusNumericBinopFun(JN.makeRational(5, 2, sampleErrbacks), JN.makeRational(3, 2, sampleErrbacks), sampleErrbacks))
        .toEqual(6);
      expect(function() { bogusNumericBinopFun(JN.makeRational(5, 2, sampleErrbacks), JN.makeRational(5, 2, sampleErrbacks), sampleErrbacks); })
        .toThrowError(/second fail/);

      expect(bogusNumericBinopFun(-1, +1, sampleErrbacks)).toEqual(7);
      expect(bogusNumericBinopFun(+1, -1, sampleErrbacks)).toEqual(8);

    });

    it("fromString", function() {
      expect(JN.fromString("5", sampleErrbacks)).toEqual(5);

      var bigIntStr = "1" + new Array(309 + 1).join("0"); // 1 followed by 309 0s
      expect(JN.fromString(bigIntStr, sampleErrbacks)).toEqual(JN.makeBignum(bigIntStr));

      expect(JN.equals(JN.fromString("1e1", sampleErrbacks), 10)).toBe(true);
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
      expect(JN.equals(JN.fromString("1e311", sampleErrbacks), JN.makeBignum("1e311"), sampleErrbacks))
        .toBe(true);

      // toEqual works too, because bigints have a unique respresentation, thanks to bnpClamp()
      expect(JN.fromString("1e1", sampleErrbacks))
        .toEqual(JN.makeBignum('10'));
      expect(JN.fromString("1e5", sampleErrbacks))
        .toEqual(JN.makeBignum('1e5'));
      expect(JN.fromString("1e30", sampleErrbacks))
        .toEqual(JN.makeBignum('1e30'));
      expect(JN.fromString("1e140", sampleErrbacks))
        .toEqual(JN.makeBignum('1e140'));
      expect(JN.fromString("1e141", sampleErrbacks))
        .toEqual(JN.makeBignum('1e141'));
      expect(JN.fromString("1e307", sampleErrbacks))
        .toEqual(JN.makeBignum('1e307'));
      expect(JN.fromString("1e309", sampleErrbacks))
        .toEqual(JN.makeBignum('1e309'));
      expect(JN.fromString("1e311", sampleErrbacks))
        .toEqual(JN.makeBignum('1e311'));

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

    it('number predicates', function() {
      // isPyretNumber
      expect(JN.isPyretNumber(5)).toBe(true);
      expect(JN.isPyretNumber(-5)).toBe(true);
      expect(JN.isPyretNumber(0)).toBe(true);
      expect(JN.isPyretNumber(3.14)).toBe(true);
      expect(JN.isPyretNumber(JN.Rational.makeInstance(355,113,sampleErrbacks))).toBe(true);
      expect(JN.isPyretNumber(JN.expt(10, 400))).toBe(true);
      expect(JN.isPyretNumber(JN.Roughnum.makeInstance(2.718,sampleErrbacks))).toBe(true);

      // isRational
      expect(JN.isRational(5)).toBe(true);
      expect(JN.isRational(-5)).toBe(true);
      expect(JN.isRational(0)).toBe(true);
      expect(JN.isRational(3.14)).toBe(true);
      expect(JN.isRational(JN.Rational.makeInstance(355,113,sampleErrbacks))).toBe(true);
      expect(JN.isRational(JN.expt(10, 400))).toBe(true);
      expect(JN.isRational(JN.Roughnum.makeInstance(2.718,sampleErrbacks))).toBe(false);

      // isExact
      expect(JN.isExact(5)).toBe(true);
      expect(JN.isExact(-5)).toBe(true);
      expect(JN.isExact(0)).toBe(true);
      expect(JN.isExact(3.14)).toBe(true);
      expect(JN.isExact(JN.Rational.makeInstance(355,113,sampleErrbacks))).toBe(true);
      expect(JN.isExact(JN.expt(10, 400))).toBe(true);
      expect(JN.isExact(JN.Roughnum.makeInstance(2.718,sampleErrbacks))).toBe(false);

      // isReal
      expect(JN.isReal(5)).toBe(true);
      expect(JN.isReal(-5)).toBe(true);
      expect(JN.isReal(0)).toBe(true);
      expect(JN.isReal(3.14)).toBe(true);
      expect(JN.isReal(JN.Rational.makeInstance(355,113,sampleErrbacks))).toBe(true);
      expect(JN.isReal(JN.expt(10, 400))).toBe(true);
      expect(JN.isReal(JN.Roughnum.makeInstance(2.718,sampleErrbacks))).toBe(true);

      // isInteger
      expect(JN.isInteger(5)).toBe(true);
      expect(JN.isInteger(-5)).toBe(true);
      expect(JN.isInteger(0)).toBe(true);
      expect(JN.isInteger(3.14)).toBe(false);
      expect(JN.isInteger(JN.Rational.makeInstance(355,113,sampleErrbacks))).toBe(false);
      expect(JN.isInteger(JN.expt(10, 400))).toBe(true);
      expect(JN.isInteger(JN.Roughnum.makeInstance(2.718,sampleErrbacks)))
         .toBe(false);

      // isRoughnum
      expect(JN.isRoughnum(5)).toBe(false);
      expect(JN.isRoughnum(-5)).toBe(false);
      expect(JN.isRoughnum(0)).toBe(false);
      expect(JN.isRoughnum(3.14)).toBe(false);
      expect(JN.isRoughnum(JN.Rational.makeInstance(355,113,sampleErrbacks))).toBe(false);
      expect(JN.isRoughnum(JN.expt(10, 400))).toBe(false);
      expect(JN.isRoughnum(JN.Roughnum.makeInstance(2.718,sampleErrbacks))).toBe(true);

      // isPositive
      expect(JN.isPositive(5)).toBe(true);
      expect(JN.isPositive(-5)).toBe(false);
      expect(JN.isPositive(0)).toBe(false);
      expect(JN.isPositive(3.14)).toBe(true);
      expect(JN.isPositive(JN.Rational.makeInstance(355,113,sampleErrbacks))).toBe(true);
      expect(JN.isPositive(JN.expt(10, 400))).toBe(true);
      expect(JN.isPositive(JN.Roughnum.makeInstance(2.718,sampleErrbacks))).toBe(true);

      // isNonPositive
      expect(JN.isNonPositive(5)).toBe(false);
      expect(JN.isNonPositive(-5)).toBe(true);
      expect(JN.isNonPositive(0)).toBe(true);
      expect(JN.isNonPositive(3.14)).toBe(false);
      expect(JN.isNonPositive(JN.Rational.makeInstance(355,113,sampleErrbacks))).toBe(false);
      expect(JN.isNonPositive(JN.expt(10, 400))).toBe(false);
      expect(JN.isNonPositive(JN.Roughnum.makeInstance(2.718,sampleErrbacks))).toBe(false);

      // isNegative
      expect(JN.isNegative(5)).toBe(false);
      expect(JN.isNegative(-5)).toBe(true);
      expect(JN.isNegative(0)).toBe(false);
      expect(JN.isNegative(3.14)).toBe(false);
      expect(JN.isNegative(JN.Rational.makeInstance(355,113,sampleErrbacks))).toBe(false);
      expect(JN.isNegative(JN.expt(10, 400))).toBe(false);
      expect(JN.isNegative(JN.Roughnum.makeInstance(2.718,sampleErrbacks))).toBe(false);

      // isNonNegative
      expect(JN.isNonNegative(5)).toBe(true);
      expect(JN.isNonNegative(-5)).toBe(false);
      expect(JN.isNonNegative(0)).toBe(true);
      expect(JN.isNonNegative(3.14)).toBe(true);
      expect(JN.isNonNegative(JN.Rational.makeInstance(355,113,sampleErrbacks))).toBe(true);
      expect(JN.isNonNegative(JN.expt(10, 400))).toBe(true);
      expect(JN.isNonNegative(JN.Roughnum.makeInstance(2.718,sampleErrbacks))).toBe(true);

    });

    it('number casts', function() {

      expect(JN.toFixnum(5, sampleErrbacks)).toBe(5);
      expect(JN.toFixnum(-5, sampleErrbacks)).toBe(-5);
      expect(JN.toFixnum(0, sampleErrbacks)).toBe(0);
      expect(JN.toFixnum(3.14, sampleErrbacks)).toBe(3.14);
      expect(JN.toFixnum(JN.Rational.makeInstance(355, 113, sampleErrbacks), sampleErrbacks))
        .toBe(355/113);
      expect(JN.toFixnum(JN.expt(10, 400, sampleErrbacks), sampleErrbacks)).toBe(Infinity);
      expect(JN.toFixnum(JN.Roughnum.makeInstance(2.718, sampleErrbacks), sampleErrbacks)).toBe(2.718);

      // toRational (toExact is its alias)
      expect(JN.toRational(5, sampleErrbacks)).toBe(5);
      expect(JN.toRational(-5, sampleErrbacks)).toBe(-5);
      expect(JN.toRational(0, sampleErrbacks)).toBe(0);
      expect(JN.toRational(3.14, sampleErrbacks)).toBe(3.14);
      expect(JN.equals(
        JN.toRational(
          JN.Rational.makeInstance(355, 113, sampleErrbacks), sampleErrbacks),
        JN.Rational.makeInstance(355, 113, sampleErrbacks),
        sampleErrbacks))
        .toBe(true);
      expect(JN.equals(
        JN.toRational(JN.expt(10, 400), sampleErrbacks),
        JN.expt(10, 400, sampleErrbacks),
        sampleErrbacks))
        .toBe(true);
      expect(JN.equals(
        JN.toRational(JN.Roughnum.makeInstance(2.718, sampleErrbacks)),
        JN.fromString('2.718', sampleErrbacks),
        sampleErrbacks))
        .toBe(true);

      // toRoughnum
      expect(JN.roughlyEquals(
        JN.toRoughnum(5, sampleErrbacks),
        JN.fromString('~5', sampleErrbacks),
        0.00001, sampleErrbacks))
        .toBe(true);
      expect(JN.roughlyEquals(
        JN.toRoughnum(-5, sampleErrbacks),
        JN.fromString('~-5', sampleErrbacks),
        0.00001, sampleErrbacks))
        .toBe(true);
      expect(JN.roughlyEquals(
        JN.toRoughnum(0, sampleErrbacks),
        JN.fromString('~0', sampleErrbacks),
        0.00001, sampleErrbacks))
        .toBe(true);
      expect(JN.roughlyEquals(
        JN.toRoughnum(3.14, sampleErrbacks),
        JN.fromString('~3.14', sampleErrbacks),
        0.00001, sampleErrbacks))
        .toBe(true);
      expect(JN.roughlyEquals(
        JN.toRoughnum(
          JN.Rational.makeInstance(355, 113, sampleErrbacks), sampleErrbacks),
        JN.Roughnum.makeInstance(355/113, sampleErrbacks),
        0.00001, sampleErrbacks))
        .toBe(true);
      expect(function() {
        JN.roughlyEquals(
          JN.toRoughnum(JN.expt(10, 400), sampleErrbacks),
          JN.toFixnum(Infinity, sampleErrbacks),
          0.00001, sampleErrbacks);
      })
        .toThrowError(/overflow/);

    });

    it('other subrs', function() {

      // toRepeatingDecimal
      expect(JN.toRepeatingDecimal(883, 700, undefined, sampleErrbacks))
        .toEqual(['1', '26', '142857']);
      expect(JN.toRepeatingDecimal(883, 700, {limit: 2}, sampleErrbacks))
        .toEqual(['1', '26', '...']);
      expect(function() {
        JN.toRepeatingDecimal(355/113, 10, undefined, sampleErrbacks);
      }).toThrowError(/not an integer/);
      expect(function() {
        JN.toRepeatingDecimal(10, 113/355, undefined, sampleErrbacks);
      }).toThrowError(/not an integer/);
      expect(function() {
        JN.toRepeatingDecimal(JN.makeRational(355, 113), 10, undefined, sampleErrbacks);
      }).toThrowError(/not an integer/);
      expect(function() {
        JN.toRepeatingDecimal(10, JN.makeRational(113, 355), undefined, sampleErrbacks);
      }).toThrowError(/not an integer/);

      // errbacks given as 3rd rather than 4th arg, as happened once on cpo
      expect(function() {
        JN.toRepeatingDecimal(355/133, 10, sampleErrbacks);
      }).toThrowError(/undefined/);
      expect(function() {
        JN.toRepeatingDecimal(10, 355/133, sampleErrbacks);
      }).toThrowError(/undefined/);
      expect(function() {
        JN.toRepeatingDecimal(944473296573929, JN.makeBignum(0), sampleErrbacks);
      }).toThrowError(/undefined/);

      // above 3 tests, with correct errbacks arg
      expect(function() {
        JN.toRepeatingDecimal(355/133, 10, undefined, sampleErrbacks);
      }).toThrowError(/is not an integer/);
      expect(function() {
        JN.toRepeatingDecimal(10, 355/133, undefined, sampleErrbacks);
      }).toThrowError(/is not an integer/);
      expect(function() {
        JN.toRepeatingDecimal(944473296573929, JN.makeBignum(0), undefined, sampleErrbacks);
      }).toThrowError(/d equals 0/);

      expect(JN._innards.getResidue(183, 700, 512, sampleErrbacks))
      .toEqual(['26', '142857']);
      expect(JN._innards.getResidue(183, 700, 2, sampleErrbacks))
      .toEqual(['26', '...']);

      // toStringDigits
      expect(JN.toStringDigits(123456789, 5, sampleErrbacks))
        .toBe("123456789.00000");
      expect(JN.toStringDigits(123456789, -5, sampleErrbacks))
        .toBe("123500000");
      expect(JN.toStringDigits(JN.makeRational(355, 113, sampleErrbacks), 5, sampleErrbacks))
        .toBe("3.14159");
      expect(JN.toStringDigits(JN.makeRational(355 * 1e9, 113, sampleErrbacks), -5, sampleErrbacks))
        .toBe("3141600000");

      // fromFixnum
      expect(JN.fromFixnum(5, sampleErrbacks)).toEqual(5);
      expect(JN.equals(JN.fromFixnum(1/2, sampleErrbacks),
        JN.makeRational(1, 2, sampleErrbacks),
        sampleErrbacks))
        .toBe(true);
      expect(JN.fromFixnum(1.5e3, sampleErrbacks)).toEqual(1500);
      expect(JN.fromFixnum(1e311, sampleErrbacks)).toBe(false);

      // sign
      expect(JN._innards.sign(JN.fromString('-3.14', sampleErrbacks), sampleErrbacks)).toBe(-1);
      expect(JN._innards.sign(JN.fromString('3.14', sampleErrbacks), sampleErrbacks)).toBe(1);
      expect(JN._innards.sign(JN.fromString('0.0', sampleErrbacks), sampleErrbacks)).toBe(0);

      // zfill
      expect(JN._innards.zfill(5)).toBe('00000');

      // liftFixnumInteger
      // liftFixnumInteger is a misnomer, as the fixnum it's called on need not be an integer.
      // in such cases, ensure that it produces an appropriate fraction
      var n3p14 = JN._innards.liftFixnumInteger(3.14,
                                                JN.Rational.makeInstance(1, 2, sampleErrbacks),
                                                sampleErrbacks);

      expect(JN.equals(n3p14, JN.Rational.makeInstance(314, 100, sampleErrbacks), sampleErrbacks))
      .toBe(true);
      // further ensure that liftFixnumInteger(3.14, <rational>, ...) produces a
      // valid rational whose numerator and denominator are integers
      expect(JN.isInteger(n3p14.numerator(sampleErrbacks))).toBe(true);
      expect(JN.isInteger(n3p14.denominator(sampleErrbacks))).toBe(true);
      //
      expect(JN.roughlyEquals(
        JN._innards.liftFixnumInteger(3.14, JN.Roughnum.makeInstance(2,sampleErrbacks),
          sampleErrbacks),
        JN.Roughnum.makeInstance(3.14, sampleErrbacks),
        0.000001,
        sampleErrbacks))
      .toBe(true);

    });

    it('BigInteger bnp* methods', function() {

      // bnpCopyTo
      var n9e311 = JN.makeBignum('9e311');
      var r = JN._innards.nbi();
      n9e311.copyTo(r);
      expect(r).toEqual(n9e311);

      // bnpAddTo
      var n1e311 = JN.makeBignum('1e311');
      r = JN._innards.nbi();
      n9e311.addTo(n1e311, r);
      var expectedR = JN.makeBignum('1e312');
      expect(r).toEqual(expectedR);

      // bnpSubTo
      var n8e311 = JN.makeBignum('8e311');
      r = JN._innards.nbi();
      n9e311.subTo(n8e311, r);
      expectedR = JN.makeBignum('1e311');
      expect(r).toEqual(expectedR);

      // bnpMultiplyTo
      r = JN._innards.nbi();
      n9e311.multiplyTo(n8e311, r);
      expectedR = JN.makeBignum('72e622');
      expect(r).toEqual(expectedR);

      // bnpSquareTo
      r = JN._innards.nbi();
      n9e311.squareTo(r);
      expectedR = JN.makeBignum('81e622');
      expect(r).toEqual(expectedR);

      // bnpDivRemTo
      var n2r5 = JN.makeBignum(JN.expt(2,5));
      var q = JN._innards.nbi();
      r = JN._innards.nbi();
      n2r5.divRemTo(JN.makeBignum(17), q,r);
      var expectedQ = JN.makeBignum(1);
      expectedR = JN.makeBignum(15);
      expect(r).toEqual(expectedR);
      expect(q).toEqual(expectedQ);

      // bnpDMultiply;
      var n2 = JN.makeBignum(2);
      n2.dMultiply(JN.makeBignum(3));
      expect(n2).toEqual(JN.makeBignum(6));

      // bnpModInt
      expect(n2r5.modInt(17)).toEqual(15);

      // bnpMillerRabin
      expect(JN.makeBignum(31).millerRabin()).toBe(true);
      expect(JN.makeBignum(32).millerRabin()).toBe(false);
      expect(JN.makeBignum(100043).millerRabin()).toBe(true);
      expect(JN.makeBignum(100051).millerRabin()).toBe(true); // [sic]

      // bnpIsEven
      expect(n2r5.isEven()).toBe(true);

      // bnpDLShiftTo
      r = JN._innards.nbi();
      n2r5.dlShiftTo(1, r);
      expectedR =  JN.makeBignum(JN.expt(2,26 + 5));
      expect(r).toEqual(expectedR);

      // bnpDRShiftTo
      var n2r31 = JN.makeBignum(JN.expt(2,26 + 5));
      r = JN._innards.nbi();
      n2r31.drShiftTo(1, r);
      expectedR = JN.makeBignum(JN.expt(2,5));
      expect(r).toEqual(expectedR);

      // bnpLShiftTo
      r = JN._innards.nbi();
      n2r5.lShiftTo(1, r);
      expectedR =  JN.makeBignum(JN.expt(2,6));
      expect(r).toEqual(expectedR);

      // bnpRShiftTo
      r = JN._innards.nbi();
      n2r5.rShiftTo(1, r);
      expectedR = JN.makeBignum(JN.expt(2,4));
      expect(r).toEqual(expectedR);

      // bnpExp
      expect(n9e311.bnpExp(JN.makeBignum(2), new JN._innards.NullExp()))
      .toEqual(JN.makeBignum('81e622'));

      expect(function() {
        n9e311.bnpExp(JN.makeBignum(0xffffffff + 1), new JN._innards.NullExp(), sampleErrbacks);
      }).toThrowError(/exponent .* too large/);


      // bnpToRadix
      expect(JN.makeBignum('1e8').toRadix())
      .toEqual('100000000');
      expect(JN.makeBignum('1e8').toRadix(10))
      .toEqual('100000000');
      expect(JN.makeBignum('11259375').toRadix(16))
      .toEqual('abcdef');
      expect(JN.makeBignum('1e8').toRadix())
      .toEqual('100000000');


    });

    it('BigInteger bn* functions', function() {

      var n2r5 = JN.makeBignum(Math.pow(2,5));

      // bnSigNum
      expect(n2r5.signum()).toEqual(1);

      // bnToString
      expect(JN.makeBignum('1e8').toString())
      .toEqual('100000000');
      expect(JN.makeBignum('1e8').toString(10))
      .toEqual('100000000');
      expect(JN.makeBignum('11259375').toString(16))
      .toEqual('abcdef');
      expect(JN.makeBignum('1e8').toString())
      .toEqual('100000000');

      // bnRemainder
      expect(JN.makeBignum(32).remainder(JN.makeBignum(17)))
      .toEqual(JN.makeBignum(15));

      // bnDivideAndRemainder
      expect(JN.makeBignum(32).divideAndRemainder(JN.makeBignum(17)))
      .toEqual([JN.makeBignum(1), JN.makeBignum(15)]);

      // bnModPow
      expect(JN.makeBignum(2).modPow(JN.makeBignum(5), JN.makeBignum(15)))
      .toEqual(JN.makeBignum(2));

      // bnIsProbablePrime
      expect(JN.makeBignum(31).isProbablePrime()).toBe(true);
      expect(JN.makeBignum(32).isProbablePrime()).toBe(false);
      expect(JN.makeBignum(100043).isProbablePrime()).toBe(true);
      expect(JN.makeBignum(100051).isProbablePrime()).toBe(false);

    });

    it('_integer* functions', function() {

      expect(JN._innards._integerIsZero(0)).toBe(true);
      expect(JN._innards._integerIsZero(1)).toBe(false);
      expect(JN._innards._integerIsZero(JN.makeBignum(0))).toBe(true);
      expect(JN._innards._integerIsZero(JN.makeBignum(1))).toBe(false);

      expect(JN._innards._integerIsOne(1)).toBe(true);
      expect(JN._innards._integerIsOne(2)).toBe(false);
      expect(JN._innards._integerIsOne(JN.makeBignum(1))).toBe(true);
      expect(JN._innards._integerIsOne(JN.makeBignum(2))).toBe(false);

      expect(JN._innards._integerIsNegativeOne(-1)).toBe(true);
      expect(JN._innards._integerIsNegativeOne(1)).toBe(false);
      expect(JN._innards._integerIsNegativeOne(JN.makeBignum(-1))).toBe(true);
      expect(JN._innards._integerIsNegativeOne(JN.makeBignum(1))).toBe(false);

      expect(JN._innards._integerGcd(12, 18, sampleErrbacks)).toEqual(6);
      expect(JN._innards._integerGcd(JN.makeBignum(12),
        JN.makeBignum(18), sampleErrbacks))
      .toEqual(JN.makeBignum(6))

      expect(JN._innards._integerModulo(12, 10, sampleErrbacks)).toEqual(2);
      expect(JN._innards._integerModulo(JN.makeBignum(12),
        JN.makeBignum(10), sampleErrbacks))
      .toEqual(JN.makeBignum(2))

      expect(JN._innards.splitIntIntoMantissaExpt('256'))
        .toEqual([2.56, 2]);
      expect(JN._innards.splitIntIntoMantissaExpt('111222333444555666777888999'))
        .toEqual([1.1122233344455567, 26]);

      expect(JN._innards._integerDivideToFixnum(2, 3, sampleErrbacks))
      .toEqual(2/3);
      expect(JN._innards._integerDivideToFixnum(JN.makeBignum('2e311'),
        JN.makeBignum('3e311'), sampleErrbacks))
      .toEqual(2/3);

      expect(JN._innards._integerEquals(2,2, sampleErrbacks))
      .toEqual(true);
      expect(JN._innards._integerEquals(JN.makeBignum('2e311'),
        JN.makeBignum('2e311'), sampleErrbacks))
      .toEqual(true);
      expect(JN._innards._integerEquals(2,3, sampleErrbacks))
      .toEqual(false);
      expect(JN._innards._integerEquals(JN.makeBignum('2e311'),
        JN.makeBignum('3e311'), sampleErrbacks))
      .toEqual(false);

      expect(JN._innards._integerGreaterThan(2,2, sampleErrbacks))
      .toEqual(false);
      expect(JN._innards._integerGreaterThan(JN.makeBignum('2e311'),
        JN.makeBignum('2e311'), sampleErrbacks))
      .toEqual(false);
      expect(JN._innards._integerGreaterThan(2,3, sampleErrbacks))
      .toEqual(false);
      expect(JN._innards._integerGreaterThan(JN.makeBignum('2e311'),
        JN.makeBignum('3e311'), sampleErrbacks))
      .toEqual(false);

      expect(JN._innards._integerLessThan(2,2, sampleErrbacks))
      .toEqual(false);
      expect(JN._innards._integerLessThan(JN.makeBignum('2e311'),
        JN.makeBignum('2e311'), sampleErrbacks))
      .toEqual(false);
      expect(JN._innards._integerLessThan(2,3, sampleErrbacks))
      .toEqual(true);
      expect(JN._innards._integerLessThan(JN.makeBignum('2e311'),
        JN.makeBignum('3e311'), sampleErrbacks))
      .toEqual(true);

      expect(JN._innards._integerGreaterThanOrEqual(2,2))
      .toEqual(true);
      expect(JN._innards._integerGreaterThanOrEqual(JN.makeBignum('2e311'),
        JN.makeBignum('2e311')))
      .toEqual(true);
      expect(JN._innards._integerGreaterThanOrEqual(2,3))
      .toEqual(false);
      expect(JN._innards._integerGreaterThanOrEqual(JN.makeBignum('2e311'),
        JN.makeBignum('3e311')))
      .toEqual(false);

      expect(JN._innards._integerLessThanOrEqual(2,2))
      .toEqual(true);
      expect(JN._innards._integerLessThanOrEqual(JN.makeBignum('2e311'),
        JN.makeBignum('2e311')))
      .toEqual(true);
      expect(JN._innards._integerLessThanOrEqual(2,3))
      .toEqual(true);
      expect(JN._innards._integerLessThanOrEqual(JN.makeBignum('2e311'),
        JN.makeBignum('3e311')))
      .toEqual(true);

    });

    it("nthRoot integerNthRoot", function() {
      expect(JN.equals(
        JN._innards.nthRoot(3, 8, sampleErrbacks),
        Math.pow(8, 1/3),
        sampleErrbacks))
      .toBe(true);
      expect(JN.roughlyEquals(
        JN._innards.nthRoot(3, 7.5, sampleErrbacks),
        Math.pow(7.5, 1/3),
        0.00001, sampleErrbacks))
      .toBe(true);
      expect(JN.roughlyEquals(
        JN._innards.nthRoot(3, 8.5, sampleErrbacks),
        Math.pow(8.5, 1/3),
        0.00001, sampleErrbacks))
      .toBe(true);
      expect(JN.equals(
        JN._innards.nthRoot(3, -8, sampleErrbacks),
        - Math.pow(8, 1/3),
        sampleErrbacks))
      .toBe(true);
      expect(JN.roughlyEquals(
        JN._innards.nthRoot(3, -7.5, sampleErrbacks),
        - Math.pow(7.5, 1/3),
        0.00001, sampleErrbacks))
      .toBe(true);
      expect(JN.roughlyEquals(
        JN._innards.nthRoot(3, -8.5, sampleErrbacks),
        - Math.pow(8.5, 1/3),
        0.00001, sampleErrbacks))
      .toBe(true);
      expect(function () {
        JN._innards.nthRoot(-3, 8, sampleErrbacks);
      })
        .toThrowError(/root .* negative/);

      expect(JN.equals(
        JN._innards.integerNthRoot(3, 8, sampleErrbacks),
        2, sampleErrbacks))
      .toBe(true);
      expect(JN.equals(
        JN._innards.integerNthRoot(3, 7.5, sampleErrbacks),
        1, sampleErrbacks))
      .toBe(true);
      expect(JN.equals(
        JN._innards.integerNthRoot(3, 8.5, sampleErrbacks),
        2, sampleErrbacks))
      .toBe(true);
      expect(function () {
        JN._innards.integerNthRoot(3, -8, sampleErrbacks);
      })
        .toThrowError(/radicand .* negative/);
      expect(function () {
        JN._innards.integerNthRoot(-3, 8, sampleErrbacks);
      })
        .toThrowError(/root .* negative/);

    });

    it("BigInteger methods", function() {

      expect(JN.equals(
        JN.gcd(JN.makeBignum(24), JN.makeBignum(30), sampleErrbacks),
        6, sampleErrbacks))
      .toBe(true);

      // BigInteger.*asin
      // shd raise exception for arg outside [-1, +1]
      // but this is not testable via Pyret, because args are always sane
      // by the time this method is called
      expect(function() { JN.makeBignum(-1.5).asin(sampleErrbacks); }).toThrowError(/out of domain/);
      expect(function() { JN.makeBignum(+1.5).asin(sampleErrbacks); }).toThrowError(/out of domain/);

      // BigInteger.*acos
      // shd raise exc for arg < -1 or > 1
      expect(function() { JN.makeBignum(-1.5).acos(sampleErrbacks); }).toThrowError(/out of domain/);
      expect(function() { JN.makeBignum(+1.5).acos(sampleErrbacks); }).toThrowError(/out of domain/);

      // BigInteger.*.atan
      // should work
      expect(JN.makeBignum(0).atan(sampleErrbacks)).toEqual(0);

      // atan2 (perhaps Pyret test is enough)
      expect(function () {
        JN.atan2(JN.makeBignum(0), JN.makeBignum(0), sampleErrbacks);
      }).toThrowError(/out of domain/);

      // BigInteger.*.sin
      // should work
      expect(JN.makeBignum(0).sin(sampleErrbacks)).toEqual(0);

      // BigInteger.*.cos
      // should work
      expect(JN.makeBignum(0).cos(sampleErrbacks)).toEqual(1);

      // BigInteger.*.tan
      // should work
      expect(JN.makeBignum(0).tan(sampleErrbacks)).toEqual(0);

      // BigInteger.*.expt calls bnPow, which calls bnpExp
      // should raise exception for too-large
      expect(function() {
        JN.makeBignum(2).expt(JN.makeBignum(0xffffffff + 1), sampleErrbacks);
      }).toThrowError(/exponent .* too large/);

      // BigInteger.*.log
      // should raise exception for arg <= 0
      expect(function() { JN.makeBignum(-1).log(sampleErrbacks); }).toThrowError(/logNonPositive/);

      // ensure log on VERYBIGINT and VERYSMALLRAT rationals converges
      var VERYBIGINT = JN.expt(9, JN.expt(5, 7));
      var VERYSMALLRAT = JN.divide(1, VERYBIGINT);
      expect(JN.roughlyEquals(VERYBIGINT.log(sampleErrbacks),
        171658, 0.1, sampleErrbacks));
      expect(JN.roughlyEquals(VERYSMALLRAT.log(sampleErrbacks),
        -171658, 0.1, sampleErrbacks));

      expect(JN.equals(
        JN.gcd(JN.makeBignum(24), JN.makeBignum(30), sampleErrbacks),
        6, sampleErrbacks))
      .toBe(true);

      expect(JN.equals(
        JN.lcm(JN.makeBignum(24), JN.makeBignum(30), sampleErrbacks),
        120, sampleErrbacks))
      .toBe(true);

    });

    it("Rational methods", function() {
      expect(function () { JN.Rational.makeInstance(undefined, undefined, sampleErrbacks); })
        .toThrowError(/undefined/);
      expect(JN.equals(JN.Rational.makeInstance(1, undefined, sampleErrbacks), 1)).toBe(true);
      expect(JN.equals(JN.Rational.makeInstance(1, -1, sampleErrbacks), -1)).toBe(true);
      expect(JN.equals(JN.Rational.makeInstance(2, 1, sampleErrbacks), 2)).toBe(true);
      expect(JN.equals(JN.Rational.makeInstance(0, 1, sampleErrbacks), 0)).toBe(true);
      expect(JN.equals(JN.Rational.makeInstance(2, 3, sampleErrbacks),
        JN.fromString("2/3", sampleErrbacks)))
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
        JN.fromString("4/3", sampleErrbacks),
        sampleErrbacks))
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

      expect(JN.Rational.makeInstance(2, 4, sampleErrbacks).toFixnum(sampleErrbacks)).toEqual(0.5);

      expect(JN.Rational.makeInstance(4, 6, sampleErrbacks)
        .numerator(sampleErrbacks))
        .toEqual(2);
      expect(JN.Rational.makeInstance(4, 6, sampleErrbacks)
        .denominator(sampleErrbacks))
        .toEqual(3);

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

      expect(JN.equals(JN.Roughnum.makeInstance(3.14, sampleErrbacks).toFixnum(sampleErrbacks), 3.14)).toBe(true);

      expect(JN.roughlyEquals(JN.Roughnum.makeInstance(3.14, sampleErrbacks), 3.14,
        0.0001, sampleErrbacks))
        .toBe(true);

      expect(JN.Roughnum.makeInstance(3.14, sampleErrbacks).isRoughnum()).toBe(true);

      expect(JN.equals(JN.Roughnum.makeInstance(3.14, sampleErrbacks).toFixnum(sampleErrbacks), 3.14)).toBe(true);

      // shouldn't roughnum's numerator method take errbacks?

      expect(JN.Roughnum.makeInstance(3.14, sampleErrbacks)
        .numerator(sampleErrbacks).toFixnum(sampleErrbacks))
        .toEqual(157);
      expect(JN.Roughnum.makeInstance(3.14, sampleErrbacks)
        .denominator(sampleErrbacks).toFixnum(sampleErrbacks))
        .toEqual(50);

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

    });

  });

  jazz.execute();

});
