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
    throwDomainError: function(x) { throw new Error('sample domainError ' + x); },
    throwLogNonPositive: function(x) { throw new Error('sample logNonPositive ' + x); },
    throwUndefinedValue: function(x) { throw new Error('sample undefinedValue ' + x); },
    throwGeneralError: function(x) { throw new Error('sample generalError ' + x); },
  };

  var JN = JNlib.MakeNumberLibrary(sampleErrbacks);

  describe("check functions that don't allow testing via Pyret programs", function() {

    it("make*opFun", function() {

      expect(function() { JN.toFixnum('a'); })
        .toThrowError(/domainError/);

     var bogusIntegerUnOpFun = JN._innards.makeIntegerUnOp(
        function(x) {
          if (x <= 2) return 10;
          return 20;
        },
        function(x) {
          if (JN.lessThanOrEqual(x, 2)) return 30;
          return 40;
        },
        {
          ignoreOverflow: true,
        }
      );

      expect(bogusIntegerUnOpFun(1)).toEqual(10);
      expect(bogusIntegerUnOpFun(3)).toEqual(20);
      expect(bogusIntegerUnOpFun(JN.makeBignum(1))).toEqual(30);
      expect(bogusIntegerUnOpFun(JN.makeBignum(3))).toEqual(40);

      var bogusIntegerBinopFun = JN._innards.makeIntegerBinop(
        function(x, y) {
          if (x <= 2 && y <= 2) return 1;
          if (x <= 2) return 2;
          if (y <= 2) return 3;
          JN._innards.errbacks.throwDomainError('makeIntegerBinop first fail');
        },
        function(x, y) {
          if (JN.lessThanOrEqual(x, 2) && JN.lessThanOrEqual(y, 2)) return 4;
          if (JN.lessThanOrEqual(x, 2)) return 5;
          if (JN.lessThanOrEqual(y, 2)) return 6;
          JN._innards.errbacks.throwDomainError('makeIntegerBinop second fail');
        },
        {
          isOverflow: true,
        }
      );

      expect(bogusIntegerBinopFun(1, 1)).toEqual(1);
      expect(bogusIntegerBinopFun(1, 3)).toEqual(2);
      expect(bogusIntegerBinopFun(3, 1)).toEqual(3);
      expect(function() { bogusIntegerBinopFun(3, 3); }).toThrowError(/first fail/);

      expect(bogusIntegerBinopFun(JN.makeBignum(1), JN.makeBignum(1))).
        toEqual(4);
      expect(bogusIntegerBinopFun(JN.makeBignum(1), JN.makeBignum(3))).
        toEqual(5);
      expect(bogusIntegerBinopFun(JN.makeBignum(3), JN.makeBignum(1))).
        toEqual(6);
      expect(function() {
        bogusIntegerBinopFun(JN.makeBignum(3), JN.makeBignum(3));
      }).toThrowError(/second fail/);

      var bogusNumericBinopFun = JN._innards.makeNumericBinop(
        function(x, y) {
          if (x <= 2 && y <= 2) return 1;
          if (x <= 2) return 2;
          if (y <= 2) return 3;
          JN._innards.errbacks.throwDomainError('makeNumericBinop first fail');
        },
        function(x, y) {
          if (JN.lessThanOrEqual(x, 2) && JN.lessThanOrEqual(y, 2)) return 4;
          if (JN.lessThanOrEqual(x, 2)) return 5;
          if (JN.lessThanOrEqual(y, 2)) return 6;
          JN._innards.errbacks.throwDomainError('makeNumericBinop second fail');
        },
        {
          isXSpecialCase: function(x) {
            return JN.lessThanOrEqual(x, 0);
          },
          onXSpecialCase: function(x, y) {
            return 7;
          },
          isYSpecialCase: function(y) {
            return JN.lessThanOrEqual(y, 0);
          },
          onYSpecialCase: function(x, y) {
            return 8;
          }
        }
      );

      expect(bogusNumericBinopFun(1, 1)).toEqual(1);
      expect(bogusNumericBinopFun(1, 3)).toEqual(2);
      expect(bogusNumericBinopFun(3, 1)).toEqual(3);
      expect(function() { bogusNumericBinopFun(3, 3); }).toThrowError(/first fail/);

      expect(bogusNumericBinopFun(JN.makeRational(3, 2), JN.makeRational(3, 2)))
        .toEqual(4);
      expect(bogusNumericBinopFun(JN.makeRational(3, 2), JN.makeRational(5, 2)))
        .toEqual(5);
      expect(bogusNumericBinopFun(JN.makeRational(5, 2), JN.makeRational(3, 2)))
        .toEqual(6);
      expect(function() { bogusNumericBinopFun(JN.makeRational(5, 2), JN.makeRational(5, 2)); })
        .toThrowError(/second fail/);

      expect(bogusNumericBinopFun(-1, +1)).toEqual(7);
      expect(bogusNumericBinopFun(+1, -1)).toEqual(8);

    });

    it("fromString", function() {
      expect(JN.fromString("5")).toEqual(5);

      var bigIntStr = "1" + new Array(309 + 1).join("0"); // 1 followed by 309 0s
      expect(JN.fromString(bigIntStr)).toEqual(JN.makeBignum(bigIntStr));

      expect(JN.equals(JN.fromString("1e1"), 10)).toBe(true);
      expect(JN.equals(JN.fromString("1e5"), JN.makeBignum("1e5")))
        .toBe(true);
      expect(JN.equals(JN.fromString("1e30"), JN.makeBignum("1e30")))
        .toBe(true);
      expect(JN.equals(JN.fromString("1e140"), JN.makeBignum("1e140")))
        .toBe(true);
      expect(JN.equals(JN.fromString("1e141"), JN.makeBignum("1e141")))
        .toBe(true);
      expect(JN.equals(JN.fromString("1e307"), JN.makeBignum("1e307")))
        .toBe(true);
      expect(JN.equals(JN.fromString("1e309"), JN.makeBignum("1e309")))
        .toBe(true);
      expect(JN.equals(JN.fromString("1e311"), JN.makeBignum("1e311")))
        .toBe(true);

      // toEqual works too, because bigints have a unique respresentation, thanks to bnpClamp()
      expect(JN.fromString("1e1"))
        .toEqual(JN.makeBignum('10'));
      expect(JN.fromString("1e5"))
        .toEqual(JN.makeBignum('1e5'));
      expect(JN.fromString("1e30"))
        .toEqual(JN.makeBignum('1e30'));
      expect(JN.fromString("1e140"))
        .toEqual(JN.makeBignum('1e140'));
      expect(JN.fromString("1e141"))
        .toEqual(JN.makeBignum('1e141'));
      expect(JN.fromString("1e307"))
        .toEqual(JN.makeBignum('1e307'));
      expect(JN.fromString("1e309"))
        .toEqual(JN.makeBignum('1e309'));
      expect(JN.fromString("1e311"))
        .toEqual(JN.makeBignum('1e311'));

      expect(JN.equals(JN.fromString("1/2"),
        JN.makeRational(1, 2)))
        .toBe(true);
      expect(JN.equals(JN.fromString("355/113"),
        JN.makeRational(355, 113)))
        .toBe(true);
      expect(JN.equals(JN.fromString("1.5e3"), 1500)).toBe(true);
      expect(JN.roughlyEquals(JN.fromString("~2.71828"),
        JN.fromFixnum(2.71828),
        0.001))
        .toBe(true);
      expect(JN.fromString("not-a-string")).toBe(false);

    });

    it('number predicates', function() {
      // isPyretNumber
      expect(JN.isPyretNumber(5)).toBe(true);
      expect(JN.isPyretNumber(-5)).toBe(true);
      expect(JN.isPyretNumber(0)).toBe(true);
      expect(JN.isPyretNumber(3.14)).toBe(true);
      expect(JN.isPyretNumber(JN.Rational.makeInstance(355,113))).toBe(true);
      expect(JN.isPyretNumber(JN.expt(10, 400))).toBe(true);
      expect(JN.isPyretNumber(JN.Roughnum.makeInstance(2.718))).toBe(true);

      // isRational
      expect(JN.isRational(5)).toBe(true);
      expect(JN.isRational(-5)).toBe(true);
      expect(JN.isRational(0)).toBe(true);
      expect(JN.isRational(3.14)).toBe(true);
      expect(JN.isRational(JN.Rational.makeInstance(355,113))).toBe(true);
      expect(JN.isRational(JN.expt(10, 400))).toBe(true);
      expect(JN.isRational(JN.Roughnum.makeInstance(2.718))).toBe(false);

      // isExact
      expect(JN.isExact(5)).toBe(true);
      expect(JN.isExact(-5)).toBe(true);
      expect(JN.isExact(0)).toBe(true);
      expect(JN.isExact(3.14)).toBe(true);
      expect(JN.isExact(JN.Rational.makeInstance(355,113))).toBe(true);
      expect(JN.isExact(JN.expt(10, 400))).toBe(true);
      expect(JN.isExact(JN.Roughnum.makeInstance(2.718))).toBe(false);

      // isReal
      expect(JN.isReal(5)).toBe(true);
      expect(JN.isReal(-5)).toBe(true);
      expect(JN.isReal(0)).toBe(true);
      expect(JN.isReal(3.14)).toBe(true);
      expect(JN.isReal(JN.Rational.makeInstance(355,113))).toBe(true);
      expect(JN.isReal(JN.expt(10, 400))).toBe(true);
      expect(JN.isReal(JN.Roughnum.makeInstance(2.718))).toBe(true);

      // isInteger
      expect(JN.isInteger(5)).toBe(true);
      expect(JN.isInteger(-5)).toBe(true);
      expect(JN.isInteger(0)).toBe(true);
      expect(JN.isInteger(3.14)).toBe(false);
      expect(JN.isInteger(JN.Rational.makeInstance(355,113))).toBe(false);
      expect(JN.isInteger(JN.expt(10, 400))).toBe(true);
      expect(JN.isInteger(JN.Roughnum.makeInstance(2.718)))
         .toBe(false);

      // isRoughnum
      expect(JN.isRoughnum(5)).toBe(false);
      expect(JN.isRoughnum(-5)).toBe(false);
      expect(JN.isRoughnum(0)).toBe(false);
      expect(JN.isRoughnum(3.14)).toBe(false);
      expect(JN.isRoughnum(JN.Rational.makeInstance(355,113))).toBe(false);
      expect(JN.isRoughnum(JN.expt(10, 400))).toBe(false);
      expect(JN.isRoughnum(JN.Roughnum.makeInstance(2.718))).toBe(true);

      // isPositive
      expect(JN.isPositive(5)).toBe(true);
      expect(JN.isPositive(-5)).toBe(false);
      expect(JN.isPositive(0)).toBe(false);
      expect(JN.isPositive(3.14)).toBe(true);
      expect(JN.isPositive(JN.Rational.makeInstance(355,113))).toBe(true);
      expect(JN.isPositive(JN.expt(10, 400))).toBe(true);
      expect(JN.isPositive(JN.Roughnum.makeInstance(2.718))).toBe(true);

      // isNonPositive
      expect(JN.isNonPositive(5)).toBe(false);
      expect(JN.isNonPositive(-5)).toBe(true);
      expect(JN.isNonPositive(0)).toBe(true);
      expect(JN.isNonPositive(3.14)).toBe(false);
      expect(JN.isNonPositive(JN.Rational.makeInstance(355,113))).toBe(false);
      expect(JN.isNonPositive(JN.expt(10, 400))).toBe(false);
      expect(JN.isNonPositive(JN.Roughnum.makeInstance(2.718))).toBe(false);

      // isNegative
      expect(JN.isNegative(5)).toBe(false);
      expect(JN.isNegative(-5)).toBe(true);
      expect(JN.isNegative(0)).toBe(false);
      expect(JN.isNegative(3.14)).toBe(false);
      expect(JN.isNegative(JN.Rational.makeInstance(355,113))).toBe(false);
      expect(JN.isNegative(JN.expt(10, 400))).toBe(false);
      expect(JN.isNegative(JN.Roughnum.makeInstance(2.718))).toBe(false);

      // isNonNegative
      expect(JN.isNonNegative(5)).toBe(true);
      expect(JN.isNonNegative(-5)).toBe(false);
      expect(JN.isNonNegative(0)).toBe(true);
      expect(JN.isNonNegative(3.14)).toBe(true);
      expect(JN.isNonNegative(JN.Rational.makeInstance(355,113))).toBe(true);
      expect(JN.isNonNegative(JN.expt(10, 400))).toBe(true);
      expect(JN.isNonNegative(JN.Roughnum.makeInstance(2.718))).toBe(true);

    });

    it('number casts', function() {

      expect(JN.toFixnum(5)).toBe(5);
      expect(JN.toFixnum(-5)).toBe(-5);
      expect(JN.toFixnum(0)).toBe(0);
      expect(JN.toFixnum(3.14)).toBe(3.14);
      expect(JN.toFixnum(JN.Rational.makeInstance(355, 113)))
        .toBe(355/113);
      expect(JN.toFixnum(JN.expt(10, 400))).toBe(Infinity);
      expect(JN.toFixnum(JN.Roughnum.makeInstance(2.718))).toBe(2.718);

      // toRational (toExact is its alias)
      expect(JN.toRational(5)).toBe(5);
      expect(JN.toRational(-5)).toBe(-5);
      expect(JN.toRational(0)).toBe(0);
      expect(JN.toRational(3.14)).toBe(3.14);
      expect(JN.equals(
        JN.toRational(JN.Rational.makeInstance(355, 113)),
        JN.Rational.makeInstance(355, 113)))
        .toBe(true);
      expect(JN.equals(
        JN.toRational(JN.expt(10, 400)),
        JN.expt(10, 400)))
        .toBe(true);
      expect(JN.equals(
        JN.toRational(JN.Roughnum.makeInstance(2.718)),
        JN.fromString('2.718')))
        .toBe(true);

      // toRoughnum
      expect(JN.roughlyEquals(
        JN.toRoughnum(5),
        JN.fromString('~5'),
        0.00001))
        .toBe(true);
      expect(JN.roughlyEquals(
        JN.toRoughnum(-5),
        JN.fromString('~-5'),
        0.00001))
        .toBe(true);
      expect(JN.roughlyEquals(
        JN.toRoughnum(0),
        JN.fromString('~0'),
        0.00001))
        .toBe(true);
      expect(JN.roughlyEquals(
        JN.toRoughnum(3.14),
        JN.fromString('~3.14'),
        0.00001))
        .toBe(true);
      expect(JN.roughlyEquals(
        JN.toRoughnum(JN.Rational.makeInstance(355, 113)),
        JN.Roughnum.makeInstance(355/113),
        0.00001))
        .toBe(true);
      expect(function() {
        JN.roughlyEquals(
          JN.expt(10, 400).toRoughnum(),
          JN.toFixnum(Infinity),
          0.00001);
      })
        .toThrowError(/overflow/);

    });

    it('other subrs', function() {

      // toRepeatingDecimal
      expect(JN.toRepeatingDecimal(883, 700, undefined))
        .toEqual(['1', '26', '142857']);
      expect(JN.toRepeatingDecimal(883, 700, {limit: 2}))
        .toEqual(['1', '26', '...']);
      expect(function() {
        JN.toRepeatingDecimal(355/113, 10, undefined);
      }).toThrowError(/not an integer/);
      expect(function() {
        JN.toRepeatingDecimal(10, 113/355, undefined);
      }).toThrowError(/not an integer/);
      expect(function() {
        JN.toRepeatingDecimal(JN.makeRational(355, 113), 10, undefined);
      }).toThrowError(/not an integer/);
      expect(function() {
        JN.toRepeatingDecimal(10, JN.makeRational(113, 355), undefined);
      }).toThrowError(/not an integer/);

      // above 3 tests, with correct errbacks arg
      expect(function() {
        JN.toRepeatingDecimal(355/133, 10, undefined);
      }).toThrowError(/is not an integer/);
      expect(function() {
        JN.toRepeatingDecimal(10, 355/133, undefined);
      }).toThrowError(/is not an integer/);
      expect(function() {
        JN.toRepeatingDecimal(944473296573929, JN.makeBignum(0), undefined);
      }).toThrowError(/d equals 0/);

      expect(JN._innards.getResidue(183, 700, 512))
      .toEqual(['26', '142857']);
      expect(JN._innards.getResidue(183, 700, 2))
      .toEqual(['26', '...']);

      // toStringDigits
      expect(JN.toStringDigits(123456789, 5))
        .toBe("123456789.00000");
      expect(JN.toStringDigits(123456789, -5))
        .toBe("123500000");
      expect(JN.toStringDigits(JN.makeRational(355, 113), 5))
        .toBe("3.14159");
      expect(JN.toStringDigits(JN.makeRational(355 * 1e9, 113), -5))
        .toBe("3141600000");

      // fromFixnum
      expect(JN.fromFixnum(5)).toEqual(5);
      expect(JN.equals(JN.fromFixnum(1/2),
        JN.makeRational(1, 2)))
        .toBe(true);
      expect(JN.fromFixnum(1.5e3)).toEqual(1500);
      expect(JN.fromFixnum(1e311)).toBe(false);

      // sign
      expect(JN._innards.sign(JN.fromString('-3.14'))).toBe(-1);
      expect(JN._innards.sign(JN.fromString('3.14'))).toBe(1);
      expect(JN._innards.sign(JN.fromString('0.0'))).toBe(0);

      // zfill
      expect(JN._innards.zfill(5)).toBe('00000');

      // liftFixnumInteger
      // liftFixnumInteger is a misnomer, as the fixnum it's called on need not be an integer.
      // in such cases, ensure that it produces an appropriate fraction
      var n3p14 = JN._innards.liftFixnumInteger(3.14,
                                                JN.Rational.makeInstance(1, 2));

      expect(JN.equals(n3p14, JN.Rational.makeInstance(314, 100)))
      .toBe(true);
      // further ensure that liftFixnumInteger(3.14, <rational>, ...) produces a
      // valid rational whose numerator and denominator are integers
      expect(JN.isInteger(n3p14.numerator())).toBe(true);
      expect(JN.isInteger(n3p14.denominator())).toBe(true);
      //
      expect(JN.roughlyEquals(
        JN._innards.liftFixnumInteger(3.14, JN.Roughnum.makeInstance(2)),
        JN.Roughnum.makeInstance(3.14),
        0.000001))
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
        n9e311.bnpExp(JN.makeBignum(0xffffffff + 1), new JN._innards.NullExp());
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

      expect(JN._innards._integerGcd(12, 18)).toEqual(6);
      expect(JN._innards._integerGcd(JN.makeBignum(12),
        JN.makeBignum(18)))
      .toEqual(JN.makeBignum(6))

      expect(JN._innards._integerModulo(12, 10)).toEqual(2);
      expect(JN._innards._integerModulo(JN.makeBignum(12),
        JN.makeBignum(10)))
      .toEqual(JN.makeBignum(2))

      expect(JN._innards.splitIntIntoMantissaExpt('256'))
        .toEqual([2.56, 2]);
      expect(JN._innards.splitIntIntoMantissaExpt('111222333444555666777888999'))
        .toEqual([1.1122233344455567, 26]);

      expect(JN._innards._integerDivideToFixnum(2, 3))
      .toEqual(2/3);
      expect(JN._innards._integerDivideToFixnum(JN.makeBignum('2e311'),
        JN.makeBignum('3e311')))
      .toEqual(2/3);

      expect(JN._innards._integerEquals(2,2))
      .toEqual(true);
      expect(JN._innards._integerEquals(JN.makeBignum('2e311'),
        JN.makeBignum('2e311')))
      .toEqual(true);
      expect(JN._innards._integerEquals(2,3))
      .toEqual(false);
      expect(JN._innards._integerEquals(JN.makeBignum('2e311'),
        JN.makeBignum('3e311')))
      .toEqual(false);

      expect(JN._innards._integerGreaterThan(2,2))
      .toEqual(false);
      expect(JN._innards._integerGreaterThan(JN.makeBignum('2e311'),
        JN.makeBignum('2e311')))
      .toEqual(false);
      expect(JN._innards._integerGreaterThan(2,3))
      .toEqual(false);
      expect(JN._innards._integerGreaterThan(JN.makeBignum('2e311'),
        JN.makeBignum('3e311')))
      .toEqual(false);

      expect(JN._innards._integerLessThan(2,2))
      .toEqual(false);
      expect(JN._innards._integerLessThan(JN.makeBignum('2e311'),
        JN.makeBignum('2e311')))
      .toEqual(false);
      expect(JN._innards._integerLessThan(2,3))
      .toEqual(true);
      expect(JN._innards._integerLessThan(JN.makeBignum('2e311'),
        JN.makeBignum('3e311')))
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
        JN._innards.nthRoot(3, 8),
        Math.pow(8, 1/3)))
      .toBe(true);
      expect(JN.roughlyEquals(
        JN._innards.nthRoot(3, 7.5),
        Math.pow(7.5, 1/3),
        0.00001))
      .toBe(true);
      expect(JN.roughlyEquals(
        JN._innards.nthRoot(3, 8.5),
        Math.pow(8.5, 1/3),
        0.00001))
      .toBe(true);
      expect(JN.equals(
        JN._innards.nthRoot(3, -8),
        - Math.pow(8, 1/3)))
      .toBe(true);
      expect(JN.roughlyEquals(
        JN._innards.nthRoot(3, -7.5),
        - Math.pow(7.5, 1/3),
        0.00001))
      .toBe(true);
      expect(JN.roughlyEquals(
        JN._innards.nthRoot(3, -8.5),
        - Math.pow(8.5, 1/3),
        0.00001))
      .toBe(true);
      expect(function () {
        JN._innards.nthRoot(-3, 8);
      })
        .toThrowError(/root .* negative/);

      expect(JN.equals(
        JN._innards.integerNthRoot(3, 8),
        2))
      .toBe(true);
      expect(JN.equals(
        JN._innards.integerNthRoot(3, 7.5),
        1))
      .toBe(true);
      expect(JN.equals(
        JN._innards.integerNthRoot(3, 8.5),
        2))
      .toBe(true);
      expect(function () {
        JN._innards.integerNthRoot(3, -8);
      })
        .toThrowError(/radicand .* negative/);
      expect(function () {
        JN._innards.integerNthRoot(-3, 8);
      })
        .toThrowError(/root .* negative/);

    });

    it("BigInteger methods", function() {

      expect(JN.equals(
        JN.gcd(JN.makeBignum(24), JN.makeBignum(30)),
        6))
      .toBe(true);

      // BigInteger.*asin
      // shd raise exception for arg outside [-1, +1]
      // but this is not testable via Pyret, because args are always sane
      // by the time this method is called
      expect(function() { JN.makeBignum(-1.5).asin(); }).toThrowError(/out of domain/);
      expect(function() { JN.makeBignum(+1.5).asin(); }).toThrowError(/out of domain/);

      // BigInteger.*acos
      // shd raise exc for arg < -1 or > 1
      expect(function() { JN.makeBignum(-1.5).acos(); }).toThrowError(/out of domain/);
      expect(function() { JN.makeBignum(+1.5).acos(); }).toThrowError(/out of domain/);

      // BigInteger.*.atan
      // should work
      expect(JN.makeBignum(0).atan()).toEqual(0);

      // atan2 (perhaps Pyret test is enough)
      expect(function () {
        JN.atan2(JN.makeBignum(0), JN.makeBignum(0));
      }).toThrowError(/out of domain/);

      // BigInteger.*.sin
      // should work
      expect(JN.makeBignum(0).sin()).toEqual(0);

      // BigInteger.*.cos
      // should work
      expect(JN.makeBignum(0).cos()).toEqual(1);

      // BigInteger.*.tan
      // should work
      expect(JN.makeBignum(0).tan()).toEqual(0);

      // BigInteger.*.expt calls bnPow, which calls bnpExp
      // should raise exception for too-large
      expect(function() {
        JN.makeBignum(2).expt(JN.makeBignum(0xffffffff + 1));
      }).toThrowError(/exponent .* too large/);

      // BigInteger.*.log
      // should raise exception for arg <= 0
      expect(function() { JN.makeBignum(-1).log(); }).toThrowError(/logNonPositive/);

      // ensure log on VERYBIGINT and VERYSMALLRAT rationals converges
      var VERYBIGINT = JN.expt(9, JN.expt(5, 7));
      var VERYSMALLRAT = JN.divide(1, VERYBIGINT);
      expect(JN.roughlyEquals(VERYBIGINT.log(),
        171658, 0.1));
      expect(JN.roughlyEquals(VERYSMALLRAT.log(),
        -171658, 0.1));

      expect(JN.equals(
        JN.gcd(JN.makeBignum(24), JN.makeBignum(30)),
        6))
      .toBe(true);

      expect(JN.equals(
        JN.lcm(JN.makeBignum(24), JN.makeBignum(30)),
        120))
      .toBe(true);

    });

    it("Rational methods", function() {
      expect(function () { JN.Rational.makeInstance(undefined, undefined); })
        .toThrowError(/undefined/);
      expect(JN.equals(JN.Rational.makeInstance(1, undefined), 1)).toBe(true);
      expect(JN.equals(JN.Rational.makeInstance(1, -1), -1)).toBe(true);
      expect(JN.equals(JN.Rational.makeInstance(2, 1), 2)).toBe(true);
      expect(JN.equals(JN.Rational.makeInstance(0, 1), 0)).toBe(true);
      expect(JN.equals(JN.Rational.makeInstance(2, 3),
        JN.fromString("2/3")))
        .toBe(true);

      expect(JN.Rational.makeInstance(1, 3).equals(
        JN.Rational.makeInstance(1, 3)))
        .toBe(true);

      expect(JN.Rational.makeInstance(2, 3).toString()).toBe("2/3");
      expect(JN.Rational.makeInstance(2, 1).toString()).toBe("2");

      expect(JN.equals(JN.Rational.makeInstance(1, 3).add(
        JN.Rational.makeInstance(2, 3)),
        1))
        .toBe(true);

      expect(JN.equals(JN.Rational.makeInstance(4, 3).subtract(
        JN.Rational.makeInstance(1, 3)),
        1))
        .toBe(true);

      expect(JN.equals(JN.Rational.makeInstance(-4, 3).negate(),
        JN.fromString("4/3")))
        .toBe(true);

      expect(JN.equals(JN.Rational.makeInstance(2, 3).multiply(
        JN.Rational.makeInstance(3, 2)),
        1))
        .toBe(true);

      expect(JN.equals(JN.Rational.makeInstance(2, 3).divide(
        JN.Rational.makeInstance(4, 6)),
        1))
        .toBe(true);

      // toRational?
      expect(JN.Rational.makeInstance(2, 3).isRational()).toBe(true);

      expect(JN.Rational.makeInstance(2, 4).toFixnum()).toEqual(0.5);

      expect(JN.Rational.makeInstance(4, 6)
        .numerator())
        .toEqual(2);
      expect(JN.Rational.makeInstance(4, 6)
        .denominator())
        .toEqual(3);

      expect(JN.Rational.makeInstance(2, 3).greaterThan(
        JN.Rational.makeInstance(1, 3)))
        .toBe(true);

      expect(JN.Rational.makeInstance(1, 3).greaterThanOrEqual(
        JN.Rational.makeInstance(1, 3)))
        .toBe(true);

      expect(JN.Rational.makeInstance(1, 3).lessThan(
        JN.Rational.makeInstance(2, 3)))
        .toBe(true);

      expect(JN.Rational.makeInstance(1, 3).lessThanOrEqual(
        JN.Rational.makeInstance(1, 3)))
        .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(101, 4).integerSqrt(),
        5))
        .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(100, 9).sqrt(),
        JN.Rational.makeInstance(10, 3)))
        .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(-4, 3).abs(),
        JN.Rational.makeInstance(4, 3)))
        .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(4, 3).floor(),
        1))
      .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(4, 3).ceiling(),
        2))
      .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(4, 3).round(),
        1))
      .toBe(true);

      expect(JN.equals(
        JN.Rational.makeInstance(7, 2).roundEven(),
        4))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(5, 2).log(),
        JN.fromFixnum(0.91629),
        0.001))
      .toBe(true);

      // tan(pi/4) == 1
      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(355, 4 * 113).tan(),
        1, 0.001))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(1000, 1732).atan(),
        JN.makeRoughnum(355 / (6 * 113)),
        0.001))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(1732, 1000).atan(),
        JN.fromFixnum(355 / (3 * 113)),
        0.001))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(355, 2 * 113).cos(),
        0, 0.001))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(355, 2 * 113).sin(),
        1, 0.001))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(9, 4).expt(
          JN.Rational.makeInstance(3, 2)),
        27 / 8, 0.001))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(3, 2).exp(),
        JN.fromFixnum(Math.exp(1.5)), 0.001))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(1, 2).acos(),
        JN.Rational.makeInstance(355, 3 * 113),
        0.001))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Rational.makeInstance(1, 2).asin(),
        JN.Rational.makeInstance(355, 6 * 113),
        0.001))
      .toBe(true);

    });

    it("Roughnum methods", function() {

      expect(function () { JN.Roughnum.makeInstance(undefined); })
        .toThrowError(/unsuitable/);

      expect(JN.equals(JN.Roughnum.makeInstance(3.14).toFixnum(), 3.14)).toBe(true);

      expect(JN.roughlyEquals(JN.Roughnum.makeInstance(3.14), 3.14,
        0.0001))
        .toBe(true);

      expect(JN.Roughnum.makeInstance(3.14).isRoughnum()).toBe(true);

      expect(JN.equals(JN.Roughnum.makeInstance(3.14).toFixnum(), 3.14)).toBe(true);

      expect(JN.Roughnum.makeInstance(3.14)
        .numerator().toFixnum())
        .toEqual(157);
      expect(JN.Roughnum.makeInstance(3.14)
        .denominator().toFixnum())
        .toEqual(50);

      expect(JN.equals(
        JN.Roughnum.makeInstance(3.14).floor(),
        3))
      .toBe(true);

      expect(JN.equals(
        JN.Roughnum.makeInstance(3.14).ceiling(),
        4))
      .toBe(true);

      expect(JN.equals(
        JN.Roughnum.makeInstance(3.14).round(),
        3))
      .toBe(true);

      expect(JN.equals(
        JN.Roughnum.makeInstance(3.5).roundEven(),
        4))
      .toBe(true);

      expect(JN.Roughnum.makeInstance(2.3).greaterThan(
        JN.Roughnum.makeInstance(1.3)))
        .toBe(true);

      expect(JN.Roughnum.makeInstance(1.3).greaterThanOrEqual(
        JN.Roughnum.makeInstance(1.3)))
        .toBe(true);

      expect(JN.Roughnum.makeInstance(1.3).lessThan(
        JN.Roughnum.makeInstance(2.3)))
        .toBe(true);

      expect(JN.Roughnum.makeInstance(1.3).lessThanOrEqual(
        JN.Roughnum.makeInstance(1.3)))
        .toBe(true);

      // why is roughnum integersqrt so different

      expect(function() {
        JN.Roughnum.makeInstance(101).integerSqrt();
      }).toThrowError(/can only be applied to an integer/);

      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(100).sqrt(),
        JN.Roughnum.makeInstance(10),
        0.0001))
        .toBe(true);

      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(-3.14).abs(),
        JN.Roughnum.makeInstance(3.14),
        0.0001))
        .toBe(true);

      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(2.5).log(),
        JN.fromFixnum(0.91629),
        0.001))
      .toBe(true);

      // tan(pi/4) == 1
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance((355 / (4 * 113))).tan(),
        1, 0.001))
      .toBe(true);

      // tan(pi/6) = 1/sqrt(3)
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(1/1.732).atan(),
        JN.fromFixnum(355 / (6 * 113)),
        0.001))
      .toBe(true);

      // tan(pi/3) = sqrt(3)
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(1.732).atan(),
        JN.fromFixnum(355 / (3 * 113)),
        0.001))
      .toBe(true);

      // cos(pi/2) = 0
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(355 / (2 * 113)).cos(),
        0, 0.001))
      .toBe(true);

      // sin(pi/2) = 1
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(355 / (2 * 113)).sin(),
        1, 0.001))
      .toBe(true);

      // (9/4)^(3/2) = 27/8
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(9/4).expt(
          JN.Roughnum.makeInstance(3/2)),
        27 / 8, 0.001))
      .toBe(true);

      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(3/2).exp(),
        JN.fromFixnum(Math.exp(1.5)), 0.001))
      .toBe(true);

      // cos(pi/3) = 1/2
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(1/2).acos(),
        JN.Roughnum.makeInstance(355/(3 * 113)),
        0.001))
      .toBe(true);

      // sin(pi/6) = 1/2
      expect(JN.roughlyEquals(
        JN.Roughnum.makeInstance(1/2).asin(),
        JN.Roughnum.makeInstance(355/(6 * 113)),
        0.001))
      .toBe(true);

    });

  });

  jazz.execute();

});
