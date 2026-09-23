// Modifications to Danny Yoo's js-numbers library, whose LICENSE is:

/*

Licensing
---------

This software is covered under the following copyright:

 *
 * Copyright (c) 2010  Danny Yoo
 * All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL TOM WU BE LIABLE FOR ANY SPECIAL, INCIDENTAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF
 * THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT
 * OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * In addition, the following condition applies:
 *
 * All redistributions must retain an intact copy of this copyright notice
 * and disclaimer.
 *

======================================================================

js-numbers uses code from the jsbn library.  The LICENSE to it is:

Licensing
---------

This software is covered under the following copyright:

 *
 * Copyright (c) 2003-2005  Tom Wu
 * All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL TOM WU BE LIABLE FOR ANY SPECIAL, INCIDENTAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF
 * THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT
 * OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * In addition, the following condition applies:
 *
 * All redistributions must retain an intact copy of this copyright notice
 * and disclaimer.
 *

Address all questions regarding this license to:

  Tom Wu
  tjw@cs.Stanford.EDU

*/

/*

No notion of levels (complex, exact, inexact, flonum).

No complex numbers.

Added roughnums.

pyretnum := fixnum | boxnum

A fixnum is simply a JS double, and we prefer to use them
whenever possible, viz., for integers that are small enough.

boxnum := BigInteger | Rational | Roughnum.

An integer is either a fixnum or a BigInteger.

*/

define("pyret-base/js/js-numbers", function() {
  'use strict';
  function MakeNumberLibrary(errbacks) {
    // Abbreviation
    var Numbers = {};

    // makeNumericBinop: (fixnum fixnum -> any) (pyretnum pyretnum -> any) -> (pyretnum pyretnum) X
    // Creates a binary function that works either on fixnums or boxnums.
    // Applies the appropriate binary function, ensuring that both pyretnums are
    // coerced to be the same kind.
    var makeNumericBinop = function(onFixnums, onBoxednums, options) {
      options = options || {};
      return function(x, y) {

        if (options.isXSpecialCase && options.isXSpecialCase(x))
          return options.onXSpecialCase(x, y);
        if (options.isYSpecialCase && options.isYSpecialCase(y))
          return options.onYSpecialCase(x, y);

        if (typeof(x) === 'number' &&
            typeof(y) === 'number') {
          return onFixnums(x, y);
        }

        if (typeof(x) === 'number') {
          x = liftFixnumInteger(x, y);
        }
        if (typeof(y) === 'number') {
          y = liftFixnumInteger(y, x);
        }

        if (x instanceof Roughnum) {
          // y is rough, rat or bigint
          if (!(y instanceof Roughnum)) {
            // y is rat or bigint
            y = y.toRoughnum();
          }
        } else if (y instanceof Roughnum) {
          // x is rat or bigint
          x = x.toRoughnum();
        } else if (x instanceof Rational) {
          // y is rat or bigint
          if (!(y instanceof Rational)) {
            // y is bigint
            y = new Rational(y, 1);
          }
        } else if (y instanceof Rational) {
          // x is bigint
          x = new Rational(x, 1);
        }

        var result = onBoxednums(x, y);
        if (result instanceof BigInteger) { return normalizeInteger(result); }
        return result;
      };
    };

    // normalizeInteger: integer-pyretnum -> integer-pyretnum
    // Collapses a BigInteger back to a fixnum when it fits, so that
    // fixnums stay the canonical representation of small integers.
    var normalizeInteger = function(n) {
      if (n instanceof BigInteger && n.v >= BIG_MIN_FIXNUM && n.v <= BIG_MAX_FIXNUM) {
        return Number(n.v);
      }
      return n;
    };

    // fromFixnum: fixnum -> pyretnum
    var fromFixnum = function(x) {
      if (typeof(x) === 'number' && !isFinite(x)) {
        errbacks.throwDomainError('fromFixnum: cannot convert ' + x + ' to a number');
      }
      return fromString(String(x));
    };

    var expandExponent = function(s) {
      var match = s.match(scientificPattern), mantissaChunks, exponent;
      if (match) {
        mantissaChunks = match[1].match(/^([^.]*)(.*)$/);
        exponent = Number(match[2]);

        if (mantissaChunks[2].length === 0) {
          return mantissaChunks[1] + zfill(exponent);
        }

        if (exponent >= mantissaChunks[2].length - 1) {
          return (mantissaChunks[1] +
                  mantissaChunks[2].substring(1) +
                  zfill(exponent - (mantissaChunks[2].length - 1)));
        } else {
          return (mantissaChunks[1] +
                  mantissaChunks[2].substring(1, 1+exponent));
        }
      } else {
        return s;
      }
    };

    // zfill: integer -> string
    // builds a string of "0"'s of length n.
    var zfill = function(n) {
      var buffer = [];
      buffer.length = n;
      for (var i = 0; i < n; i++) {
        buffer[i] = '0';
      }
      return buffer.join('');
    };

    // liftFixnumInteger: fixnum-integer boxed-pyretnum -> boxed-pyretnum
    // Lifts up fixnum integers to a boxed type.

    var liftFixnumInteger = function(x, other) {
      if (other instanceof Roughnum)
        return new Roughnum(x);
      else if (other instanceof BigInteger)
        return makeBignum(x);
      else
        return fromFixnum(x);
    };


    // isPyretNumber: any -> boolean
    // Pure predicate. Raw JS numbers must be integers (the fixnum
    // representation) to count as pyretnums; non-integer JS numbers
    // are not valid pyretnums and return false.
    var isPyretNumber = function(thing) {
      if (typeof(thing) === 'number') return Number.isInteger(thing);
      return (thing instanceof Rational ||
              thing instanceof Roughnum ||
              thing instanceof BigInteger);
    };

    // assertPyretNumber: contract check for entry-point operations that
    // require a pyretnum. Throws a domainError for clearly wrong types
    // (string, null, etc.). For non-integer JS numbers, logs to
    // console.error rather than throwing — we don't yet have full
    // coverage of all call sites that may pass these in, and an
    // exception in the wild from existing code we haven't audited would
    // be worse than the current lax behavior. Flip the non-integer
    // branch to errbacks.throwDomainError once we're confident.
    var assertPyretNumber = function(thing, callerName) {
      if (typeof thing === 'number') {
        if (!Number.isInteger(thing)) {
          console.error(
            'js-numbers ' + callerName +
              ': non-integer JS number leaked into pyretnum: ' + thing,
            new Error('stack').stack);
        }
        return;
      }
      if (thing instanceof Rational ||
          thing instanceof Roughnum ||
          thing instanceof BigInteger) {
        return;
      }
      errbacks.throwDomainError(
        callerName + ': arg ' + thing + ' is not a number.');
    };

    // isRational: pyretnum -> boolean
    var isRational = function(n) {
      if (!isPyretNumber(n)) return false;
      if (typeof(n) === 'number') return true;
      return n.isRational();
    };

    var isExact = isRational;

    // isReal: pyretnum -> boolean
    var isReal = function(n) {
      if (!isPyretNumber(n)) return false;
      if (typeof(n) === 'number') return true;
      return n.isReal();
    };

    // isInteger: pyretnum -> boolean
    var isInteger = function(n) {
      if (!isPyretNumber(n)) return false;
      if (typeof(n) === 'number') return true;
      return n.isInteger();
    };

    var isRoughnum = function(n) {
      if (!isPyretNumber(n)) return false;
      if (typeof(n) === 'number') return false;
      return n.isRoughnum();
    };

    var isPositive = function(n) {
      if (!isPyretNumber(n)) return false;
      if (typeof(n) === 'number') return n > 0;
      return n.isPositive();
    };

    var isNonPositive = function(n) {
      if (!isPyretNumber(n)) return false;
      if (typeof(n) === 'number') return n <= 0;
      return n.isNonPositive();
    };

    var isNegative = function(n) {
      if (!isPyretNumber(n)) return false;
      if (typeof(n) === 'number') return n < 0;
      return n.isNegative();
    };

    var isNonNegative = function(n) {
      if (!isPyretNumber(n)) return false;
      if (typeof(n) === 'number') return n >= 0;
      return n.isNonNegative();
    };

    // toFixnum: pyretnum -> javascript-number
    var toFixnum = function(n) {
      assertPyretNumber(n, 'toFixnum');
      if (typeof(n) === 'number') return n;
      return n.toFixnum();
    };

    // toRational: pyretnum -> pyretnum
    var toRational = function(n) {
      if (typeof(n) === 'number')
        return n;
      return n.toRational();
    };

    var toExact = toRational;

    // toRoughnum: pyretnum -> pyretnum

    var toRoughnum = function(n) {
      if (typeof(n) === 'number') {
        return Roughnum.makeInstance(n);
      } else {
        return n.toRoughnum();
      }
    };

    //////////////////////////////////////////////////////////////////////

    // add: pyretnum pyretnum -> pyretnum
    var add = function(x, y) {
      var sum;
      if (typeof(x) === 'number' && typeof(y) === 'number') {
        sum = x + y;
        if (isOverflow(sum)) {
          return (makeBignum(x)).add(makeBignum(y));
        }
        else {
          return sum;
        }
      }
      return addSlow(x, y);
    };

    var addSlow = makeNumericBinop(
      function(x, y) {
        var sum = x + y;
        if (isOverflow(sum)) {
          return (makeBignum(x)).add(makeBignum(y));
        } else {
          return sum;
        }
      },
      function(x, y) {
        return x.add(y);
      },
      {isXSpecialCase: function(x) {
        return isInteger(x) && _integerIsZero(x) },
       onXSpecialCase: function(x, y) { return y; },
       isYSpecialCase: function(y) {
         return isInteger(y) && _integerIsZero(y) },
       onYSpecialCase: function(x, y) { return x; }
      });

    var subtract = function(x, y) {
      if (typeof(x) === 'number' && typeof(y) === 'number') {
        var diff = x - y;
        if (isOverflow(diff)) {
          return (makeBignum(x)).subtract(makeBignum(y));
        } else {
          return diff;
        }
      }
      return subtractSlow(x, y);
    };

    // subtract: pyretnum pyretnum -> pyretnum
    var subtractSlow = makeNumericBinop(
      function(x, y) {
        var diff = x - y;
        if (isOverflow(diff)) {
          return (makeBignum(x)).subtract(makeBignum(y));
        } else {
          return diff;
        }
      },
      function(x, y) {
        return x.subtract(y);
      },
      {isXSpecialCase: function(x) {
        return isInteger(x) && _integerIsZero(x) },
       onXSpecialCase: function(x, y) { return negate(y); },
       isYSpecialCase: function(y) {
         return isInteger(y) && _integerIsZero(y) },
       onYSpecialCase: function(x, y) { return x; }
      });

    // mulitply: pyretnum pyretnum -> pyretnum
    var multiply = function(x, y) {
      var prod;
      if (typeof(x) === 'number' && typeof(y) === 'number') {
        prod = x * y;
        if (isOverflow(prod)) {
          return (makeBignum(x)).multiply(makeBignum(y));
        } else {
          return prod;
        }
      }
      return multiplySlow(x, y);
    };
    var multiplySlow = makeNumericBinop(
      function(x, y) {
        var prod = x * y;
        if (isOverflow(prod)) {
          return (makeBignum(x)).multiply(makeBignum(y));
        } else {
          return prod;
        }
      },
      function(x, y) {
        return x.multiply(y);
      },
      {isXSpecialCase: function(x) {
        return (isInteger(x) &&
                (_integerIsZero(x) || _integerIsOne(x) || _integerIsNegativeOne(x))) },
       onXSpecialCase: function(x, y) {
         if (_integerIsZero(x))
           return 0;
         if (_integerIsOne(x))
           return y;
         if (_integerIsNegativeOne(x))
           return negate(y);
       },
       isYSpecialCase: function(y) {
         return (isInteger(y) &&
                 (_integerIsZero(y) || _integerIsOne(y) || _integerIsNegativeOne(y)))},
       onYSpecialCase: function(x, y) {
         if (_integerIsZero(y))
           return 0;
         if (_integerIsOne(y))
           return x;
         if (_integerIsNegativeOne(y))
           return negate(x);
       }
      });

    // divide: pyretnum pyretnum -> pyretnum
    var divide = makeNumericBinop(
      function(x, y) {
        if (_integerIsZero(y))
          errbacks.throwDivByZero("/: division by zero, " + x + ' ' + y);
        var div = x / y;
        if (isOverflow(div)) {
          return (makeBignum(x)).divide(makeBignum(y));
        } else if (Math.floor(div) !== div) {
          return Rational.makeInstance(x, y);
        } else {
          return div;
        }
      },
      function(x, y) {
        if (equalsAnyZero(y)) {
          errbacks.throwDivByZero('/: division by zero, ' + x + ' ' + y);
        }
        return x.divide(y);
      },
      {
        isXSpecialCase: function(x) {
          return equalsAnyZero(x);
        },
        onXSpecialCase: function(x, y) {
          if (equalsAnyZero(y)) {
            errbacks.throwDivByZero("/: division by zero, " + x + ' ' + y);
          }
          return (x instanceof Roughnum) ? x : 0;
        },
        isYSpecialCase: function(y) {
          return equalsAnyZero(y);
        },
        onYSpecialCase: function(x, y) {
          errbacks.throwDivByZero("/: division by zero, " + x + ' ' + y);
        }
      });

    var equals = function(x, y) {
      if (x === y) { return true; }
      else {
        if (typeof x === "number" && typeof y === "number") { return false; }
        else {
          return equalsSlow(x, y);
        }
      }
    };
    // equals: pyretnum pyretnum -> boolean
    var equalsSlow = makeNumericBinop(
      function(x, y) {
        return x === y;
      },
      function(x, y) {
        return x.equals(y);
      });

    var equalsAnyZero = function(x) {
      if (typeof(x) === 'number') return x === 0;
      if (x instanceof Roughnum) return x.n === 0;
      if (x instanceof BigInteger) return _integerIsZero(x);
      return false;
    };

    // eqv: pyretnum pyretnum -> boolean
    var eqv = function(x, y) {
      if (x === y)
        return true;
      if (typeof(x) === 'number' && typeof(y) === 'number')
        return x === y;
      if (x instanceof Roughnum && y instanceof Roughnum)
        return x.n === y.n;
      var ex = isRational(x), ey = isRational(y);
      return (((ex && ey) || (!ex && !ey)) && equals(x, y));
    };

    // approxEqual: pyretnum pyretnum pyretnum -> boolean
    var approxEquals = function(x, y, delta) {
      return lessThanOrEqual(abs(subtract(x, y)),
                             delta);
    };

    // used for within
    var roughlyEquals = function(x, y, delta) {
      if (isNegative(delta)) {
        errbacks.throwToleranceError("negative tolerance " + delta);
      }

      if (x === y) return true;

      if (isRoughnum(delta) && delta.n === Number.MIN_VALUE) {
        if ((isRoughnum(x) || isRoughnum(y)) &&
            (Math.abs(subtract(x,y).n) === Number.MIN_VALUE)) {
          errbacks.throwToleranceError("roughnum tolerance too small for meaningful comparison, " + x + ' ' + y + ' ' + delta);
        }
      }

      var ratx = isRoughnum(x) ? x.toRational() : x;
      var raty = isRoughnum(y) ? y.toRational() : y;

      var ratdelta = isRoughnum(delta) ? delta.toRational() : delta;
      return approxEquals(ratx, raty, ratdelta);
    };

    var roughlyEqualsRel = function(computedValue, trueValue, delta, smoothed) {
      if (isNegative(delta)) {
        errbacks.throwRelToleranceError('negative relative tolerance ' + delta)
      }

      if (computedValue === trueValue) {
        return true
      }

      var deltaIsRough = isRoughnum(delta)
      var argNumsAreRough = isRoughnum(computedValue) || isRoughnum(trueValue)

      var ratCv = isRoughnum(computedValue) ? computedValue.toRational() : computedValue
      var ratTv = isRoughnum(trueValue) ? trueValue.toRational() : trueValue

      var ratDelta = isRoughnum(delta) ? delta.toRational(): delta

      var err = abs(subtract(ratCv, ratTv))
      var denom = min(abs(ratCv), abs(ratTv))
      if (smoothed) {
        denom = add(denom, 1);
      }

      if (lessThanOrEqual(ratDelta, 1)) {
        var absDelta = multiply(ratDelta, denom)
        if (deltaIsRough && toRoughnum(absDelta).n === Number.MIN_VALUE) {
          if (argNumsAreRough && Math.abs(toRoughnum(err).n) === Number.MIN_VALUE) {
            errbacks.throwRelToleranceError('roughnum tolerance too small for meaningful comparison, ' +
                                            computedValue + ' ' + trueValue + ' ' + delta)
          }
        }

        return lessThanOrEqual(err, absDelta)
      } else {
        var errRatio = divide(err, denom)

        if (deltaIsRough && delta.n === Number.MIN_VALUE) {
          if (argNumsAreRough && Math.abs(toRoughnum(errRatio).n) === Number.MIN_VALUE) {
            errbacks.throwRelToleranceError('roughnum tolerance too small for meaningful comparison, ' +
                                            computedValue + ' ' + trueValue + ' ' + delta)
          }
        }

        return lessThanOrEqual(errRatio, ratDelta)
      }
    }

    // greaterThanOrEqual: pyretnum pyretnum -> boolean
    var greaterThanOrEqual = function(x, y) {
      if(typeof x === "number" && typeof y === "number") {
        return x >= y;
      }
      if (x instanceof Roughnum || y instanceof Roughnum) {
        return toFixnum(x) >= toFixnum(y);
      }
      return makeNumericBinop(undefined, function(x, y) {
        return x.greaterThanOrEqual(y);
      })(x, y);
    }

    // lessThanOrEqual: pyretnum pyretnum -> boolean
    var lessThanOrEqual = function(x, y) {
      if(typeof x === "number" && typeof y === "number") {
        return x <= y;
      }
      if (x instanceof Roughnum || y instanceof Roughnum) {
        return toFixnum(x) <= toFixnum(y);
      }
      return makeNumericBinop(undefined, function(x, y) {
        return x.lessThanOrEqual(y);
      })(x, y);
    };

    // greaterThan: pyretnum pyretnum -> boolean
    var greaterThan = function(x, y) {
      if(typeof x === "number" && typeof y === "number") {
        return x > y;
      }
      if (x instanceof Roughnum || y instanceof Roughnum) {
        return toFixnum(x) > toFixnum(y);
      }
      return makeNumericBinop(undefined, function(x, y) {
        return x.greaterThan(y);
      })(x, y);
    };

    // lessThan: pyretnum pyretnum -> boolean
    var lessThan = function(x, y) {
      if(typeof x === "number" && typeof y === "number") {
        return x < y;
      }
      if (x instanceof Roughnum || y instanceof Roughnum) {
        return toFixnum(x) < toFixnum(y);
      }
      return makeNumericBinop(undefined, function(x, y) {
        return x.lessThan(y);
      })(x, y);
    };

    // expt: pyretnum pyretnum -> pyretnum
    var expt = makeNumericBinop(
      function(x, y) {
        // Reached only with integer y >= 1 (0 and negative exponents are
        // special-cased below); Math.pow is not guaranteed exact for
        // integer arguments, so square-and-multiply in exact double
        // integer arithmetic, falling back to bignums on overflow.
        if (Math.floor(y) !== y) {
          return Math.pow(x, y);
        }
        var result = 1, base = x, exp = y;
        while (exp > 0) {
          if (exp % 2 === 1) {
            result *= base;
            if (isOverflow(result)) {
              return (makeBignum(x)).expt(makeBignum(y));
            }
            exp -= 1;
          } else {
            base *= base;
            if (isOverflow(base)) {
              return (makeBignum(x)).expt(makeBignum(y));
            }
            exp /= 2;
          }
        }
        return result;
      },
      function(x, y) {
        return x.expt(y);
      },
      {
        isXSpecialCase: function(x) {
          return eqv(x, 0) || eqv(x, 1);
        },
        onXSpecialCase: function(x, y) {
          if (eqv(x, 0)) {
            if (y instanceof Roughnum && equalsAnyZero(y)) {
              // 0 to a rough zero power is discontinuous: the roughnum
              // could represent 0 (giving 1), a positive value (giving
              // 0), or a negative value (division by zero). Unlike other
              // bases, where a rough exponent near 0 is a rounding error
              // away from the exact-0 answer, here the three cases
              // disagree completely, so there is no answer to round to.
              errbacks.throwDomainError("expt: 0 raised to a rough zero power is ambiguous (could be 0, 1, or a division by zero)");
            } else if (equalsAnyZero(y)) {
              return 1;
            } else if (lessThan(y, 0)) {
              errbacks.throwDivByZero("expt: division by zero");
            } else {
              return 0;
            }
          } else { // i.e., x is 1
            return 1;
          }
        },

        isYSpecialCase: function(y) {
          return eqv(y, 0) || lessThan(y, 0);
        },
        onYSpecialCase: function(x, y) {
          if (eqv(y, 0)) {
            return 1;
          } else { // i.e., y is negative
            if (x instanceof Roughnum || y instanceof Roughnum) {
              if (equalsAnyZero(x)) {
                errbacks.throwDivByZero("expt: division by zero");
              }
              var pow = Math.pow(toFixnum(x), toFixnum(y));
              if (isNaN(pow)) {
                errbacks.throwDomainError('expt: taking nonintegral power ' + y +
                                          ' of negative number ' + x);
              }
              return Roughnum.makeInstance(pow);
            }
            return expt(divide(1, x), negate(y));
          }
        }
      });

    // exp: pyretnum -> pyretnum
    var exp = function(n) {
      if ( eqv(n, 0) ) {
        return 1;
      }
      if (typeof(n) === 'number') {
        var res = Math.exp(n);
        if (!isFinite(res))
          errbacks.throwGeneralError('exp: argument too large: ' + n);
        return Roughnum.makeInstance(res);
      }
      return n.exp();
    };

    // modulo: pyretnum pyretnum -> pyretnum
    var modulo = function(m, n) {
      if (! isInteger(m)) {
        errbacks.throwDomainError('modulo: the first argument '
                                  + m + " is not an integer.", m, n);
      }
      if (! isInteger(n)) {
        errbacks.throwDomainError('modulo: the second argument '
                                  + n + " is not an integer.", m, n);
      }
      if (_integerIsZero(n)) {
        errbacks.throwDomainError('modulo: the second argument is zero');
      }
      var result;
      if (typeof(m) === 'number' && typeof(n) === 'number') {
        result = m % n;
        if (n < 0) {
          if (result <= 0)
            return result;
          else
            return result + n;
        } else {
          if (result < 0)
            return result + n;
          else
            return result;
        }
      }
      result = _integerModulo(floor(m), floor(n));
      // The sign of the result should match the sign of n.
      if (lessThan(n, 0)) {
        if (lessThanOrEqual(result, 0)) {
          return result;
        }
        return add(result, n);

      } else {
        if (lessThan(result, 0)) {
          return add(result, n);
        }
        return result;
      }
    };

    // numerator: pyretnum -> pyretnum
    var numerator = function(n) {
      if (typeof(n) === 'number')
        return n;
      return n.numerator();
    };

    // denominator: pyretnum -> pyretnum
    var denominator = function(n) {
      if (typeof(n) === 'number')
        return 1;
      return n.denominator();
    };

    // sqrt: pyretnum -> pyretnum
    var sqrt = function(n) {
      if (lessThan(n, 0)) {
        errbacks.throwSqrtNegative('sqrt: negative argument ' + n);
      }
      if (typeof(n) === 'number') {
        var result = Math.sqrt(n);
        if (Math.floor(result) === result && result * result === n) {
          return result;
        } else {
          return Roughnum.makeInstance(result);
        }
      }
      return n.sqrt();
    };

    // abs: pyretnum -> pyretnum
    var abs = function(n) {
      if (typeof(n) === 'number') {
        return Math.abs(n);
      }
      return n.abs();
    };

    // min :: pyretnum, pyretnum -> pyretnum
    var min = function(n, m) {
      if (lessThan(n, m)) {
        return n;
      }
      return m;
    }
    

    // floor: pyretnum -> pyretnum
    var floor = function(n) {
      if (typeof(n) === 'number')
        return Math.floor(n);
      return n.floor();
    };

    // ceiling: pyretnum -> pyretnum
    var ceiling = function(n) {
      if (typeof(n) === 'number')
        return Math.ceil(n);
      return n.ceiling();
    };

    // round: pyretnum -> pyretnum
    var round = function(n) {
      if (typeof(n) === 'number') {
        return n;
      }
      return n.round();
    };

    var roundEven = function(n) {
      if (typeof(n) === 'number') return n;
      return n.roundEven();
    };

    // NB: all of these trig-gy generic functions should now return roughnum rather than float
    // (except for an arg of 0, etc)

    var ln10 = Math.log(10)

    // log: pyretnum -> pyretnum
    var log = function(n) {
      if ( eqv(n, 1) ) {
        return 0;
      }
      if (lessThanOrEqual(n, 0)) {
        errbacks.throwLogNonPositive('log: non-positive argument ' + n);
      }
      if (typeof(n) === 'number') {
        return Roughnum.makeInstance(Math.log(n));
      }
      if (isRational(n) && !isInteger(n)) {
        var nfix = n.toFixnum();
        if (isFinite(nfix) && nfix !== 0) {
          return Roughnum.makeInstance(Math.log(nfix));
        }
        return subtract(log(numerator(n)),
                        log(denominator(n)));
      }
      var nFix = n.toFixnum();
      if (typeof(nFix) === 'number' && nFix !== Infinity) {
        return Roughnum.makeInstance(Math.log(nFix));
      }
      // at this point, n must be a very large positive number;
      // n > 1e308, i.e, has at least 308 digits;
      // we can safely ignore its fractional part;
      var nStr = n.round().toString();
      var nLen = nStr.length;
      // we furthermore need only the integer part's first few digits
      // although we must remember the number of digits ignored;
      var firstFewLen = 308; // has to be <= 308
      // say integer      N = yyy...yyyxxx...xxx
      // where the number of x's is nx;
      // So              N ~= yyy...yyy * 10^nx
      // We'll first find the common (base 10) log of N
      //          log10(N) ~= log10(yyy...yyy * 10^nx)
      //                    = log10(yyy...yyy) + nx
      // Now to convert this to the natural log
      //              ln(N) = log10(N) / log10(e)
      //                    = log10(N) * ln(10)
      //                   ~= [log10(yyy...yyy) + nx] * ln(10)
      //                    = log10(yyy...yyy) * ln(10) + nx * ln(10)
      //                    = ln(yyy...yyy)             + nx * ln(10)
      // JS gives us ln(yyy...yyy) and ln(10) so we have a good
      // approximation for ln(N)
      var nFirstFew = parseInt(nStr.substring(0, firstFewLen));
      var nLog = Math.log(nFirstFew) + (nLen - firstFewLen) * ln10;
      return Roughnum.makeInstance(nLog);
    };

    // tan: pyretnum -> pyretnum
    var tan = function(n) {
      if (eqv(n, 0)) { return 0; }
      if (typeof(n) === 'number') {
        return Roughnum.makeInstance(Math.tan(n));
      }
      return n.tan();
    };

    // atan: pyretnum -> pyretnum
    var atan = function(n) {
      if (eqv(n, 0)) { return 0; }
      if (typeof(n) === 'number') {
        return Roughnum.makeInstance(Math.atan(n));
      }
      return n.atan();
    };

    // atan2: pyretnum pyretnum -> pyretnum, angle in [0, 2pi)
    var atan2 = function(y, x) {
      if (equalsAnyZero(x) && equalsAnyZero(y)) {
        errbacks.throwDomainError('atan2: out of domain argument (0, 0)');
      }
      if (isRational(y) && equalsAnyZero(y) && isPositive(x)) {
        return 0;
      }
      var fy = toFixnum(y), fx = toFixnum(x);
      if (!isFinite(fy) || !isFinite(fx)) {
        // both exact and at least one beyond the double range: scale by the larger
        var m = greaterThan(abs(y), abs(x)) ? abs(y) : abs(x);
        fy = toFixnum(divide(y, m));
        fx = toFixnum(divide(x, m));
      }
      var res = Math.atan2(fy, fx);
      if (res < 0) { res += 2 * Math.PI; }
      return Roughnum.makeInstance(res);
    };

    // cos: pyretnum -> pyretnum
    var cos = function(n) {
      if (eqv(n, 0)) { return 1; }
      if (typeof(n) === 'number') {
        return Roughnum.makeInstance(Math.cos(n));
      }
      return n.cos();
    };

    // sin: pyretnum -> pyretnum
    var sin = function(n) {
      if (eqv(n, 0)) { return 0; }
      if (typeof(n) === 'number') {
        return Roughnum.makeInstance(Math.sin(n));
      }
      return n.sin();
    };

    // acos: pyretnum -> pyretnum
    var acos = function(n) {
      if (eqv(n, 1)) { return 0; }
      if (lessThan(n, -1) || greaterThan(n, 1)) {
        errbacks.throwDomainError('acos: out of domain argument ' + n);
      }
      if (typeof(n) === 'number') {
        return Roughnum.makeInstance(Math.acos(n));
      }
      return n.acos();
    };

    // asin: pyretnum -> pyretnum
    var asin = function(n) {
      if (eqv(n, 0)) { return 0; }
      if (lessThan(n, -1) || greaterThan(n, 1)) {
        errbacks.throwDomainError('asin: out of domain argument ' + n);
      }
      if (typeof(n) === 'number') {
        return Roughnum.makeInstance(Math.asin(n));
      }
      return n.asin();
    };

    // sqr: pyretnum -> pyretnum
    var sqr = function(x) {
      return multiply(x, x);
    };

    // integerSqrt: pyretnum -> pyretnum
    var integerSqrt = function(x) {
      if (! isInteger(x)) {
        errbacks.throwDomainError('integer-sqrt: the argument ' + x.toString() +
                                  " is not an integer.", x);
      }
      if (typeof (x) === 'number') {
        if(x < 0) {
          errbacks.throwSqrtNegative('integerSqrt of negative number', x);
        } else {
          // Math.sqrt is correctly rounded but floor(sqrt(x)) can still
          // be off by one near perfect squares; adjust exactly.
          var s = Math.floor(Math.sqrt(x));
          while (s * s > x) { s -= 1; }
          while ((s + 1) * (s + 1) <= x) { s += 1; }
          return s;
        }
      }
      return x.integerSqrt();
    };

    // gcd: pyretnum pyretnum -> pyretnum
    var gcd = function(first, second) {
      if (! isInteger(first)) {
        errbacks.throwDomainError('gcd: the argument ' + first.toString() +
                                  " is not an integer.", first);
      }
      if (! isInteger(second)) {
        errbacks.throwDomainError('gcd: the argument ' + second.toString() +
                                  " is not an integer.", second);
      }
      var a = abs(first), t;
      var b = abs(second);
      while (! _integerIsZero(b)) {
        t = a;
        a = b;
        b = _integerModulo(t, b);
      }
      return a;
    };

    // lcm: pyretnum pyretnum -> pyretnum
    var lcm = function(first, second) {
      if (! isInteger(first)) {
        errbacks.throwDomainError('lcm: the argument ' + first.toString() +
                                  " is not an integer.", first);
      }
      if (! isInteger(second)) {
        errbacks.throwDomainError('lcm: the argument ' + second.toString() +
                                  " is not an integer.", second);
      }
      var result = abs(first);
      second = abs(second);
      if (_integerIsZero(result)) { return 0; }
      var divisor = _integerGcd(result, second);
      if (_integerIsZero(divisor)) {
        return 0;
      }
      result = divide(multiply(result, second), divisor);
      return result;
    };

    var quotient = function(x, y) {
      if (! isInteger(x)) {
        errbacks.throwDomainError('quotient: the first argument ' + x.toString() +
                                  " is not an integer.", x);
      }
      if (! isInteger(y)) {
        errbacks.throwDomainError('quotient: the second argument ' + y.toString() +
                                  " is not an integer.", y);
      }
      if (_integerIsZero(y)) {
        errbacks.throwDivByZero('quotient: division by zero, ' + x + ' ' + y);
      }
      return _integerQuotient(x, y);
    };

    var remainder = function(x, y) {
      if (equalsAnyZero(y)) {
        errbacks.throwDivByZero('remainder: division by zero, ' + x + ' ' + y);
      }
      if (isInteger(x) && isInteger(y)) {
        return _integerRemainder(x, y);
      } else if (isRational(x) && isRational(y)) {
        var xn = numerator(x); var xd = denominator(x);
        var yn = numerator(y); var yd = denominator(y);
        var new_d = lcm(xd, yd);
        var new_xn = multiply(xn, divide(new_d, xd));
        var new_yn = multiply(yn, divide(new_d, yd));
        return divide(remainder(new_xn, new_yn), new_d);
      } else {
        var res = toFixnum(x) % toFixnum(y);
        return Roughnum.makeInstance(res);
      }
    };

    //////////////////////////////////////////////////////////////////////

    // Helpers

    // isOverflow: javascript-number -> boolean
    // Returns true if we consider the number an overflow.
    var MIN_FIXNUM = -(9e15);
    var MAX_FIXNUM = (9e15);
    var BIG_MIN_FIXNUM = BigInt(MIN_FIXNUM);
    var BIG_MAX_FIXNUM = BigInt(MAX_FIXNUM);
    var isOverflow = function(n) {
      return !(MIN_FIXNUM <= n && n <= MAX_FIXNUM);
    };

    // negate: pyretnum -> pyretnum
    // multiplies a number times -1.
    var negate = function(n) {
      if (typeof(n) === 'number') {
        return -n;
      }
      return n.negate();
    };

    // halve: pyretnum -> pyretnum
    // Divide a number by 2.
    var halve = function(n) {
      return divide(n, 2);
    };

    // fastExpt: computes n^k by squaring.
    // n^k = (n^2)^(k/2)
    // Assumes k is non-negative integer.
    var fastExpt = function(n, k) {
      var acc = 1;
      while (true) {
        if (_integerIsZero(k)) {
          return acc;
        }
        if (equals(modulo(k, 2), 0)) {
          n = multiply(n, n);
          k = divide(k, 2);
        } else {
          acc = multiply(acc, n);
          k = subtract(k, 1);
        }
      }
    };

    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

    // Integer operations
    // Integers are either represented as fixnums or as BigIntegers.

    // makeIntegerBinop: (fixnum fixnum -> X) (BigInteger BigInteger -> X) -> X
    // Helper to collect the common logic for coercing integer fixnums or bignums to a
    // common type before doing an operation.
    var makeIntegerBinop = function(onFixnums, onBignums, options) {
      options = options || {};
      return (function(m, n) {
        if (m instanceof Rational) {
          m = numerator(m);
        }

        if (n instanceof Rational) {
          n = numerator(n);
        }

        if (typeof(m) === 'number' && typeof(n) === 'number') {
          var result = onFixnums(m, n);
          if (! isOverflow(result) ||
              (options.ignoreOverflow)) {
            return result;
          }
        }
        if (m instanceof Roughnum || n instanceof Roughnum) {
          return Roughnum.makeInstance(
            onFixnums(toFixnum(m), toFixnum(n)));
        }
        if (typeof(m) === 'number') {
          m = makeBignum(m);
        }
        if (typeof(n) === 'number') {
          n = makeBignum(n);
        }
        var result = onBignums(m, n);
        if (result instanceof BigInteger) { return normalizeInteger(result); }
        return result;
      });
    };

    var makeIntegerUnOp = function(onFixnums, onBignums, options) {
      options = options || {};
      return (function(m) {
        if (m instanceof Rational) {
          m = numerator(m);
        }

        if (typeof(m) === 'number') {
          var result = onFixnums(m);
          if (! isOverflow(result) ||
              (options.ignoreOverflow)) {
            return result;
          }
        }
        if (m instanceof Roughnum) {
          return Roughnum.makeInstance(onFixnums(toFixnum(m)));
        }
        if (typeof(m) === 'number') {
          m = makeBignum(m);
        }
        var result = onBignums(m);
        if (result instanceof BigInteger) { return normalizeInteger(result); }
        return result;
      });
    };

    // _integerModulo: integer-pyretnum integer-pyretnum -> integer-pyretnum
    var _integerModulo = makeIntegerBinop(
      function(m, n) {
        return m % n;
      },
      function(m, n) {
        return bnMod.call(m, n);
      });

    // _integerGcd: integer-pyretnum integer-pyretnum -> integer-pyretnum
    var _integerGcd = makeIntegerBinop(
      function(a, b) {
        var t;
        while (b !== 0) {
          t = a;
          a = b;
          b = t % b;
        }
        return a;
      },
      function(m, n) {
        return bnGCD.call(m, n);
      });

    // _integerIsZero: integer-pyretnum -> boolean
    // Returns true if the number is zero.
    var _integerIsZero = makeIntegerUnOp(
      function(n){
        return n === 0;
      },
      function(n) {
        return bnEquals.call(n, BigInteger.ZERO);
      }
    );

    // _integerIsOne: integer-pyretnum -> boolean
    var _integerIsOne = makeIntegerUnOp(
      function(n) {
        return n === 1;
      },
      function(n) {
        return bnEquals.call(n, BigInteger.ONE);
      });

    // _integerIsNegativeOne: integer-pyretnum -> boolean
    var _integerIsNegativeOne = makeIntegerUnOp(
      function(n) {
        return n === -1;
      },
      function(n) {
        return bnEquals.call(n, BigInteger.NEGATIVE_ONE);
      });

    // _integerAdd: integer-pyretnum integer-pyretnum -> integer-pyretnum
    var _integerAdd = makeIntegerBinop(
      function(m, n) {
        return m + n;
      },
      function(m, n) {
        return bnAdd.call(m, n);
      });

    // _integerSubtract: integer-pyretnum integer-pyretnum -> integer-pyretnum
    var _integerSubtract = makeIntegerBinop(
      function(m, n) {
        return m - n;
      },
      function(m, n) {
        return bnSubtract.call(m, n);
      });

    // _integerMultiply: integer-pyretnum integer-pyretnum -> integer-pyretnum
    var _integerMultiply = makeIntegerBinop(
      function(m, n) {
        return m * n;
      },
      function(m, n) {
        return bnMultiply.call(m, n);
      });

    //_integerQuotient: integer-pyretnum integer-pyretnum -> integer-pyretnum
    var _integerQuotient = makeIntegerBinop(
      function(m, n) {
        return ((m - (m % n))/ n);
      },
      function(m, n) {
        return bnDivide.call(m, n);
      });

    var _integerRemainder = makeIntegerBinop(
      function(m, n) {
        return m % n;
      },
      function(m, n) {
        return bnRemainder.call(m, n);
      });

    // splitIntIntoMantissaExpt: integer-pyretnum -> [JS-double, JS-int]
    // 
    // splitIntIntoMantissaExpt takes an integer s (either unboxed or BigInteger)
    //   and returns [mantissa, exponent]
    //   such that s ~= mantissa * 10^exponent.
    // mantissa is a JS-double, and is chosen to have one non-zero digit
    //   to the left of the decimal point.
    // Because mantissa is a JS-double, there is in general a loss of precision.
    // splitIntIntoMantissaExpt is used to create a best-possible JS-double version
    //   of its argument arbitrarily precise integer.
    // E.g., splitIntIntoMantissaExpt(256) returns
    //   [2.56, 2]
    // splitIntIntoMantissaExpt(111222333444555666777888999) returns
    //   [1.1122233344455567, 26]
    //
    var splitIntIntoMantissaExpt = function(s) {
      var str = s.toString();
      var c0 = str[0];
      var sign = '';

      if (c0 === '-' || c0 === '+') {
        str = str.substring(1);
        if (c0 === '-') {
          sign = '-';
        }
      }

      var d0 = str[0];
      str = str.substring(1);

      var mantissa = Number(sign + d0 + '.' + str);
      var expt = str.length;

      return [mantissa, expt];
    };

    // bigIntegerQuotientToDouble: BigInteger BigInteger -> JS-double
    //
    // Correctly-rounded (round-half-even, IEEE-754 double) value of n/d,
    // for nonzero d. Computes floor((n << s) / d) with enough quotient
    // bits, then rounds to the double's significand width using the
    // division remainder as the sticky bit, handling subnormals and
    // overflow to +/-Infinity.
    var bigIntegerQuotientToDouble = function(n, d) {
      var negative = false;
      if (n.signum() < 0) { n = n.negate(); negative = !negative; }
      if (d.signum() < 0) { d = d.negate(); negative = !negative; }
      if (d.signum() === 0) {
        errbacks.throwDivByZero('/: division by zero, ' + n + ' ' + d);
      }
      if (n.signum() === 0) { return negative ? -0 : 0; }

      var shift = 55 - (n.bitLength() - d.bitLength());
      var num = n, den = d;
      if (shift > 0) { num = n.shiftLeft(shift); }
      else if (shift < 0) { den = d.shiftLeft(-shift); }
      var qr = num.divideAndRemainder(den);
      return roundScaledToDouble(qr[0], shift, qr[1].signum() !== 0, negative);
    };

    // roundScaledToDouble: BigInteger int boolean boolean -> JS-double
    //
    // Correctly rounded (half even) double of q * 2^(-shift) (+ a positive
    // amount below one unit of q when sticky), q having at least 55 bits.
    var roundScaledToDouble = function(q, shift, sticky, negative) {
      var e2 = q.bitLength() - 1 - shift;

      if (e2 > 1023) { return negative ? -Infinity : Infinity; }
      if (e2 < -1075) { return negative ? -0 : 0; }
      // significand bits available at this exponent (53 for normals,
      // fewer for subnormals; e2 === -1075 keeps 0 bits and can round
      // up to the minimum subnormal)
      var keep = Math.min(53, 1075 + e2);
      var drop = q.bitLength() - keep;
      var q2 = q.shiftRight(drop);
      var rem = q.subtract(q2.shiftLeft(drop));
      var half = BigInteger.ONE.shiftLeft(drop - 1);
      var cmp = rem.compareTo(half);
      if (cmp > 0 || (cmp === 0 && (sticky || q2.testBit(0)))) {
        // a carry to 2^keep needs no renormalization: it still scales
        // exactly below, yielding 2^(e2+1) (or Infinity on overflow)
        q2 = q2.add(BigInteger.ONE);
      }
      if (q2.signum() === 0) { return negative ? -0 : 0; }
      // q2 <= 2^53, so this Number conversion is exact
      var res = Number(q2.toString());
      // exact scaling by 2^(e2 - keep + 1): every intermediate double in
      // these loops is exactly representable (see report)
      var s = e2 - keep + 1;
      while (s > 0) { res *= 2; s -= 1; }
      while (s < 0) { res *= 0.5; s += 1; }
      return negative ? -res : res;
    };

    // bigIntegerSqrtToDouble: BigInteger -> JS-double
    //
    // Correctly rounded double of the square root of a nonnegative
    // BigInteger, for radicands that do not fit a double themselves.
    var bigIntegerSqrtToDouble = function(n) {
      var k = Math.max(0, 56 - (n.bitLength() >> 1));
      var m = n.shiftLeft(2 * k);
      var s = m.integerSqrt();
      if (typeof(s) === 'number') { s = makeBignum(s); }
      var sticky = !s.multiply(s).equals(m);
      return roundScaledToDouble(s, k, sticky, false);
    };

    // _integerDivideToFixnum: integer-pyretnum integer-pyretnum -> fixnum
    //
    // _integerDivideToFixnum takes two integers (possibly BigIntegers) and
    //   returns the correctly-rounded JS-double of their quotient.
    var _integerDivideToFixnum = makeIntegerBinop(
      function(m, n) {
        return m / n;
      },
      function(m, n) {
        return bigIntegerQuotientToDouble(m, n);
      },
      { ignoreOverflow: false,
        doNotCoerceToFloating: true
      }
    );

    // _integerEquals: integer-pyretnum integer-pyretnum -> boolean
    var _integerEquals = makeIntegerBinop(
      function(m, n) {
        return m === n;
      },
      function(m, n) {
        return bnEquals.call(m, n);
      },
      {doNotCoerceToFloating: true});

    // _integerGreaterThan: integer-pyretnum integer-pyretnum -> boolean
    var _integerGreaterThan = makeIntegerBinop(
      function(m, n) {
        return m > n;
      },
      function(m, n) {
        return bnCompareTo.call(m, n) > 0;
      },
      {doNotCoerceToFloating: true});

    // _integerLessThan: integer-pyretnum integer-pyretnum -> boolean
    var _integerLessThan = makeIntegerBinop(
      function(m, n) {
        return m < n;
      },
      function(m, n) {
        return bnCompareTo.call(m, n) < 0;
      },
      {doNotCoerceToFloating: true});

    // _integerGreaterThanOrEqual: integer-pyretnum integer-pyretnum -> boolean
    var _integerGreaterThanOrEqual = makeIntegerBinop(
      function(m, n) {
        return m >= n;
      },
      function(m, n) {
        return bnCompareTo.call(m, n) >= 0;
      },
      {doNotCoerceToFloating: true});

    // _integerLessThanOrEqual: integer-pyretnum integer-pyretnum -> boolean
    var _integerLessThanOrEqual = makeIntegerBinop(
      function(m, n) {
        return m <= n;
      },
      function(m, n) {
        return bnCompareTo.call(m, n) <= 0;
      },
      {doNotCoerceToFloating: true});

    //////////////////////////////////////////////////////////////////////
    // The boxed number types are expected to implement the following
    // interface.
    //
    // toString: -> string

    // isFinite: -> boolean

    // isInteger: -> boolean
    // Produce true if this number can be coerced into an integer.

    // isRational: -> boolean
    // Produce true if the number is rational.

    // isExact === isRational

    // isReal: -> boolean
    // Produce true if the number is real.

    // toRational: -> pyretnum
    // Produce an exact number.

    // toExact === toRational

    // toRoughnum: -> pyretnum
    // Produce a roughnum.

    // toFixnum: -> fixnum
    // Produce a javascript number.

    // greaterThan: pyretnum -> boolean
    // Compare against instance of the same type.

    // greaterThanOrEqual: pyretnum -> boolean
    // Compare against instance of the same type.

    // lessThan: pyretnum -> boolean
    // Compare against instance of the same type.

    // lessThanOrEqual: pyretnum -> boolean
    // Compare against instance of the same type.

    // add: pyretnum -> pyretnum
    // Add with an instance of the same type.

    // subtract: pyretnum -> pyretnum
    // Subtract with an instance of the same type.

    // multiply: pyretnum -> pyretnum
    // Multiply with an instance of the same type.

    // divide: pyretnum -> pyretnum
    // Divide with an instance of the same type.

    // numerator: -> pyretnum
    // Return the numerator.

    // denominator: -> pyretnum
    // Return the denominator.

    // integerSqrt: -> pyretnum
    // Produce the integer square root.

    // sqrt: -> pyretnum
    // Produce the square root.

    // abs: -> pyretnum
    // Produce the absolute value.

    // floor: -> pyretnum
    // Produce the floor.

    // ceiling: -> pyretnum
    // Produce the ceiling.

    // log: -> pyretnum
    // Produce the log.

    // atan: -> pyretnum
    // Produce the arc tangent.

    // cos: -> pyretnum
    // Produce the cosine.

    // sin: -> pyretnum
    // Produce the sine.

    // expt: pyretnum -> pyretnum
    // Produce the power to the input.

    // exp: -> pyretnum
    // Produce e raised to the given power.

    // acos: -> pyretnum
    // Produce the arc cosine.

    // asin: -> pyretnum
    // Produce the arc sine.

    // round: -> pyretnum
    // Round to the nearest integer.

    // equals: pyretnum -> boolean
    // Produce true if the given number of the same type is equal.

    //////////////////////////////////////////////////////////////////////

    // Rationals

    var Rational = function(n, d) {
      this.n = n;
      this.d = d;
    };

    Rational.makeInstance = function(n, d) {
      if (n === undefined)
        errbacks.throwUndefinedValue("n undefined", n, d);

      if (d === undefined) { d = 1; }

      if (!isInteger(n) || !isInteger(d)) {
        errbacks.throwDomainError("makeRational: " + n + ' ' + d + " are not both integers");
      }

      if (_integerIsZero(d)) {
        errbacks.throwDivByZero("/: division by zero, " + n + ' ' + d);
      }

      if (_integerLessThan(d, 0)) {
        n = negate(n);
        d = negate(d);
      }

      var divisor = _integerGcd(abs(n), abs(d));
      n = normalizeInteger(_integerQuotient(n, divisor));
      d = normalizeInteger(_integerQuotient(d, divisor));

      // Optimization: if we can get around construction the rational
      // in favor of just returning n, do it:
      if (_integerIsOne(d) || _integerIsZero(n)) {
        return n;
      }

      return new Rational(n, d);
    };

    Rational.prototype.toString = function() {
      if (_integerIsOne(this.d)) {
        return this.n.toString() + "";
      } else {
        return this.n.toString() + "/" + this.d.toString();
      }
    };

    Rational.prototype.isFinite = function() {
      return true;
    };

    Rational.prototype.equals = function(other) {
      return (other instanceof Rational &&
              _integerEquals(this.n, other.n) &&
              _integerEquals(this.d, other.d));
    };

    Rational.prototype.isInteger = function() {
      return _integerIsOne(this.d);
    };

    Rational.prototype.isRational = function() {
      return true;
    };

    Rational.prototype.isExact = Rational.prototype.isRational;

    Rational.prototype.isReal = function() {
      return true;
    };

    Rational.prototype.isRoughnum = function() {
      return false;
    };

    Rational.prototype.isPositive = function() {
      // don't care about this.d
      return this.n > 0;
    };

    Rational.prototype.isNonNegative = function() {
      return this.n >= 0;
    };

    Rational.prototype.isNegative = function() {
      return this.n < 0;
    };

    Rational.prototype.isNonPositive = function() {
      return this.n <= 0;
    };

    Rational.prototype.add = function(other) {
      return Rational.makeInstance(_integerAdd(_integerMultiply(this.n, other.d),
                                               _integerMultiply(this.d, other.n)),
                                   _integerMultiply(this.d, other.d));
    };

    Rational.prototype.subtract = function(other) {
      return Rational.makeInstance(_integerSubtract(_integerMultiply(this.n, other.d),
                                                    _integerMultiply(this.d, other.n)),
                                   _integerMultiply(this.d, other.d));
    };

    Rational.prototype.negate = function() {
      return Rational.makeInstance(negate(this.n), this.d)
    };

    Rational.prototype.multiply = function(other) {
      return Rational.makeInstance(_integerMultiply(this.n, other.n),
                                   _integerMultiply(this.d, other.d));
    };

    Rational.prototype.divide = function(other) {
      if (_integerIsZero(this.d) || _integerIsZero(other.n)) {  // dead code!
        errbacks.throwDivByZero("/: division by zero", this, other);
      }
      return Rational.makeInstance(_integerMultiply(this.n, other.d),
                                   _integerMultiply(this.d, other.n));
    };

    Rational.prototype.toRational = function() {
      return this;
    };

    Rational.prototype.toExact = Rational.prototype.toRational;


    Rational.prototype.toFixnum = function() {
      return _integerDivideToFixnum(this.n, this.d);
    };

    Rational.prototype.toRoughnum = function() {
      return Roughnum.makeInstance(this.toFixnum());
    };

    Rational.prototype.numerator = function() {
      return this.n;
    };

    Rational.prototype.denominator = function() {
      return this.d;
    };

    Rational.prototype.greaterThan = function(other) {
      return _integerGreaterThan(_integerMultiply(this.n, other.d),
                                 _integerMultiply(this.d, other.n));
    };

    Rational.prototype.greaterThanOrEqual = function(other) {
      return _integerGreaterThanOrEqual(_integerMultiply(this.n, other.d),
                                        _integerMultiply(this.d, other.n));
    };

    Rational.prototype.lessThan = function(other) {
      return _integerLessThan(_integerMultiply(this.n, other.d),
                              _integerMultiply(this.d, other.n));
    };

    Rational.prototype.lessThanOrEqual = function(other) {
      return _integerLessThanOrEqual(_integerMultiply(this.n, other.d),
                                     _integerMultiply(this.d, other.n));
    };

    Rational.prototype.integerSqrt = function() {
      var result = sqrt(this);
      return toRational(floor(result));
    };

    Rational.prototype.sqrt = function() {
      // callers ensure this >= 0, so this.n >= 0 and this.d > 0
      var rootN = integerSqrt(this.n);
      var rootD = integerSqrt(this.d);
      if (equals(multiply(rootN, rootN), this.n) &&
          equals(multiply(rootD, rootD), this.d)) {
        return Rational.makeInstance(rootN, rootD);
      }
      var fix = this.toFixnum();
      if (fix > 0 && isFinite(fix)) {
        return Roughnum.makeInstance(Math.sqrt(fix));
      }
      // magnitudes beyond double range: approximate componentwise
      return divide(sqrt(this.n), sqrt(this.d));
    };

    Rational.prototype.abs = function() {
      return Rational.makeInstance(abs(this.n),
                                   this.d);
    };

    Rational.prototype.floor = function() {
      var quotient = _integerQuotient(this.n, this.d);
      if (_integerLessThan(this.n, 0)) {
        return subtract(quotient, 1);
      } else {
        return quotient;
      }
    };

    Rational.prototype.ceiling = function() {
      var quotient = _integerQuotient(this.n, this.d);
      if (_integerLessThan(this.n, 0)) {
        return quotient;
      } else {
        return add(quotient, 1);
      }
    };

    Rational.prototype.round = function() {
      var halfintp = equals(this.d, 2);
      var negativep = _integerLessThan(this.n, 0);
      var n = this.n;
      if (negativep) {
        n = negate(n);
      }
      var quo = _integerQuotient(n, this.d);
      if (halfintp) {
        // rounding half to away from 0
        // uncomment following if rounding half to even
        // if (_integerIsOne(_integerModulo(quo, 2)))
        quo = add(quo, 1);
      } else {
        var rem = _integerRemainder(n, this.d);
        if (greaterThan(multiply(rem, 2), this.d)) {
          quo = add(quo, 1);
        }
      }
      if (negativep) {
        quo = negate(quo);
      }
      return quo;
    };

    Rational.prototype.roundEven = function() {
      // rounds half-integers to even
      var halfintp = equals(this.d, 2);
      var negativep = _integerLessThan(this.n, 0);
      var n = this.n;
      if (negativep) n = negate(n);
      var quo = _integerQuotient(n, this.d);
      if (halfintp) {
        if (_integerIsOne(_integerModulo(quo, 2)))
          quo = add(quo, 1);
      } else {
        var rem = _integerRemainder(n, this.d);
        if (greaterThan(multiply(rem, 2), this.d))
          quo = add(quo, 1);
      }
      if (negativep) quo = negate(quo);
      return quo;
    };

    Rational.prototype.log = function(){
      return log(this)
    };

    Rational.prototype.tan = function(){
      return Roughnum.makeInstance(Math.tan(this.toFixnum()));
    };

    Rational.prototype.atan = function(){
      return Roughnum.makeInstance(Math.atan(this.toFixnum()));
    };

    Rational.prototype.cos = function(){
      return Roughnum.makeInstance(Math.cos(this.toFixnum()));
    };

    Rational.prototype.sin = function(){
      return Roughnum.makeInstance(Math.sin(this.toFixnum()));
    };

    var integerNthRoot = function(n, m) {
      if (sign(n) < 0)
        errbacks.throwDomainError('integerNthRoot: root ' + n + ' is negative.');
      if (sign(m) < 0)
        errbacks.throwDomainError('integerNthRoot: radicand ' + m + ' is negative.');
      var guessPrev, guessToTheN;
      var guess = (m instanceof BigInteger)
        ? BigInteger.ONE.shiftLeft(Math.ceil(m.bitLength() / toFixnum(n)))
        : floor(m);

      // find closest integral zero of x^n - m = 0 using Newton-Raphson.
      // if k'th guess is x_k, then
      // x_{k+1} = floor( x_k - [(x_k)^n - m]/[n (x_k)^(n-1)] ).
      // Stop iteration if (x_k)^n is close enough to m, or
      // if x_k stops evolving

      while(true) {
        guessToTheN = expt(guess, n);
        if (lessThanOrEqual(guessToTheN, m) &&
            lessThan(m, expt(add(guess, 1), n))) break;
        guessPrev = guess;
        guess = floor(subtract(guess, divide(subtract(guessToTheN, m),
                                             multiply(n, divide(guessToTheN, guess)))));
        if (equals(guess, guessPrev)) break;
      }

      return guess;
    };

    var nthRoot = function(n, m) {
      var mNeg = (sign(m) < 0);
      var mAbs = (mNeg ? abs(m) : m);
      var approx;

      if (mNeg && _integerModulo(n, 2) === 0)
        errbacks.throwDomainError('expt: taking even (' + n + ') root of negative integer ' + m);

      approx = integerNthRoot(n, mAbs);
      if (mNeg) approx = negate(approx);
      if (eqv(expt(approx, n), m)) return approx;

      approx = Roughnum.makeInstance(Math.pow(toFixnum(mAbs),
                                              toFixnum(divide(1,n))));
      return (mNeg ? negate(approx) : approx);
    };

    Rational.prototype.expt = function(a) {
      if (isInteger(a) && greaterThanOrEqual(a, 0)) {
        if (greaterThan(a, 0xffffffff)) {
          errbacks.throwDomainError('expt: exponent ' + a + ' too large');
        }
        return fastExpt(this, a);
      } else if (_integerLessThanOrEqual(a.d, 8)) {
        var nRaisedToAn = expt(this.n, a.n);
        var dRaisedToAn = expt(this.d, a.n);
        var newN = nthRoot(a.d, nRaisedToAn);
        var newD = nthRoot(a.d, dRaisedToAn);
        if (isRational(newN) && isRational(newD) &&
            equals(floor(newN), newN) &&
            equals(floor(newD), newD)) {
          return Rational.makeInstance(newN, newD);
        } else {
          return divide(newN, newD);
        }
      } else {
        if (this.isNegative() && !a.isInteger())
          errbacks.throwDomainError('expt: raising negative number ' + this + ' to nonintegral power ' + a);
        return Roughnum.makeInstance(Math.pow(this.toFixnum(), a.toFixnum()));
      }
    };

    Rational.prototype.exp = function(){
      var res = Math.exp(this.toFixnum());
      if (!isFinite(res))
        errbacks.throwDomainError('exp: argument too large: ' + this);
      return Roughnum.makeInstance(res);
    };

    Rational.prototype.acos = function(){
      return Roughnum.makeInstance(Math.acos(this.toFixnum()));
    };

    Rational.prototype.asin = function(){
      return Roughnum.makeInstance(Math.asin(this.toFixnum()));
    };

    // sign: Number -> {-1, 0, 1}
    var sign = function(n) {
      if (lessThan(n, 0)) {
        return -1;
      } else if (greaterThan(n, 0)) {
        return 1;
      } else {
        return 0;
      }
    };

    // Roughnums

    var Roughnum = function(n) {
      if (!(typeof(n) === 'number'))
        errbacks.throwGeneralError('roughnum constructor got unsuitable arg ' + n);
      this.n = n;
    };

    Roughnum.makeInstance = function(n) {
      if (typeof(n) === 'number' && !isFinite(n)) {
        errbacks.throwDomainError('roughnum overflow error');
      }
      return new Roughnum(n);
    };

    Roughnum.prototype.isFinite = function() {
      //actually always true, as we don't store overflows
      return (isFinite(this.n));
    };

    Roughnum.prototype.toRational = function() {
      if (!isFinite(this.n)) {
        // this _should_ be dead, as we don't store overflows
        errbacks.throwInternalError("toRational: no exact representation for " + this);
      }

      return fromString(this.n.toString());
    };

    Roughnum.prototype.toExact = Roughnum.prototype.toRational;

    Roughnum.prototype.toString = function() {
      return '~' + this.n.toString();
    };

    Roughnum.prototype.equals = function(other) {
      errbacks.throwIncomparableValues("roughnums cannot be compared for equality");
    };

    Roughnum.prototype.isRational = function() {
      return false;
    };

    Roughnum.prototype.isExact = Roughnum.prototype.isRational;

    Roughnum.prototype.isInteger = function() {
      return false;
    };

    Roughnum.prototype.isReal = function() {
      return true;
    };

    Roughnum.prototype.isRoughnum = function() {
      return true;
    };

    Roughnum.prototype.isPositive = function() {
      return this.n > 0;
    };

    Roughnum.prototype.isNonNegative = function() {
      return this.n >= 0;
    };

    Roughnum.prototype.isNegative = function() {
      return this.n < 0;
    };

    Roughnum.prototype.isNonPositive = function() {
      return this.n <= 0;
    };

    Roughnum.prototype.add = function(other) {
      return Roughnum.makeInstance(this.n + other.n);
    };

    Roughnum.prototype.subtract = function(other) {
      return Roughnum.makeInstance(this.n - other.n);
    };

    Roughnum.prototype.negate = function() {
      return Roughnum.makeInstance(-this.n);
    };

    Roughnum.prototype.multiply = function(other) {
      return Roughnum.makeInstance(this.n * other.n);
    };

    Roughnum.prototype.divide = function(other) {
      return Roughnum.makeInstance(this.n / other.n);
    };

    Roughnum.prototype.toFixnum = function() {
      return this.n;
    };

    Roughnum.prototype.toRoughnum = function() {
      return this;
    };

    Roughnum.prototype.numerator = function() {
      return toRoughnum(numerator(this.toRational()));
    };

    Roughnum.prototype.denominator = function() {
      return toRoughnum(denominator(this.toRational()));
    };

    Roughnum.prototype.floor = function() {
      var res = Math.floor(this.n);
      if (isOverflow(res)) { return floor(this.toRational()); }
      return res;
    };

    Roughnum.prototype.ceiling = function() {
      var res = Math.ceil(this.n);
      if (isOverflow(res)) { return ceiling(this.toRational()); }
      return res;
    };

    Roughnum.prototype.round = function(){
      var negativep = (this.n < 0);
      var n = this.n;
      if (negativep) n = -n;
      var res = Math.round(n);
      if (negativep) res = -res;
      if (isOverflow(res)) { return round(this.toRational()); }
      return res;
    };

    Roughnum.prototype.roundEven = function() {
      var negativep = (this.n < 0);
      var n = this.n;
      if (negativep) n = -n;
      var res = Math.round(n);
      if ((Math.abs(n - res) === 0.5) && (res % 2 === 1))
        res -= 1;
      if (negativep) res = -res;
      if (isOverflow(res)) { return roundEven(this.toRational()); }
      return res;
    };

    Roughnum.prototype.greaterThan = function(other) {
      return this.n > other.n;
    };

    Roughnum.prototype.greaterThanOrEqual = function(other) {
      return this.n >= other.n;
    };

    Roughnum.prototype.lessThan = function(other) {
      return this.n < other.n;
    };

    Roughnum.prototype.lessThanOrEqual = function(other) {
      return this.n <= other.n;
    };

    Roughnum.prototype.integerSqrt = function() {
      if (isInteger(this)) {
        if(this.n >= 0) {
          return Roughnum.makeInstance(Math.floor(Math.sqrt(this.n)));
        } else {
          errbacks.throwDomainError('integerSqrt of negative roughnum', this.n);
        }
      } else {
        errbacks.throwDomainError("integerSqrt: can only be applied to an integer", this);
      }
    };

    Roughnum.prototype.sqrt = function() {
      return Roughnum.makeInstance(Math.sqrt(this.n));
    };

    Roughnum.prototype.abs = function() {
      return Roughnum.makeInstance(Math.abs(this.n));
    };

    Roughnum.prototype.log = function(){
      if (this.n < 0)
        errbacks.throwDomainError('log of negative roughnum', this.n);
      else
        return Roughnum.makeInstance(Math.log(this.n));
    };

    Roughnum.prototype.tan = function(){
      return Roughnum.makeInstance(Math.tan(this.n));
    };

    Roughnum.prototype.atan = function(){
      return Roughnum.makeInstance(Math.atan(this.n));
    };

    Roughnum.prototype.cos = function(){
      return Roughnum.makeInstance(Math.cos(this.n));
    };

    Roughnum.prototype.sin = function(){
      return Roughnum.makeInstance(Math.sin(this.n));
    };

    Roughnum.prototype.expt = function(a){
      if (this.n === 1) {
        return this;
      } else {
        var res = Math.pow(this.n, a.n);
        if (isNaN(res)) {
          errbacks.throwDomainError('expt: taking nonintegral power ' + a +
                                    ' of negative number ' + this);
        }
        return Roughnum.makeInstance(res);
      }
    };

    Roughnum.prototype.exp = function(){
      var res = Math.exp(this.n);
      if (!isFinite(res))
        errbacks.throwDomainError('exp: argument too large: ' + this);
      return Roughnum.makeInstance(res);
    };

    Roughnum.prototype.acos = function(){
      return Roughnum.makeInstance(Math.acos(this.n));
    };

    Roughnum.prototype.asin = function(){
      return Roughnum.makeInstance(Math.asin(this.n));
    };

    var rationalRegexp = new RegExp("^([+-]?\\d+)/(\\d+)$");
    var digitRegexp = new RegExp("^[+-]?\\d+$");
    var flonumRegexp = new RegExp("^([-+]?)(\\d+\)((?:\\.\\d+)?)((?:[Ee][-+]?\\d+)?)$");


    var roughnumDecRegexp = new RegExp("^~([-+]?\\d+(?:\\.\\d+)?(?:[Ee][-+]?\\d+)?)$");

    var roughnumRatRegexp = new RegExp("^~([+-]?\\d+)/(\\d+)$");

    var scientificPattern = new RegExp("^([+-]?\\d*\\.?\\d*)[Ee]([+]?\\d+)$");

  // fromString: string -> (pyretnum | false)
    var fromString = function(x) {
      if (x.match(digitRegexp)) {
        var n = Number(x);
        if (isOverflow(n)) {
          return makeBignum(x);
        } else {
          return n;
        }
      }

      var aMatch = x.match(rationalRegexp);
      if (aMatch) {
        var d = fromString(aMatch[2]);
        if (_integerIsZero(d)) { return false; }
        return Rational.makeInstance(fromString(aMatch[1]), d);
      }

      aMatch = x.match(flonumRegexp);
      if (aMatch) {
        var negativeP = (aMatch[1] === "-");
        //
        var beforeDecimalString = aMatch[2];
        var beforeDecimal = 0;
        if (beforeDecimalString !== '') {
          beforeDecimal = makeBignum(beforeDecimalString);
        }
        //
        var afterDecimalString = aMatch[3];
        var denominatorTen = 1;
        var afterDecimal = 0;
        if (afterDecimalString !== '') {
          afterDecimalString = afterDecimalString.substring(1);
          denominatorTen = makeBignum('1' + new Array(afterDecimalString.length + 1).join('0'));
          if (afterDecimalString !== '') {
            afterDecimal = makeBignum(afterDecimalString);
          }
        }
        //
        var exponentString = aMatch[4];
        var exponentNegativeP = false;
        var exponent = 1;
        if (exponentString !== '') {
          exponentString = exponentString.substring(1);
          var exponentSign = exponentString.charAt(0);
          exponentNegativeP = (exponentSign === '-');
          if (exponentSign === '-' || exponentSign === '+') {
            exponentString = exponentString.substring(1);
          }
          exponent = makeBignum('1' + new Array(Number(exponentString) + 1).join('0'));
        }

        var finalDen = denominatorTen;
        var finalNum = _integerAdd(_integerMultiply(beforeDecimal, denominatorTen),
                                   afterDecimal);
        if (negativeP) {
          finalNum = negate(finalNum);
        }
        //
        if (!equals(exponent, 1)) {
          if (exponentNegativeP) {
            finalDen = _integerMultiply(finalDen, exponent);
          } else {
            finalNum = _integerMultiply(finalNum, exponent);
          }
        }
        return Rational.makeInstance(finalNum, finalDen);
      }

      aMatch = x.match(roughnumRatRegexp);
      if (aMatch) {
        var rd = fromString(aMatch[2]);
        if (_integerIsZero(rd)) { return false; }
        return toRoughnum(Rational.makeInstance(fromString(aMatch[1]), rd));
      }

      aMatch = x.match(roughnumDecRegexp);
      if (aMatch) {
        return Roughnum.makeInstance(Number(aMatch[1]));
      }

      return false; // if all else fails

    };

    ///////////////////////////////////////////////////////////

    // recognizing numbers in (We)Scheme syntax:

    var hashModifiersRegexp = new RegExp("^(#[ei]#[bodx]|#[bodx]#[ei]|#[bodxei])(.*)$")

    function schemeRationalRegexp(digits) { return new RegExp("^([+-]?["+digits+"]+)/(["+digits+"]+)$"); }

    function matchComplexRegexp(radix, x) {
      var sign = "[+-]";
      var maybeSign = "[+-]?";
      var digits = digitsForRadix(radix)
      var expmark = "["+expMarkForRadix(radix)+"]"
      var digitSequence = "["+digits+"]+"

      var unsignedRational = digitSequence+"/"+digitSequence
      var rational = maybeSign + unsignedRational

      var noDecimal = digitSequence
      var decimalNumOnRight = "["+digits+"]*\\.["+digits+"]+"
      var decimalNumOnLeft = "["+digits+"]+\\.["+digits+"]*"

      var unsignedDecimal = "(?:" + noDecimal + "|" + decimalNumOnRight + "|" + decimalNumOnLeft + ")"

      var special = "(?:inf\.0|nan\.0|inf\.f|nan\.f)"

      var unsignedRealNoExp = "(?:" + unsignedDecimal + "|" + unsignedRational + ")"
      var unsignedReal = unsignedRealNoExp + "(?:" + expmark + maybeSign + digitSequence + ")?"
      var unsignedRealOrSpecial = "(?:" + unsignedReal + "|" + special + ")"
      var real = "(?:" + maybeSign + unsignedReal + "|" + sign + special + ")"

      var alt1 = new RegExp("^(" + rational + ")"
                            + "(" + sign + unsignedRational + "?)"
                            + "i$");
      var alt2 = new RegExp("^(" + real + ")?"
                            + "(" + sign + unsignedRealOrSpecial + "?)"
                            + "i$");
      var alt3 = new RegExp("^(" + real + ")@(" + real + ")$");

      var match1 = x.match(alt1)
      var match2 = x.match(alt2)
      var match3 = x.match(alt3)

      return match1 ? match1 :
	match2 ? match2 :
	match3 ? match3 :
	/* else */ false
    }

    function schemeDigitRegexp(digits) { return new RegExp("^[+-]?["+digits+"]+$"); }
    /**
       /* NB: !!!! flonum regexp only matches "X.", ".X", or "X.X", NOT "X", this
       /* must be separately checked with schemeDigitRegexp.
       /* I know this seems dumb, but the alternative would be that this regexp
       /* returns six matches, which also seems dumb.
       /***/
    function schemeFlonumRegexp(digits) {
      var decimalNumOnRight = "(["+digits+"]*)\\.(["+digits+"]+)"
      var decimalNumOnLeft = "(["+digits+"]+)\\.(["+digits+"]*)"
      return new RegExp("^(?:([+-]?)(" +
                        decimalNumOnRight+"|"+decimalNumOnLeft +
                        "))$");
    }
    function schemeScientificPattern(digits, exp_mark) {
      var noDecimal = "["+digits+"]+"
      var decimalNumOnRight = "["+digits+"]*\\.["+digits+"]+"
      var decimalNumOnLeft = "["+digits+"]+\\.["+digits+"]*"
      return new RegExp("^(?:([+-]?" +
			"(?:"+noDecimal+"|"+decimalNumOnRight+"|"+decimalNumOnLeft+")" +
			")["+exp_mark+"]([+-]?["+digits+"]+))$");
    }

    function digitsForRadix(radix) {
      return radix === 2  ? "01" :
	radix === 8  ? "0-7" :
	radix === 10 ? "0-9" :
	radix === 16 ? "0-9a-fA-F" :
	errbacks.throwInternalError("digitsForRadix: invalid radix", this, radix)
    }
    function expMarkForRadix(radix) {
      return (radix === 2 || radix === 8 || radix === 10) ? "defsl" :
	(radix === 16)                               ? "sl" :
	errbacks.throwInternalError("expMarkForRadix: invalid radix", this, radix)
    }

    function Exactness(i) {
      this.defaultp = function () { return i == 0; }
      this.exactp = function () { return i == 1; }
      this.inexactp = function () { return i == 2; }
    }

    Exactness.def = new Exactness(0);
    Exactness.on = new Exactness(1);
    Exactness.off = new Exactness(2);

    Exactness.prototype.intAsExactp = function () { return this.defaultp() || this.exactp(); };
    Exactness.prototype.floatAsInexactp = function () { return this.defaultp() || this.inexactp(); };

    // fromSchemeString: string boolean -> (scheme-number | false)
    var fromSchemeString = function(x, exactness) {

      var radix = 10
      var exactness = typeof exactness === 'undefined' ? Exactness.def :
	  exactness === true               ? Exactness.on :
	  exactness === false              ? Exactness.off :
	  /* else */  errbacks.throwInternalError( "exactness must be true or false"
                                                   , x
                                                   , exactness) ;

      var hMatch = x.toLowerCase().match(hashModifiersRegexp)
      if (hMatch) {
	var modifierString = hMatch[1].toLowerCase();

	var exactFlag = modifierString.match(new RegExp("(#[ei])"))
	var radixFlag = modifierString.match(new RegExp("(#[bodx])"))

	if (exactFlag) {
	  var f = exactFlag[1].charAt(1)
	  exactness = f === 'e' ? Exactness.on :
	    f === 'i' ? Exactness.off :
	    // this case is unreachable
	  errbacks.throwInternalError("invalid exactness flag", x)
	}
	if (radixFlag) {
	  var f = radixFlag[1].charAt(1)
	  radix = f === 'b' ? 2 :
            f === 'o' ? 8 :
            f === 'd' ? 10 :
            f === 'x' ? 16 :
	    // this case is unreachable
	  errbacks.throwInternalError("invalid radix flag", x)
	}
      }

      var numberString = hMatch ? hMatch[2] : x
      // if the string begins with a hash modifier, then it must parse as a
      // number, an invalid parse is an error, not false. False is returned
      // when the item could potentially have been read as a symbol.
      var mustBeANumberp = hMatch ? true : false

      return fromSchemeStringRaw(numberString, radix, exactness, mustBeANumberp)
    };

    function fromSchemeStringRaw(x, radix, exactness, mustBeANumberp) {
      var cMatch = matchComplexRegexp(radix, x);
      if (cMatch) {
        errbacks.throwGeneralError("Complex Numbers are not supported in Pyret: " + x);
      }

      return fromSchemeStringRawNoComplex(x, radix, exactness, mustBeANumberp)
    }

    function fromSchemeStringRawNoComplex(x, radix, exactness, mustBeANumberp) {
      var aMatch = x.match(schemeRationalRegexp(digitsForRadix(radix)));
      if (aMatch) {
        // parse both parts exactly, then apply the requested exactness to
        // the whole ratio (parsing the parts inexactly would build a
        // Rational out of Roughnums)
        var schemeN = fromSchemeStringRawNoComplex(aMatch[1], radix, Exactness.on);
        var schemeD = fromSchemeStringRawNoComplex(aMatch[2], radix, Exactness.on);
        if (_integerIsZero(schemeD)) {
          if (mustBeANumberp) {
            errbacks.throwDivByZero("division by zero in " + x);
          }
          return false;
        }
        var schemeRat = Rational.makeInstance(schemeN, schemeD);
        return exactness.inexactp() ? toRoughnum(schemeRat) : schemeRat;
      }

      if (x === '+nan.0' ||
          x === '-nan.0' ||
          x === '+inf.0' ||
          x === '-inf.0') {
        errbacks.throwDomainError(x + ' is not supported in Pyret');
      }

      var fMatch = x.match(schemeFlonumRegexp(digitsForRadix(radix)))
      if (fMatch) {
	var integralPart = fMatch[3] !== undefined ? fMatch[3] : fMatch[5];
	var fractionalPart = fMatch[4] !== undefined ? fMatch[4] : fMatch[6];
	return parseFloat( fMatch[1]
                           , integralPart
                           , fractionalPart
                           , radix
                           , exactness
                         )
      }

      var sMatch = x.match(schemeScientificPattern( digitsForRadix(radix)
					            , expMarkForRadix(radix)
					          ))
      if (sMatch) {
	var coefficient = fromSchemeStringRawNoComplex(sMatch[1], radix, exactness)
	var exponent = fromSchemeStringRawNoComplex(sMatch[2], radix, exactness)
	return multiply(coefficient, expt(radix, exponent));
      }

      // Finally, integer tests.
      if (x.match(schemeDigitRegexp(digitsForRadix(radix)))) {
	var n = parseInt(x, radix);
	if (isOverflow(n)) {
	  var overflowed = parseBigInteger(x, radix);
	  return exactness.intAsExactp() ? overflowed : overflowed.toRoughnum();
	} else if (exactness.intAsExactp()) {
	  return n;
	} else {
	  return Roughnum.makeInstance(n)
	}
      } else if (mustBeANumberp) {
	if(x.length===0) errbacks.throwGeneralError("no digits");
	errbacks.throwGeneralError("bad number: " + x);
      } else {
	return false;
      }
    };

    function parseFloat(sign, integralPart, fractionalPart, radix, exactness) {
      var sign = (sign == "-" ? -1 : 1);
      var integralPartValue = integralPart === ""  ? 0  :
	  exactness.intAsExactp() ? parseExactInt(integralPart, radix) :
	  parseInt(integralPart, radix)

      var fractionalNumerator = fractionalPart === "" ? 0 :
	  exactness.intAsExactp() ? parseExactInt(fractionalPart, radix) :
	  parseInt(fractionalPart, radix)
      /* unfortunately, for these next two calculations, `expt` and `divide` */
      /* will promote to Bignum and Rational, respectively, but we only want */
      /* these if we're parsing in exact mode */
      var fractionalDenominator = exactness.intAsExactp() ? expt(radix, fractionalPart.length) :
	  Math.pow(radix, fractionalPart.length)
      var fractionalPartValue = fractionalPart === "" ? 0 :
	  exactness.intAsExactp() ? divide(fractionalNumerator, fractionalDenominator) :
	  fractionalNumerator / fractionalDenominator

      var forceInexact = function(o) {
	return typeof o === "number" ? Roughnum.makeInstance(o) :
	  o.toRoughnum();
      }

      return exactness.floatAsInexactp() ? forceInexact(multiply(sign, add( integralPartValue, fractionalPartValue))) :
	multiply(sign, add(integralPartValue, fractionalPartValue));
    }

    function parseExactInt(str, radix) {
      return fromSchemeStringRawNoComplex(str, radix, Exactness.on, true);
    }

    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////

    // BigInteger boxes a native BigInt in field v.

    var BIG_ZERO = BigInt(0);
    var BIG_ONE = BigInt(1);

    function BigInteger(v) {
      this.v = v;
    }

    function nbv(v) { return new BigInteger(v); }

    var BIG_RADIX_PREFIX = { 2: '0b', 8: '0o', 10: '', 16: '0x' };

    var parseBigInteger = function(s, radix) {
      var negative = false;
      if (s.charAt(0) === '-' || s.charAt(0) === '+') {
        negative = s.charAt(0) === '-';
        s = s.substring(1);
      }
      var v = BigInt(BIG_RADIX_PREFIX[radix] + s);
      return nbv(negative ? -v : v);
    };

    var bigAbs = function(v) { return v < BIG_ZERO ? -v : v; };

    var bigTooLarge = function(e, what) {
      if (e instanceof RangeError) {
        errbacks.throwDomainError(what + ' too large');
      }
      throw e;
    };

    function bnToString(b) { return this.v.toString(b || 10); }
    function bnNegate() { return nbv(-this.v); }
    function bnAbs() { return this.v < BIG_ZERO ? nbv(-this.v) : this; }
    function bnCompareTo(a) { return this.v < a.v ? -1 : (this.v > a.v ? 1 : 0); }
    function bnEquals(a) { return this.v === a.v; }
    function bnSigNum() { return this.v < BIG_ZERO ? -1 : (this.v > BIG_ZERO ? 1 : 0); }
    function bnBitLength() {
      var v = bigAbs(this.v);
      if (v === BIG_ZERO) { return 0; }
      var hex = v.toString(16);
      return 4 * (hex.length - 1) + (32 - Math.clz32(parseInt(hex.charAt(0), 16)));
    }
    function bnAdd(a) { return nbv(this.v + a.v); }
    function bnSubtract(a) { return nbv(this.v - a.v); }
    function bnMultiply(a) {
      try { return nbv(this.v * a.v); }
      catch (e) { return bigTooLarge(e, 'multiply: result'); }
    }
    function bnDivide(a) { return nbv(this.v / a.v); }
    function bnRemainder(a) { return nbv(this.v % a.v); }
    function bnDivideAndRemainder(a) { return [nbv(this.v / a.v), nbv(this.v % a.v)]; }
    function bnMod(a) {
      var r = this.v % a.v;
      if (r < BIG_ZERO) { r += bigAbs(a.v); }
      return nbv(r);
    }
    function bnShiftLeft(n) {
      try { return nbv(this.v << BigInt(n)); }
      catch (e) { return bigTooLarge(e, 'shift: result'); }
    }
    function bnShiftRight(n) { return nbv(this.v >> BigInt(n)); }
    function bnTestBit(n) { return ((this.v >> BigInt(n)) & BIG_ONE) !== BIG_ZERO; }
    function bnGCD(a) {
      var x = bigAbs(this.v), y = bigAbs(a.v), t;
      while (y !== BIG_ZERO) {
        t = x % y;
        x = y;
        y = t;
      }
      return nbv(x);
    }
    function bnPow(e) {
      if (greaterThan(e, 0xffffffff)) {
        errbacks.throwDomainError('expt: exponent ' + e + ' too large');
      }
      if (lessThan(e, 1)) {
        return BigInteger.ONE;
      }
      try { return nbv(this.v ** BigInt(e instanceof BigInteger ? e.v : e)); }
      catch (err) { return bigTooLarge(err, 'expt: result'); }
    }

    BigInteger.prototype.toString = bnToString;
    BigInteger.prototype.negate = bnNegate;
    BigInteger.prototype.abs = bnAbs;
    BigInteger.prototype.compareTo = bnCompareTo;
    BigInteger.prototype.equals = bnEquals;
    BigInteger.prototype.signum = bnSigNum;
    BigInteger.prototype.bitLength = bnBitLength;
    BigInteger.prototype.add = bnAdd;
    BigInteger.prototype.subtract = bnSubtract;
    BigInteger.prototype.multiply = bnMultiply;
    BigInteger.prototype.remainder = bnRemainder;
    BigInteger.prototype.divideAndRemainder = bnDivideAndRemainder;
    BigInteger.prototype.mod = bnMod;
    BigInteger.prototype.shiftLeft = bnShiftLeft;
    BigInteger.prototype.shiftRight = bnShiftRight;
    BigInteger.prototype.testBit = bnTestBit;
    BigInteger.prototype.gcd = bnGCD;
    BigInteger.prototype.pow = bnPow;

    BigInteger.ZERO = nbv(BIG_ZERO);
    BigInteger.ONE = nbv(BIG_ONE);
    BigInteger.NEGATIVE_ONE = nbv(-BIG_ONE);

    // Other methods we need to add for compatibilty with js-numbers numeric tower.

    // add is implemented above.
    // subtract is implemented above.
    // multiply is implemented above.
    // equals is implemented above.
    // abs is implemented above.
    // negate is defined above.

    // makeBignum: string -> BigInteger
    var makeBignum = function(s) {
      if (typeof(s) === 'number') {
        if (Number.isInteger(s)) { return nbv(BigInt(s)); }
        s = s + '';
      }
      var sci = s.match(scientificPattern);
      if (sci) {
        var fraction = (sci[1].split('.')[1] || '').replace(/0+$/, '');
        if (fraction.length > Number(sci[2])) {
          errbacks.throwDomainError('makeBignum: ' + s + ' is not an integer string');
        }
      }
      s = expandExponent(s);
      if (! s.match(digitRegexp)) {
        errbacks.throwDomainError('makeBignum: ' + s + ' is not an integer string');
      }
      return parseBigInteger(s, 10);
    };

    var zerostring = function(n) {
      var buf = [];
      for (var i = 0; i < n; i++) {
        buf.push('0');
      }
      return buf.join('');
    };

    BigInteger.prototype.isFinite = function() {
      return true;
    };

    BigInteger.prototype.isInteger = function() {
      return true;
    };

    BigInteger.prototype.isRational = function() {
      return true;
    };

    BigInteger.prototype.isExact = BigInteger.prototype.isRational;

    BigInteger.prototype.isReal = function() {
      return true;
    };

    BigInteger.prototype.isRoughnum = function() {
      return false;
    };

    BigInteger.prototype.isPositive = function() {
      return this.compareTo(BigInteger.ZERO) > 0;
    };

    BigInteger.prototype.isNonNegative = function() {
      return this.compareTo(BigInteger.ZERO) >= 0;
    };

    BigInteger.prototype.isNegative = function() {
      return this.compareTo(BigInteger.ZERO) < 0;
    };

    BigInteger.prototype.isNonPositive = function() {
      return this.compareTo(BigInteger.ZERO) <= 0;
    };

    BigInteger.prototype.toRational = function() {
      return this;
    };

    BigInteger.prototype.toExact = BigInteger.prototype.toRational;

    BigInteger.prototype.toFixnum = function() {
      return bigIntegerQuotientToDouble(this, BigInteger.ONE);
    }

    BigInteger.prototype.toRoughnum = function() {
      return Roughnum.makeInstance(this.toFixnum());
    };

    BigInteger.prototype.greaterThan = function(other) {
      return this.compareTo(other) > 0;
    };

    BigInteger.prototype.greaterThanOrEqual = function(other) {
      return this.compareTo(other) >= 0;
    };

    BigInteger.prototype.lessThan = function(other) {
      return this.compareTo(other) < 0;
    };

    BigInteger.prototype.lessThanOrEqual = function(other) {
      return this.compareTo(other) <= 0;
    };

    // divide: pyretnum -> pyretnum
    // WARNING NOTE: we override the old version of divide.
    BigInteger.prototype.divide = function(other) {
      var quotientAndRemainder = bnDivideAndRemainder.call(this, other);
      if (quotientAndRemainder[1].compareTo(BigInteger.ZERO) === 0) {
        return quotientAndRemainder[0];
      } else {
        var result = add(quotientAndRemainder[0],
                         Rational.makeInstance(quotientAndRemainder[1], other));
        return result;
      }
    };

    BigInteger.prototype.numerator = function() {
      return this;
    };

    BigInteger.prototype.denominator = function() {
      return 1;
    };

    (function() {
      // Classic implementation of Newton-Raphson square-root search,
      // adapted for integer-sqrt.
      // http://en.wikipedia.org/wiki/Newton's_method#Square_root_of_a_number
      var searchIter = function(n, guess) {
        while(!(lessThanOrEqual(sqr(guess),n) &&
                lessThan(n,sqr(add(guess, 1))))) {
          guess = floor(divide(add(guess,
                                   floor(divide(n, guess))),
                               2));
        }
        return guess;
      };

      // integerSqrt: -> pyretnum
      BigInteger.prototype.integerSqrt = function() {
        var n;
        if(sign(this) >= 0) {
          return searchIter(this, BigInteger.ONE.shiftLeft(Math.ceil(this.bitLength() / 2)));
        } else {
          errbacks.throwDomainError('integerSqrt of negative bignum ' + this);
        }
      };
    })();

    (function() {
      // Get an approximation using integerSqrt, and then start another
      // Newton-Raphson search if necessary.
      BigInteger.prototype.sqrt = function() {
        var approx = this.integerSqrt(), fix;
        if (eqv(sqr(approx), this)) {
          return approx;
        }
        fix = toFixnum(this);
        if (isFinite(fix)) {
          return Roughnum.makeInstance(Math.sqrt(fix));
        }
        return Roughnum.makeInstance(bigIntegerSqrtToDouble(this));
      };
    })();

    // sqrt: -> pyretnum
    // http://en.wikipedia.org/wiki/Newton's_method#Square_root_of_a_number
    // Produce the square root.

    // floor: -> pyretnum
    // Produce the floor.
    BigInteger.prototype.floor = function() {
      return this;
    }

    // ceiling: -> pyretnum
    // Produce the ceiling.
    BigInteger.prototype.ceiling = function() {
      return this;
    }

    // round: -> pyretnum
    // Round to the nearest integer.
    BigInteger.prototype.round = function() {
      return this;
    };

    BigInteger.prototype.roundEven = function() {
      return this;
    };

    // log: -> pyretnum
    // Produce the log.
    BigInteger.prototype.log = function() {
      return log(this);
    };

    // tan: -> pyretnum
    // Produce the tan.
    BigInteger.prototype.tan = function() {
      return tan(this.toFixnum());
    };

    // atan: -> pyretnum
    // Produce the arc tangent.
    BigInteger.prototype.atan = function() {
      return atan(this.toFixnum());
    };

    // cos: -> pyretnum
    // Produce the cosine.
    BigInteger.prototype.cos = function() {
      return cos(this.toFixnum());
    };

    // sin: -> pyretnum
    // Produce the sine.
    BigInteger.prototype.sin = function() {
      return sin(this.toFixnum());
    };

    // expt: pyretnum -> pyretnum
    // Produce the power to the input.
    BigInteger.prototype.expt = function(n) {
      return bnPow.call(this, n);
    };

    // exp: -> pyretnum
    // Produce e raised to the given power.
    BigInteger.prototype.exp = function() {
      var res = Math.exp(this.toFixnum());
      if (!isFinite(res))
        errbacks.throwDomainError('exp: argument too large: ' + this);
      return Roughnum.makeInstance(res);
    };

    // acos: -> pyretnum
    // Produce the arc cosine.
    BigInteger.prototype.acos = function() {
      return Roughnum.makeInstance(Math.acos(this.toFixnum()));
    };

    // asin: -> pyretnum
    // Produce the arc sine.
    BigInteger.prototype.asin = function() {
      return Roughnum.makeInstance(Math.asin(this.toFixnum()));
    };

    //////////////////////////////////////////////////////////////////////
    // getResidue: integer, integer, integer -> [string, string]
    //
    // Given the numerator and denominator of a proper (<= 1) fraction,
    // returns two strings constituting its repeating-decimal representation,
    // where the first string is the non-repeating digits immediately after the
    // decimal point, and the second string is the repeating digits thereafter.
    // The third argument is the limit on the size of the repeating digits.
    // If exceeded, the second string is `...`.
    var getResidue = function(r, d, limit) {
      var digits = [];
      var seenRemainders = {};
      seenRemainders[r] = true;
      while(true) {
        if (limit-- <= 0) {
          return [digits.join(''), '...']
        }

        var nextDigit = quotient(
          multiply(r, 10), d);
        var nextRemainder = remainder(
          multiply(r, 10),
          d);
        digits.push(nextDigit.toString());
        if (seenRemainders[nextRemainder]) {
          r = nextRemainder;
          break;
        } else {
          seenRemainders[nextRemainder] = true;
          r = nextRemainder;
        }
      }

      var firstRepeatingRemainder = r;
      var repeatingDigits = [];
      while (true) {
        var nextDigit = quotient(multiply(r, 10), d);
        var nextRemainder = remainder(
          multiply(r, 10),
          d);
        repeatingDigits.push(nextDigit.toString());
        if (equals(nextRemainder, firstRepeatingRemainder)) {
          break;
        } else {
          r = nextRemainder;
        }
      };

      var digitString = digits.join('');
      var repeatingDigitString = repeatingDigits.join('');

      while (digitString.length >= repeatingDigitString.length &&
             (digitString.substring(
               digitString.length - repeatingDigitString.length)
              === repeatingDigitString)) {
        digitString = digitString.substring(
          0, digitString.length - repeatingDigitString.length);
      }

      return [digitString, repeatingDigitString];

    };

    //////////////////////////////////////////////////////////////////////
    // toRepeatingDecimal: jsnum jsnum {limit: number}? -> [string, string, string]
    //
    // Given the numerator and denominator parts of a rational,
    // produces the repeating-decimal representation, where the first
    // part are the digits before the decimal, the second are the
    // non-repeating digits after the decimal, and the third are the
    // remaining repeating decimals.
    //
    // An optional limit on the decimal expansion can be provided via
    // a `limit` field of an object supplied as a third argument. This
    // cuts off the search if we go past the limit.
    // If this happens, the third string returned becomes '...' to indicate
    // that the search was prematurely cut off.
    // The default limit is 512.
    var toRepeatingDecimal = function(n, d, options) {
      // default limit on decimal expansion; can be overridden
      var limit = 512;
      if (options && typeof(options.limit) !== 'undefined') {
        limit = options.limit;
      }
      if (! isInteger(n)) {
        errbacks.throwDomainError('toRepeatingDecimal: n ' + n.toString() +
                                  " is not an integer.");
      }
      if (! isInteger(d)) {
        errbacks.throwDomainError('toRepeatingDecimal: d ' + d.toString() +
                                  " is not an integer.");
      }
      if (equals(d, 0)) {
        errbacks.throwDomainError('toRepeatingDecimal: d equals 0');
      }
      if (lessThan(d, 0)) {
        errbacks.throwDomainError('toRepeatingDecimal: d < 0');
      }
      var sign = (lessThan(n, 0) ? "-" : "");
      n = abs(n);
      var beforeDecimalPoint = sign + quotient(n, d);
      var afterDecimals = getResidue(remainder(n, d), d, limit);
      return [beforeDecimalPoint].concat(afterDecimals);
    };
    //////////////////////////////////////////////////////////////////////
    // toStringDigits: jsnum jsnum -> string
    // Converts the number to a string, providing digits precision in the
    // output.  If digits is positive, provides that many digits to the right
    // of the decimal point (including adding zeroes beyond the actual precision of
    // the number).  If digits is negative, rounds that many positions to the
    // left of the decimal, replacing them with zeroes.
    //
    // Note that num-to-string-digits is only for formatting, and its
    // output's apparent precision may be unrelated to the actual precision of the
    // input number, which may have been an approximation, or unrepresentable in
    // decimal.
    function toStringDigits(n, digits) {
      if (!isInteger(digits)) {
        errbacks.throwDomainError('num-to-string-digits: digits should be an integer');
      }
      var tenDigits = expt(10, digits);
      var d = toFixnum(digits);
      n = divide(round(multiply(n, tenDigits)), tenDigits);
      if (isInteger(n)) {
        var ans = n.toString();
        if (d >= 1) {
          ans += '.';
          for (var i = 0; i < d; i++) {
            ans += '0';
          }
        }
        return ans;
      }
      // n is not an integer implies that d >= 1; n was rounded to d
      // digits above, so its expansion terminates within d digits — make
      // sure the expansion limit doesn't cut it off first
      var decimal = toRepeatingDecimal(n.numerator(), n.denominator(),
                                       { limit: Math.max(512, d + 10) });
      var ans = decimal[1].toString();
      while (ans.length < d) {
        ans += decimal[2];
      }
      return decimal[0] + '.' + ans.substring(0, d);
    }
    //////////////////////////////////////////////////////////////////////

    // External interface of js-numbers:

    Numbers['fromFixnum'] = fromFixnum;
    Numbers['fromString'] = fromString;
    Numbers['fromSchemeString'] = fromSchemeString;
    Numbers['makeBignum'] = makeBignum;
    Numbers['makeRational'] = Rational.makeInstance;
    Numbers['makeRoughnum'] = Roughnum.makeInstance;

    Numbers['isPyretNumber'] = isPyretNumber;
    Numbers['isRational'] = isRational;
    Numbers['isReal'] = isReal;
    Numbers['isExact'] = isExact;
    Numbers['isInteger'] = isInteger;
    Numbers['isRoughnum'] = isRoughnum;
    Numbers['isPositive'] = isPositive;
    Numbers['isNegative'] = isNegative;
    Numbers['isNonPositive'] = isNonPositive;
    Numbers['isNonNegative'] = isNonNegative;

    Numbers['toFixnum'] = toFixnum;
    Numbers['toExact'] = toExact;
    Numbers['toRational'] = toRational;
    Numbers['toRoughnum'] = toRoughnum;

    Numbers['add'] = add;
    Numbers['subtract'] = subtract;
    Numbers['multiply'] = multiply;
    Numbers['divide'] = divide;
    Numbers['equals'] = equals;
    Numbers['equalsAnyZero'] = equalsAnyZero;
    Numbers['eqv'] = eqv; // why is this being exported?
    Numbers['roughlyEquals'] = roughlyEquals;
    Numbers['roughlyEqualsRel'] = roughlyEqualsRel;
    Numbers['greaterThanOrEqual'] = greaterThanOrEqual;
    Numbers['lessThanOrEqual'] = lessThanOrEqual;
    Numbers['greaterThan'] = greaterThan;
    Numbers['lessThan'] = lessThan;
    Numbers['expt'] = expt;
    Numbers['exp'] = exp;
    Numbers['modulo'] = modulo;
    Numbers['numerator'] = numerator;
    Numbers['denominator'] = denominator;
    Numbers['integerSqrt'] = integerSqrt;
    Numbers['sqrt'] = sqrt;
    Numbers['abs'] = abs;
    Numbers['quotient'] = quotient;
    Numbers['remainder'] = remainder;
    Numbers['floor'] = floor;
    Numbers['ceiling'] = ceiling;
    Numbers['round'] = round;
    Numbers['roundEven'] = roundEven;
    Numbers['log'] = log;
    Numbers['tan'] = tan;
    Numbers['atan'] = atan;
    Numbers['atan2'] = atan2;
    Numbers['cos'] = cos;
    Numbers['sin'] = sin;
    Numbers['tan'] = tan;
    Numbers['acos'] = acos;
    Numbers['asin'] = asin;
    Numbers['sqr'] = sqr;
    Numbers['gcd'] = gcd;
    Numbers['lcm'] = lcm;

    Numbers['toRepeatingDecimal'] = toRepeatingDecimal;
    Numbers['toStringDigits'] = toStringDigits;

    // The following exposes the class representations for easier
    // integration with other projects.
    Numbers['BigInteger'] = BigInteger;
    Numbers['Rational'] = Rational;
    Numbers['Roughnum'] = Roughnum;
    Numbers['FloatPoint'] = Roughnum; //FIXME
    Numbers['Complex'] = Roughnum; //FIXME

    Numbers['MIN_FIXNUM'] = MIN_FIXNUM;
    Numbers['MAX_FIXNUM'] = MAX_FIXNUM;

    // the following exposes innards for testing
    Numbers['_innards'] = {
      _integerIsZero: _integerIsZero,
      _integerIsOne: _integerIsOne,
      _integerIsNegativeOne: _integerIsNegativeOne,
      _integerModulo: _integerModulo,
      _integerGcd: _integerGcd,
      _integerDivideToFixnum: _integerDivideToFixnum,
      _integerEquals: _integerEquals,
      _integerGreaterThan: _integerGreaterThan,
      _integerLessThan: _integerLessThan,
      _integerGreaterThanOrEqual: _integerGreaterThanOrEqual,
      _integerLessThanOrEqual: _integerLessThanOrEqual,
      splitIntIntoMantissaExpt: splitIntIntoMantissaExpt,
      getResidue: getResidue,
      integerNthRoot: integerNthRoot,
      liftFixnumInteger: liftFixnumInteger,
      makeIntegerUnOp: makeIntegerUnOp,
      makeIntegerBinop: makeIntegerBinop,
      makeNumericBinop: makeNumericBinop,
      nthRoot: nthRoot,
      sign: sign,
      zfill: zfill,
      getErrbacks: function() { return errbacks; },
      setErrbacks: function(newErrbacks) {
        errbacks = newErrbacks;
      }
    };

    return Numbers;
  }

  var INITIAL_ERRBACKS = (function() {
    function makeThrower(tag) {
      return function(msg, ...extras) {
        var suffix = extras.length
          ? ' [' + extras.map(function(x) {
              try { return String(x); } catch (_) { return '<unprintable>'; }
            }).join(', ') + ']'
          : '';
        throw new Error('js-numbers ' + tag + ': ' + msg + suffix);
      };
    }
    return {
      throwDivByZero:         makeThrower('division-by-zero'),
      throwDomainError:       makeThrower('domain-error'),
      throwGeneralError:      makeThrower('general-error'),
      throwIncomparableValues:makeThrower('incomparable-values'),
      throwInternalError:     makeThrower('internal-error'),
      throwLogNonPositive:    makeThrower('log-non-positive'),
      throwRelToleranceError: makeThrower('relative-tolerance-error'),
      throwSqrtNegative:      makeThrower('sqrt-negative'),
      throwToleranceError:    makeThrower('tolerance-error'),
      throwUndefinedValue:    makeThrower('undefined-value'),
    };
  })();

  // A single library instance. We can't regenerate this per call — the
  // class identities (Rational/Roughnum/BigInteger) have to be stable
  // across the module's lifetime or instanceof checks start failing.
  var DEFAULT_LIBRARY = MakeNumberLibrary(INITIAL_ERRBACKS);
  var getErrbacks = DEFAULT_LIBRARY._innards.getErrbacks;
  var setErrbacks = DEFAULT_LIBRARY._innards.setErrbacks;

  // Parameterize-style wrappers: for one call, swap the library's current
  // errbacks in for whatever the caller supplied (or INITIAL_ERRBACKS if they
  // didn't), then restore on exit — including on throw via finally.

  function wrapErrbacks1(f) {
    return function(a, errbacks) {
      var saved = getErrbacks();
      try {
        setErrbacks(errbacks ?? INITIAL_ERRBACKS);
        return f(a);
      } finally {
        setErrbacks(saved);
      }
    };
  }

  function wrapErrbacks2(f) {
    return function(a, b, errbacks) {
      var saved = getErrbacks();
      try {
        setErrbacks(errbacks ?? INITIAL_ERRBACKS);
        return f(a, b);
      } finally {
        setErrbacks(saved);
      }
    };
  }

  function wrapErrbacks3(f) {
    return function(a, b, c, errbacks) {
      var saved = getErrbacks();
      try {
        setErrbacks(errbacks ?? INITIAL_ERRBACKS);
        return f(a, b, c);
      } finally {
        setErrbacks(saved);
      }
    };
  }

  // For exports with no fixed arity. A trailing errbacks argument is detected
  // by duck-typing on one of the throw* methods — pyretnums don't carry those,
  // so the ambiguity is only theoretical.
  function wrapErrbacksVariadic(f) {
    return function(...args) {
      var errbacks;
      var last = args.length > 0 ? args[args.length - 1] : undefined;
      if (last !== null && typeof last === 'object' &&
          typeof last.throwDomainError === 'function') {
        errbacks = args.pop();
      }
      var saved = getErrbacks();
      try {
        setErrbacks(errbacks ?? INITIAL_ERRBACKS);
        return f.apply(null, args);
      } finally {
        setErrbacks(saved);
      }
    };
  }

  return {
    MakeNumberLibrary,

    fromFixnum: wrapErrbacks1(DEFAULT_LIBRARY.fromFixnum),
    fromString: wrapErrbacks1(DEFAULT_LIBRARY.fromString),
    fromSchemeString: wrapErrbacks2(DEFAULT_LIBRARY.fromSchemeString),
    makeBignum: wrapErrbacks1(DEFAULT_LIBRARY.makeBignum),
    makeRational: wrapErrbacksVariadic(DEFAULT_LIBRARY.makeRational),
    makeRoughnum: wrapErrbacks1(DEFAULT_LIBRARY.makeRoughnum),

    isPyretNumber: wrapErrbacks1(DEFAULT_LIBRARY.isPyretNumber),
    isRational: wrapErrbacks1(DEFAULT_LIBRARY.isRational),
    isReal: wrapErrbacks1(DEFAULT_LIBRARY.isReal),
    isExact: wrapErrbacks1(DEFAULT_LIBRARY.isExact),
    isInteger: wrapErrbacks1(DEFAULT_LIBRARY.isInteger),
    isRoughnum: wrapErrbacks1(DEFAULT_LIBRARY.isRoughnum),
    isPositive: wrapErrbacks1(DEFAULT_LIBRARY.isPositive),
    isNegative: wrapErrbacks1(DEFAULT_LIBRARY.isNegative),
    isNonPositive: wrapErrbacks1(DEFAULT_LIBRARY.isNonPositive),
    isNonNegative: wrapErrbacks1(DEFAULT_LIBRARY.isNonNegative),

    toFixnum: wrapErrbacks1(DEFAULT_LIBRARY.toFixnum),
    toExact: wrapErrbacks1(DEFAULT_LIBRARY.toExact),
    toRational: wrapErrbacks1(DEFAULT_LIBRARY.toRational),
    toRoughnum: wrapErrbacks1(DEFAULT_LIBRARY.toRoughnum),

    add: wrapErrbacks2(DEFAULT_LIBRARY.add),
    subtract: wrapErrbacks2(DEFAULT_LIBRARY.subtract),
    multiply: wrapErrbacks2(DEFAULT_LIBRARY.multiply),
    divide: wrapErrbacks2(DEFAULT_LIBRARY.divide),
    equals: wrapErrbacks2(DEFAULT_LIBRARY.equals),
    equalsAnyZero: wrapErrbacks1(DEFAULT_LIBRARY.equalsAnyZero),
    eqv: wrapErrbacks2(DEFAULT_LIBRARY.eqv),
    roughlyEquals: wrapErrbacks3(DEFAULT_LIBRARY.roughlyEquals),
    roughlyEqualsRel: wrapErrbacksVariadic(DEFAULT_LIBRARY.roughlyEqualsRel),
    greaterThanOrEqual: wrapErrbacks2(DEFAULT_LIBRARY.greaterThanOrEqual),
    lessThanOrEqual: wrapErrbacks2(DEFAULT_LIBRARY.lessThanOrEqual),
    greaterThan: wrapErrbacks2(DEFAULT_LIBRARY.greaterThan),
    lessThan: wrapErrbacks2(DEFAULT_LIBRARY.lessThan),
    expt: wrapErrbacks2(DEFAULT_LIBRARY.expt),
    exp: wrapErrbacks1(DEFAULT_LIBRARY.exp),
    modulo: wrapErrbacks2(DEFAULT_LIBRARY.modulo),
    numerator: wrapErrbacks1(DEFAULT_LIBRARY.numerator),
    denominator: wrapErrbacks1(DEFAULT_LIBRARY.denominator),
    integerSqrt: wrapErrbacks1(DEFAULT_LIBRARY.integerSqrt),
    sqrt: wrapErrbacks1(DEFAULT_LIBRARY.sqrt),
    abs: wrapErrbacks1(DEFAULT_LIBRARY.abs),
    quotient: wrapErrbacks2(DEFAULT_LIBRARY.quotient),
    remainder: wrapErrbacks2(DEFAULT_LIBRARY.remainder),
    floor: wrapErrbacks1(DEFAULT_LIBRARY.floor),
    ceiling: wrapErrbacks1(DEFAULT_LIBRARY.ceiling),
    round: wrapErrbacks1(DEFAULT_LIBRARY.round),
    roundEven: wrapErrbacks1(DEFAULT_LIBRARY.roundEven),
    log: wrapErrbacks1(DEFAULT_LIBRARY.log),
    tan: wrapErrbacks1(DEFAULT_LIBRARY.tan),
    atan: wrapErrbacks1(DEFAULT_LIBRARY.atan),
    atan2: wrapErrbacks2(DEFAULT_LIBRARY.atan2),
    cos: wrapErrbacks1(DEFAULT_LIBRARY.cos),
    sin: wrapErrbacks1(DEFAULT_LIBRARY.sin),
    acos: wrapErrbacks1(DEFAULT_LIBRARY.acos),
    asin: wrapErrbacks1(DEFAULT_LIBRARY.asin),
    sqr: wrapErrbacks1(DEFAULT_LIBRARY.sqr),
    gcd: wrapErrbacks2(DEFAULT_LIBRARY.gcd),
    lcm: wrapErrbacks2(DEFAULT_LIBRARY.lcm),

    toRepeatingDecimal: wrapErrbacks3(DEFAULT_LIBRARY.toRepeatingDecimal),
    toStringDigits: wrapErrbacks2(DEFAULT_LIBRARY.toStringDigits),

    BigInteger: DEFAULT_LIBRARY.BigInteger,
    Rational: DEFAULT_LIBRARY.Rational,
    Roughnum: DEFAULT_LIBRARY.Roughnum,
    MIN_FIXNUM: DEFAULT_LIBRARY.MIN_FIXNUM,
    MAX_FIXNUM: DEFAULT_LIBRARY.MAX_FIXNUM,
    _innards: DEFAULT_LIBRARY._innards,
  }
});
