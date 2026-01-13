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
  // Abbreviation
  var Numbers = {};

  // makeNumericBinop: (fixnum fixnum -> any) (pyretnum pyretnum -> any) -> (pyretnum pyretnum) X
  // Creates a binary function that works either on fixnums or boxnums.
  // Applies the appropriate binary function, ensuring that both pyretnums are
  // coerced to be the same kind.
  var makeNumericBinop = function(onFixnums, onBoxednums, options) {
    options = options || {};
    return function(x, y, errbacks) {

      if (options.isXSpecialCase && options.isXSpecialCase(x, errbacks))
        return options.onXSpecialCase(x, y, errbacks);
      if (options.isYSpecialCase && options.isYSpecialCase(y, errbacks))
        return options.onYSpecialCase(x, y, errbacks);

      if (typeof(x) === 'number' &&
          typeof(y) === 'number') {
        return onFixnums(x, y, errbacks);
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
          y = y.toRoughnum(errbacks);
        }
      } else if (y instanceof Roughnum) {
        // x is rat or bigint
        x = x.toRoughnum(errbacks);
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

      return onBoxednums(x, y, errbacks);
    };
  };

  // fromFixnum: fixnum -> pyretnum
  var fromFixnum = function(x, errbacks) {
    return fromString(String(x), errbacks);
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

  var liftFixnumInteger = function(x, other, errbacks) {
    if (other instanceof Roughnum)
      return new Roughnum(x, errbacks);
    else if (other instanceof BigInteger)
      return makeBignum(x);
    else
      return new Rational(x, 1, errbacks);
  };


  // isPyretNumber: any -> boolean
  // Returns true if the thing is a pyretnum
  var isPyretNumber = function(thing) {
    return (typeof(thing) === 'number'
            || (thing instanceof Rational ||
                thing instanceof Roughnum ||
                thing instanceof BigInteger));
  };

  // isRational: pyretnum -> boolean
  var isRational = function(n) {
    return (typeof(n) === 'number' ||
            (isPyretNumber(n) && n.isRational()));
  };

  var isExact = isRational;

  // isReal: pyretnum -> boolean
  var isReal = function(n) {
    return (typeof(n) === 'number' ||
            (isPyretNumber(n) && n.isReal()));
  };

  // isInteger: pyretnum -> boolean
  var isInteger = function(n) {
    return (typeof(n) === 'number' ||
            (isPyretNumber(n) && n.isInteger()));
  };

  var isRoughnum = function(n) {
    if (typeof(n) === 'number') {
      return false;
    } else {
      return (isPyretNumber(n) && n.isRoughnum());
    }
  };

  var isPositive = function(n) {
    if (typeof(n) === 'number') {
      return n > 0;
    } else {
      return (isPyretNumber(n) && n.isPositive());
    }
  };

  var isNonPositive = function(n) {
    if (typeof(n) === 'number') {
      return n <= 0;
    } else {
      return (isPyretNumber(n) && n.isNonPositive());
    }
  };

  var isNegative = function(n) {
    if (typeof(n) === 'number') {
      return n < 0;
    } else {
      return (isPyretNumber(n) && n.isNegative());
    }
  };

  var isNonNegative = function(n) {
    if (typeof(n) === 'number') {
      return n >= 0;
    } else {
      return (isPyretNumber(n) && n.isNonNegative());
    }
  };

  // toFixnum: pyretnum -> javascript-number
  var toFixnum = function(n) {
    if (typeof(n) === 'number')
      return n;
    return n.toFixnum();
  };

  // toRational: pyretnum -> pyretnum
  var toRational = function(n, errbacks) {
    if (typeof(n) === 'number')
      return n;
    return n.toRational(errbacks);
  };

  var toExact = toRational;

  // toRoughnum: pyretnum -> pyretnum

  var toRoughnum = function(n, errbacks) {
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(n, errbacks);
    } else {
      return n.toRoughnum(errbacks);
    }
  };

  //////////////////////////////////////////////////////////////////////

  // add: pyretnum pyretnum -> pyretnum
  var add = function(x, y, errbacks) {
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
    return addSlow(x, y, errbacks);
  };

  var addSlow = makeNumericBinop(
    function(x, y, errbacks) {
      var sum = x + y;
      if (isOverflow(sum)) {
        return (makeBignum(x)).add(makeBignum(y));
      } else {
        return sum;
      }
    },
    function(x, y, errbacks) {
      return x.add(y);
    },
    {isXSpecialCase: function(x, errbacks) {
      return isInteger(x) && _integerIsZero(x) },
     onXSpecialCase: function(x, y, errbacks) { return y; },
     isYSpecialCase: function(y, errbacks) {
       return isInteger(y) && _integerIsZero(y) },
     onYSpecialCase: function(x, y, errbacks) { return x; }
    });

  var subtract = function(x, y, errbacks) {
    if (typeof(x) === 'number' && typeof(y) === 'number') {
      var diff = x - y;
      if (isOverflow(diff)) {
        return (makeBignum(x)).subtract(makeBignum(y));
      } else {
        return diff;
      }
    }
    return subtractSlow(x, y, errbacks);
  };

  // subtract: pyretnum pyretnum -> pyretnum
  var subtractSlow = makeNumericBinop(
    function(x, y, errbacks) {
      var diff = x - y;
      if (isOverflow(diff)) {
        return (makeBignum(x)).subtract(makeBignum(y));
      } else {
        return diff;
      }
    },
    function(x, y, errbacks) {
      return x.subtract(y);
    },
    {isXSpecialCase: function(x, errbacks) {
      return isInteger(x) && _integerIsZero(x) },
     onXSpecialCase: function(x, y, errbacks) { return negate(y, errbacks); },
     isYSpecialCase: function(y, errbacks) {
       return isInteger(y) && _integerIsZero(y) },
     onYSpecialCase: function(x, y, errbacks) { return x; }
    });

  // mulitply: pyretnum pyretnum -> pyretnum
  var multiply = function(x, y, errbacks) {
    var prod;
    if (typeof(x) === 'number' && typeof(y) === 'number') {
      prod = x * y;
      if (isOverflow(prod)) {
        return (makeBignum(x)).multiply(makeBignum(y));
      } else {
        return prod;
      }
    }
    return multiplySlow(x, y, errbacks);
  };
  var multiplySlow = makeNumericBinop(
    function(x, y, errbacks) {
      var prod = x * y;
      if (isOverflow(prod)) {
        return (makeBignum(x)).multiply(makeBignum(y), errbacks);
      } else {
        return prod;
      }
    },
    function(x, y, errbacks) {
      return x.multiply(y, errbacks);
    },
    {isXSpecialCase: function(x, errbacks) {
      return (isInteger(x) &&
              (_integerIsZero(x) || _integerIsOne(x) || _integerIsNegativeOne(x))) },
     onXSpecialCase: function(x, y, errbacks) {
       if (_integerIsZero(x))
         return 0;
       if (_integerIsOne(x))
         return y;
       if (_integerIsNegativeOne(x))
         return negate(y, errbacks);
     },
     isYSpecialCase: function(y, errbacks) {
       return (isInteger(y) &&
               (_integerIsZero(y) || _integerIsOne(y) || _integerIsNegativeOne(y)))},
     onYSpecialCase: function(x, y, errbacks) {
       if (_integerIsZero(y))
         return 0;
       if (_integerIsOne(y))
         return x;
       if (_integerIsNegativeOne(y))
         return negate(x, errbacks);
     }
    });

  // divide: pyretnum pyretnum -> pyretnum
  var divide = makeNumericBinop(
    function(x, y, errbacks) {
      if (_integerIsZero(y))
        errbacks.throwDivByZero("/: division by zero, " + x + ' ' + y);
      var div = x / y;
      if (isOverflow(div)) {
        return (makeBignum(x)).divide(makeBignum(y), errbacks);
      } else if (Math.floor(div) !== div) {
        return Rational.makeInstance(x, y, errbacks);
      } else {
        return div;
      }
    },
    function(x, y, errbacks) {
      if (equalsAnyZero(y, errbacks)) {
        errbacks.throwDivByZero('/: division by zero, ' + x + ' ' + y);
      }
      return x.divide(y, errbacks);
    },
    {
      isXSpecialCase: function(x, errbacks) {
        return equalsAnyZero(x, errbacks);
      },
      onXSpecialCase: function(x, y, errbacks) {
        if (equalsAnyZero(y, errbacks)) {
          errbacks.throwDivByZero("/: division by zero, " + x + ' ' + y);
        }
        return 0;
      },
      isYSpecialCase: function(y, errbacks) {
        return equalsAnyZero(y, errbacks);
      },
      onYSpecialCase: function(x, y, errbacks) {
        errbacks.throwDivByZero("/: division by zero, " + x + ' ' + y);
      }
    });

  var equals = function(x, y, errbacks) {
    if (x === y) { return true; }
    else {
      if (typeof x === "number" && typeof y === "number") { return false; }
      else {
        return equalsSlow(x, y, errbacks);
      }
    }
  };
  // equals: pyretnum pyretnum -> boolean
  var equalsSlow = makeNumericBinop(
    function(x, y, errbacks) {
      return x === y;
    },
    function(x, y, errbacks) {
      return x.equals(y, errbacks);
    });

  var equalsAnyZero = function(x, errbacks) {
    if (typeof(x) === 'number') return x === 0;
    if (isRoughnum(x)) return x.n === 0;
    return x.equals(0, errbacks);
  };

  // eqv: pyretnum pyretnum -> boolean
  var eqv = function(x, y, errbacks) {
    if (x === y)
      return true;
    if (typeof(x) === 'number' && typeof(y) === 'number')
      return x === y;
    var ex = isRational(x), ey = isRational(y);
    return (((ex && ey) || (!ex && !ey)) && equals(x, y, errbacks));
  };

  // approxEqual: pyretnum pyretnum pyretnum -> boolean
  var approxEquals = function(x, y, delta, errbacks) {
    return lessThanOrEqual(abs(subtract(x, y, errbacks), errbacks),
                           delta, errbacks);
  };

  // used for within
  var roughlyEquals = function(x, y, delta, errbacks) {
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

    var ratx = isRoughnum(x) ? x.toRational(errbacks) : x;
    var raty = isRoughnum(y) ? y.toRational(errbacks) : y;

    var ratdelta = isRoughnum(delta) ? delta.toRational(errbacks) : delta;
    return approxEquals(ratx, raty, ratdelta, errbacks);
  };

  var roughlyEqualsRel = function(computedValue, trueValue, delta, smoothed, errbacks) {
    if (isNegative(delta)) {
      errbacks.throwRelToleranceError('negative relative tolerance ' + delta)
    }

    if (computedValue === trueValue) {
      return true
    }

    var deltaIsRough = isRoughnum(delta)
    var argNumsAreRough = isRoughnum(computedValue) || isRoughnum(trueValue)

    var ratCv = isRoughnum(computedValue) ? computedValue.toRational(errbacks) : computedValue
    var ratTv = isRoughnum(trueValue) ? trueValue.toRational(errbacks) : trueValue

    var ratDelta = isRoughnum(delta) ? delta.toRational(errbacks): delta

    var err = abs(subtract(ratCv, ratTv, errbacks), errbacks)
    var denom = min(abs(ratCv, errbacks), abs(ratTv, errbacks), errbacks)
    if (smoothed) {
      denom = add(denom, 1, errbacks);
    }

    if (lessThanOrEqual(ratDelta, 1, errbacks)) {
      var absDelta = multiply(ratDelta, denom, errbacks)
      if (deltaIsRough && toRoughnum(absDelta, errbacks).n === Number.MIN_VALUE) {
        if (argNumsAreRough && Math.abs(toRoughnum(err, errbacks).n) === Number.MIN_VALUE) {
          errbacks.throwRelToleranceError('roughnum tolerance too small for meaningful comparison, ' +
                            computedValue + ' ' + trueValue + ' ' + delta)
        }
      }

      return lessThanOrEqual(err, absDelta, errbacks)
    } else {
      var errRatio = divide(err, denom, errbacks)

      if (deltaIsRough && delta.n === Number.MIN_VALUE) {
        if (argNumsAreRough && Math.abs(toRoughnum(errRatio, errbacks).n) === Number.MIN_VALUE) {
          errbacks.throwRelToleranceError('roughnum tolerance too small for meaningful comparison, ' +
                            computedValue + ' ' + trueValue + ' ' + delta)
        }
      }

      return lessThanOrEqual(errRatio, ratDelta, errbacks)
    }
  }

  // greaterThanOrEqual: pyretnum pyretnum -> boolean
  var greaterThanOrEqual = function(x, y, errbacks) {
    if(typeof x === "number" && typeof y === "number") {
      return x >= y;
    }
    return makeNumericBinop(undefined, function(x, y, errbacks) {
      return x.greaterThanOrEqual(y);
    })(x, y, errbacks);
  }

  // lessThanOrEqual: pyretnum pyretnum -> boolean
  var lessThanOrEqual = function(x, y, errbacks) {
    if(typeof x === "number" && typeof y === "number") {
      return x <= y;
    }
    return makeNumericBinop(undefined, function(x, y, errbacks) {
      return x.lessThanOrEqual(y);
    })(x, y, errbacks);
  };

  // greaterThan: pyretnum pyretnum -> boolean
  var greaterThan = function(x, y, errbacks) {
    if(typeof x === "number" && typeof y === "number") {
      return x > y;
    }
    return makeNumericBinop(undefined, function(x, y, errbacks) {
      return x.greaterThan(y);
    })(x, y, errbacks);
  };

  // lessThan: pyretnum pyretnum -> boolean
  var lessThan = function(x, y, errbacks) {
    if(typeof x === "number" && typeof y === "number") {
      return x < y;
    }
    return makeNumericBinop(undefined, function(x, y, errbacks) {
      return x.lessThan(y);
    })(x, y, errbacks);
  };

  // expt: pyretnum pyretnum -> pyretnum
  var expt = makeNumericBinop(
    function(x, y, errbacks) {
      var pow = Math.pow(x, y);
      if (isOverflow(pow)) {
        return (makeBignum(x)).expt(makeBignum(y));
      } else {
        return pow;
      }
    },
    function(x, y, errbacks) {
      return x.expt(y, errbacks);
    },
    {
      isXSpecialCase: function(x, errbacks) {
        return eqv(x, 0, errbacks) || eqv(x, 1, errbacks);
      },
      onXSpecialCase: function(x, y, errbacks) {
        if (eqv(x, 0, errbacks)) {
          if (eqv(y, 0, errbacks)) {
            return 1;
          } else if (lessThan(y, 0, errbacks)) {
            errbacks.throwDivByZero("expt: division by zero");
          } else {
            return 0;
          }
        } else { // i.e., x is 1
          return 1;
        }
      },

      isYSpecialCase: function(y, errbacks) {
        return eqv(y, 0, errbacks) || lessThan(y, 0, errbacks);
      },
      onYSpecialCase: function(x, y, errbacks) {
        if (eqv(y, 0, errbacks)) {
          return 1;
        } else { // i.e., y is negative
          return expt(divide(1, x, errbacks), negate(y, errbacks), errbacks);
        }
      }
    });

  // exp: pyretnum -> pyretnum
  var exp = function(n, errbacks) {
    if ( eqv(n, 0, errbacks) ) {
      return 1;
    }
    if (typeof(n) === 'number') {
      var res = Math.exp(n);
      if (!isFinite(res))
        errbacks.throwGeneralError('exp: argument too large: ' + n);
      return Roughnum.makeInstance(res, errbacks);
    }
    return n.exp(errbacks);
  };

  // modulo: pyretnum pyretnum -> pyretnum
  var modulo = function(m, n, errbacks) {
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
    if (typeof(m) === 'number') {
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
    if (lessThan(n, 0, errbacks)) {
      if (lessThanOrEqual(result, 0, errbacks)) {
        return result;
      }
      return add(result, n, errbacks);

    } else {
      if (lessThan(result, 0, errbacks)) {
        return add(result, n, errbacks);
      }
      return result;
    }
  };

  // numerator: pyretnum -> pyretnum
  var numerator = function(n, errbacks) {
    if (typeof(n) === 'number')
      return n;
    return n.numerator();
  };

  // denominator: pyretnum -> pyretnum
  var denominator = function(n, errbacks) {
    if (typeof(n) === 'number')
      return 1;
    return n.denominator();
  };

  // sqrt: pyretnum -> pyretnum
  var sqrt = function(n, errbacks) {
    if (lessThan(n, 0, errbacks)) {
      errbacks.throwSqrtNegative('sqrt: negative argument ' + n);
    }
    if (typeof(n) === 'number') {
      var result = Math.sqrt(n);
      if (Math.floor(result) === result) {
        return result;
      } else {
        return Roughnum.makeInstance(result, errbacks);
      }
    }
    return n.sqrt(errbacks);
  };

  // abs: pyretnum -> pyretnum
  var abs = function(n, errbacks) {
    if (typeof(n) === 'number') {
      return Math.abs(n);
    }
    return n.abs(errbacks);
  };

  // min :: pyretnum, pyretnum -> pyretnum
  var min = function(n, m, errbacks) {
    if (lessThan(n, m, errbacks)) {
      return n;
    }
    return m;
  }
  

  // floor: pyretnum -> pyretnum
  var floor = function(n, errbacks) {
    if (typeof(n) === 'number')
      return Math.floor(n);
    return n.floor(errbacks);
  };

  // ceiling: pyretnum -> pyretnum
  var ceiling = function(n, errbacks) {
    if (typeof(n) === 'number')
      return Math.ceil(n);
    return n.ceiling(errbacks);
  };

  // round: pyretnum -> pyretnum
  var round = function(n, errbacks) {
    if (typeof(n) === 'number') {
      return n;
    }
    return n.round(errbacks);
  };

  var roundEven = function(n, errbacks) {
    if (typeof(n) === 'number') return n;
    return n.roundEven(errbacks);
  };

  // NB: all of these trig-gy generic functions should now return roughnum rather than float
  // (except for an arg of 0, etc)

  var ln10 = Math.log(10)

  // log: pyretnum -> pyretnum
  var log = function(n, errbacks) {
    if ( eqv(n, 1, errbacks) ) {
      return 0;
    }
    if (lessThanOrEqual(n, 0, errbacks)) {
      errbacks.throwLogNonPositive('log: non-positive argument ' + n);
    }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.log(n), errbacks);
    }
    if (isRational(n) && !isInteger(n)) {
      return subtract(log(numerator(n, errbacks), errbacks),
        log(denominator(n, errbacks), errbacks),
        errbacks);
    }
    var nFix = n.toFixnum();
    if (typeof(nFix) === 'number' && nFix !== Infinity) {
      return Roughnum.makeInstance(Math.log(nFix), errbacks);
    }
    // at this point, n must be a very large positive number;
    // n > 1e308, i.e, has at least 308 digits;
    // we can safely ignore its fractional part;
    var nStr = n.round(errbacks).toString();
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
    return Roughnum.makeInstance(nLog, errbacks);
  };

  // tan: pyretnum -> pyretnum
  var tan = function(n, errbacks) {
    if (eqv(n, 0, errbacks)) { return 0; }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.tan(n), errbacks);
    }
    return n.tan(errbacks);
  };

  // atan: pyretnum -> pyretnum
  var atan = function(n, errbacks) {
    if (eqv(n, 0, errbacks)) { return 0; }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.atan(n), errbacks);
    }
    return n.atan(errbacks);
  };

  var atan2 = function(y, x, errbacks) {
    if (eqv(x, 0, errbacks)) { // x = 0
      if (eqv(y, 0, errbacks)) { // x = 0, y = 0
        //return Roughnum.makeInstance(Infinity, errbacks);
        errbacks.throwDomainError('atan2: out of domain argument (0, 0)');
      } else if (greaterThan(y, 0, errbacks)) { // x = 0, y > 0
        return Roughnum.makeInstance(Math.PI/2, errbacks);
      } else { // x = 0, y < 0
        return Roughnum.makeInstance(3*Math.PI/2, errbacks);
      }
    } else if (greaterThan(x, 0, errbacks)) { // x > 0
      if (greaterThanOrEqual(y, 0, errbacks)) { // x > 0, y >= 0, 1st qdt
        // atan(y/x) is already in the right qdt
        return atan(divide(y, x, errbacks), errbacks);
      } else { // x > 0, y < 0, 4th qdt
        // atan(y/x) is the 4th qdt and negative, so make it positive by adding 2pi
        return add(atan(divide(y, x, errbacks), errbacks), 2*Math.PI, errbacks);
      }
    } else { // x < 0
      // either x < 0, y >= 0 (2nd qdt), in which case
      //        atan(y/x) must be reflected from 4th to 2nd qdt, by adding pi
      //     or x < 0, y < 0  (3rd qdt), in which case
      //        atan(y/x) must be reflected from 1st to 3rd qdt, again by adding pi
      return add(atan(divide(y, x, errbacks), errbacks), Math.PI, errbacks);
    }
  };

  // cos: pyretnum -> pyretnum
  var cos = function(n, errbacks) {
    if (eqv(n, 0, errbacks)) { return 1; }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.cos(n), errbacks);
    }
    return n.cos(errbacks);
  };

  // sin: pyretnum -> pyretnum
  var sin = function(n, errbacks) {
    if (eqv(n, 0, errbacks)) { return 0; }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.sin(n), errbacks);
    }
    return n.sin(errbacks);
  };

  // acos: pyretnum -> pyretnum
  var acos = function(n, errbacks) {
    if (eqv(n, 1, errbacks)) { return 0; }
    if (lessThan(n, -1, errbacks) || greaterThan(n, 1, errbacks)) {
      errbacks.throwDomainError('acos: out of domain argument ' + n);
    }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.acos(n), errbacks);
    }
    return n.acos(errbacks);
  };

  // asin: pyretnum -> pyretnum
  var asin = function(n, errbacks) {
    if (eqv(n, 0, errbacks)) { return 0; }
    if (lessThan(n, -1, errbacks) || greaterThan(n, 1, errbacks)) {
      errbacks.throwDomainError('asin: out of domain argument ' + n);
    }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.asin(n), errbacks);
    }
    return n.asin(errbacks);
  };

  // sqr: pyretnum -> pyretnum
  var sqr = function(x, errbacks) {
    return multiply(x, x, errbacks);
  };

  // integerSqrt: pyretnum -> pyretnum
  var integerSqrt = function(x, errbacks) {
    if (! isInteger(x)) {
      errbacks.throwDomainError('integer-sqrt: the argument ' + x.toString() +
                        " is not an integer.", x);
    }
    if (typeof (x) === 'number') {
      if(x < 0) {
        errbacks.throwSqrtNegative('integerSqrt of negative number', x);
      } else {
        return Math.floor(Math.sqrt(x));
      }
    }
    return x.integerSqrt(errbacks);
  };

  // gcd: pyretnum [pyretnum ...] -> pyretnum
  var gcd = function(first, rest, errbacks) {
    if (! isInteger(first)) {
      errbacks.throwDomainError('gcd: the argument ' + first.toString() +
                                " is not an integer.", first);
    }
    var a = abs(first, errbacks), t, b;
    for(var i = 0; i < rest.length; i++) {
      b = abs(rest[i], errbacks);
      if (! isInteger(b)) {
        errbacks.throwDomainError('gcd: the argument ' + b.toString() +
                                  " is not an integer.", b);
      }
      while (! _integerIsZero(b)) {
        t = a;
        a = b;
        b = _integerModulo(t, b);
      }
    }
    return a;
  };

  // lcm: pyretnum [pyretnum ...] -> pyretnum
  var lcm = function(first, rest, errbacks) {
    if (! isInteger(first)) {
      errbacks.throwDomainError('lcm: the argument ' + first.toString() +
                                " is not an integer.", first);
    }
    var result = abs(first, errbacks);
    if (_integerIsZero(result)) { return 0; }
    for (var i = 0; i < rest.length; i++) {
      if (! isInteger(rest[i])) {
        errbacks.throwDomainError('lcm: the argument ' + rest[i].toString() +
                                  " is not an integer.", rest[i]);
      }
      var divisor = _integerGcd(result, rest[i]);
      if (_integerIsZero(divisor)) {
        return 0;
      }
      result = divide(multiply(result, rest[i], errbacks), divisor, errbacks);
    }
    return result;
  };

  var quotient = function(x, y, errbacks) {
    if (! isInteger(x)) {
      errbacks.throwDomainError('quotient: the first argument ' + x.toString() +
                                " is not an integer.", x);
    }
    if (! isInteger(y)) {
      errbacks.throwDomainError('quotient: the second argument ' + y.toString() +
                                " is not an integer.", y);
    }
    return _integerQuotient(x, y);
  };

  var remainder = function(x, y, errbacks) {
    if (isInteger(x) && isInteger(y)) {
      return _integerRemainder(x, y);
    } else if (isRational(x) && isRational(y)) {
      var xn = numerator(x); var xd = denominator(x);
      var yn = numerator(y); var yd = denominator(y);
      var new_d = lcm(xd, [yd], errbacks);
      var new_xn = multiply(xn, divide(new_d, xd, errbacks), errbacks);
      var new_yn = multiply(yn, divide(new_d, yd, errbacks), errbacks);
      return divide(remainder(new_xn, new_yn, errbacks), new_d, errbacks);
    } else {
      var res = toFixnum(x) % toFixnum(y);
      return Roughnum.makeInstance(res, errbacks);
    }
  };

  //////////////////////////////////////////////////////////////////////

  // Helpers

  // isOverflow: javascript-number -> boolean
  // Returns true if we consider the number an overflow.
  var MIN_FIXNUM = -(9e15);
  var MAX_FIXNUM = (9e15);
  var isOverflow = function(n) {
    return (n < MIN_FIXNUM ||  MAX_FIXNUM < n);
  };

  // negate: pyretnum -> pyretnum
  // multiplies a number times -1.
  var negate = function(n, errbacks) {
    if (typeof(n) === 'number') {
      return -n;
    }
    return n.negate(errbacks);
  };

  // halve: pyretnum -> pyretnum
  // Divide a number by 2.
  var halve = function(n, errbacks) {
    return divide(n, 2, errbacks);
  };

  // fastExpt: computes n^k by squaring.
  // n^k = (n^2)^(k/2)
  // Assumes k is non-negative integer.
  var fastExpt = function(n, k, errbacks) {
    var acc = 1;
    while (true) {
      if (_integerIsZero(k)) {
        return acc;
      }
      if (equals(modulo(k, 2, errbacks), 0, errbacks)) {
        n = multiply(n, n, errbacks);
        k = divide(k, 2, errbacks);
      } else {
        acc = multiply(acc, n, errbacks);
        k = subtract(k, 1, errbacks);
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
          onFixnums(toFixnum(m), toFixnum(n)), errbacks);
      }
      if (typeof(m) === 'number') {
        m = makeBignum(m);
      }
      if (typeof(n) === 'number') {
        n = makeBignum(n);
      }
      return onBignums(m, n);
    });
  };

  var makeIntegerUnOp = function(onFixnums, onBignums, options, errbacks) {
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
        return Roughnum.makeInstance(onFixnums(toFixnum(m)), errbacks);
      }
      if (typeof(m) === 'number') {
        m = makeBignum(m);
      }
      return onBignums(m);
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

  // _integerDivideToFixnum: integer-pyretnum integer-pyretnum -> fixnum
  //
  // _integerDivideToFixnum takes two integers (possibly BigIntegers) and 
  //   returns the best fixnum representing their quotient.
  // If the args are both JS-doubles, the JS quotient is returned if it
  //   doesn't overflow.
  // If it does overflow, or if at least one of the args is a BigInt, then
  //   splitIntIntoMantissaExpt is used to convert the args to
  //   [mantissa, exponent] form. The result a*10^b, where
  //   a = the mantissae's quotient, and
  //   b = the exponents' difference
  //
  var _integerDivideToFixnum = makeIntegerBinop(
    function(m, n) {
      return m / n;
    },
    function(m, n) {
      var xm = splitIntIntoMantissaExpt(m);
      var xn = splitIntIntoMantissaExpt(n);
      var r = Number(String(xm[0] / xn[0]) + 'e' + 
        String(xm[1] - xn[1]));
      return r;
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

  Rational.makeInstance = function(n, d, errbacks) {
    if (n === undefined)
      errbacks.throwUndefinedValue("n undefined", n, d);

    if (d === undefined) { d = 1; }

    if (_integerLessThan(d, 0)) {
      n = negate(n, errbacks);
      d = negate(d, errbacks);
    }

    var divisor = _integerGcd(abs(n, errbacks), abs(d, errbacks));
    n = _integerQuotient(n, divisor);
    d = _integerQuotient(d, divisor);

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

  Rational.prototype.equals = function(other, errbacks) {
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

  Rational.prototype.add = function(other, errbacks) {
    return Rational.makeInstance(_integerAdd(_integerMultiply(this.n, other.d),
                                             _integerMultiply(this.d, other.n)),
                                 _integerMultiply(this.d, other.d), errbacks);
  };

  Rational.prototype.subtract = function(other, errbacks) {
    return Rational.makeInstance(_integerSubtract(_integerMultiply(this.n, other.d),
                                                  _integerMultiply(this.d, other.n)),
                                 _integerMultiply(this.d, other.d), errbacks);
  };

  Rational.prototype.negate = function(errbacks) {
    return Rational.makeInstance(negate(this.n, errbacks), this.d, errbacks)
  };

  Rational.prototype.multiply = function(other, errbacks) {
    return Rational.makeInstance(_integerMultiply(this.n, other.n),
                                 _integerMultiply(this.d, other.d), errbacks);
  };

  Rational.prototype.divide = function(other, errbacks) {
    if (_integerIsZero(this.d) || _integerIsZero(other.n)) {  // dead code!
      errbacks.throwDivByZero("/: division by zero", this, other);
    }
    return Rational.makeInstance(_integerMultiply(this.n, other.d),
                                 _integerMultiply(this.d, other.n), errbacks);
  };

  Rational.prototype.toRational = function() {
    return this;
  };

  Rational.prototype.toExact = Rational.prototype.toRational;


  Rational.prototype.toFixnum = function() {
    return _integerDivideToFixnum(this.n, this.d);
  };

  Rational.prototype.toRoughnum = function(errbacks) {
    return Roughnum.makeInstance(this.toFixnum(), errbacks);
  };

  Rational.prototype.numerator = function() {
    return this.n;
  };

  Rational.prototype.denominator = function() {
    return this.d;
  };

  Rational.prototype.greaterThan = function(other, errbacks) {
    return _integerGreaterThan(_integerMultiply(this.n, other.d),
                               _integerMultiply(this.d, other.n));
  };

  Rational.prototype.greaterThanOrEqual = function(other, errbacks) {
    return _integerGreaterThanOrEqual(_integerMultiply(this.n, other.d),
                                      _integerMultiply(this.d, other.n));
  };

  Rational.prototype.lessThan = function(other, errbacks) {
    return _integerLessThan(_integerMultiply(this.n, other.d),
                            _integerMultiply(this.d, other.n));
  };

  Rational.prototype.lessThanOrEqual = function(other, errbacks) {
    return _integerLessThanOrEqual(_integerMultiply(this.n, other.d),
                                   _integerMultiply(this.d, other.n));
  };

  Rational.prototype.integerSqrt = function(errbacks) {
    var result = sqrt(this);
    return toRational(floor(result, errbacks), errbacks);
  };

  Rational.prototype.sqrt = function(errbacks) {
    var newN = sqrt(this.n);
    var newD = sqrt(this.d);
    if (isRational(newN) && isRational(newD) &&
        equals(floor(newN), newN) &&
        equals(floor(newD), newD)) {
      return Rational.makeInstance(newN, newD, errbacks);
    } else {
      return divide(newN, newD, errbacks);
    }
  };

  Rational.prototype.abs = function(errbacks) {
    return Rational.makeInstance(abs(this.n, errbacks),
                                 this.d, errbacks);
  };

  Rational.prototype.floor = function(errbacks) {
    var quotient = _integerQuotient(this.n, this.d);
    if (_integerLessThan(this.n, 0)) {
      return subtract(quotient, 1, errbacks);
    } else {
      return quotient;
    }
  };

  Rational.prototype.ceiling = function(errbacks) {
    var quotient = _integerQuotient(this.n, this.d);
    if (_integerLessThan(this.n, 0)) {
      return quotient;
    } else {
      return add(quotient, 1, errbacks);
    }
  };

  Rational.prototype.round = function(errbacks) {
    var halfintp = equals(this.d, 2);
    var negativep = _integerLessThan(this.n, 0);
    var n = this.n;
    if (negativep) {
      n = negate(n, errbacks);
    }
    var quo = _integerQuotient(n, this.d);
    if (halfintp) {
      // rounding half to away from 0
      // uncomment following if rounding half to even
      // if (_integerIsOne(_integerModulo(quo, 2)))
      quo = add(quo, 1, errbacks);
    } else {
      var rem = _integerRemainder(n, this.d);
      if (greaterThan(multiply(rem, 2, errbacks), this.d, errbacks)) {
        quo = add(quo, 1, errbacks);
      }
    }
    if (negativep) {
      quo = negate(quo, errbacks);
    }
    return quo;
  };

  Rational.prototype.roundEven = function(errbacks) {
    // rounds half-integers to even
    var halfintp = equals(this.d, 2, errbacks);
    var negativep = _integerLessThan(this.n, 0);
    var n = this.n;
    if (negativep) n = negate(n, errbacks);
    var quo = _integerQuotient(n, this.d);
    if (halfintp) {
      if (_integerIsOne(_integerModulo(quo, 2)))
        quo = add(quo, 1, errbacks);
    } else {
      var rem = _integerRemainder(n, this.d);
      if (greaterThan(multiply(rem, 2, errbacks), this.d, errbacks))
        quo = add(quo, 1, errbacks);
    }
    if (negativep) quo = negate(quo, errbacks);
    return quo;
  };

  Rational.prototype.log = function(errbacks){
    return Roughnum.makeInstance(Math.log(this.toFixnum()), errbacks);
  };

  Rational.prototype.tan = function(errbacks){
    return Roughnum.makeInstance(Math.tan(this.toFixnum()), errbacks);
  };

  Rational.prototype.atan = function(errbacks){
    return Roughnum.makeInstance(Math.atan(this.toFixnum()), errbacks);
  };

  Rational.prototype.cos = function(errbacks){
    return Roughnum.makeInstance(Math.cos(this.toFixnum()), errbacks);
  };

  Rational.prototype.sin = function(errbacks){
    return Roughnum.makeInstance(Math.sin(this.toFixnum()), errbacks);
  };

  var integerNthRoot = function(n, m, errbacks) {
    var guessPrev, guessToTheN;
    var guess = m;

    // find closest integral zero of x^n - m = 0 using Newton-Raphson.
    // if k'th guess is x_k, then
    // x_{k+1} = floor( x_k - [(x_k)^n - m]/[n (x_k)^(n-1)] ).
    // Stop iteration if (x_k)^n is close enough to m, or
    // if x_k stops evolving

    while(true) {
      guessToTheN = expt(guess, n, errbacks);
      if (lessThanOrEqual(guessToTheN, m, errbacks) &&
          lessThan(m, expt(add(guess, 1, errbacks), n, errbacks), errbacks)) break;
      guessPrev = guess;
      guess = floor(subtract(guess, divide(subtract(guessToTheN, m, errbacks),
            multiply(n, divide(guessToTheN, guess, errbacks), errbacks), errbacks), errbacks), errbacks);
      if (equals(guess, guessPrev, errbacks)) break;
    }

    return guess;
  };

  var nthRoot = function(n, m, errbacks) {
    var mNeg = (sign(m) < 0);
    var mAbs = (mNeg ? abs(m, errbacks) : m);
    var approx;

    if (mNeg && _integerModulo(n, 2) === 0)
      errbacks.throwDomainError('expt: taking even (' + n + ') root of negative integer ' + m);

    approx = integerNthRoot(n, mAbs, errbacks);
    if (mNeg) approx = negate(approx, errbacks);
    if (eqv(expt(approx, n, errbacks), m, errbacks)) return approx;

    approx = Roughnum.makeInstance(Math.pow(toFixnum(mAbs),
                                            toFixnum(divide(1,n, errbacks))), errbacks);
    return (mNeg ? negate(approx, errbacks) : approx);
  };

  Rational.prototype.expt = function(a, errbacks) {
    if (isInteger(a) && greaterThanOrEqual(a, 0, errbacks)) {
      return fastExpt(this, a, errbacks);
    } else if (_integerLessThanOrEqual(a.d, 8)) {
      var nRaisedToAn = expt(this.n, a.n, errbacks);
      var dRaisedToAn = expt(this.d, a.n, errbacks);
      var newN = nthRoot(a.d, nRaisedToAn, errbacks);
      var newD = nthRoot(a.d, dRaisedToAn, errbacks);
      if (isRational(newN) && isRational(newD) &&
          equals(floor(newN), newN, errbacks) &&
          equals(floor(newD), newD, errbacks)) {
        return Rational.makeInstance(newN, newD, errbacks);
      } else {
        return divide(newN, newD, errbacks);
     }
    } else {
      if (this.isNegative() && !a.isInteger())
        errbacks.throwDomainError('expt: raising negative number ' + this + ' to nonintegral power ' + a);
      return Roughnum.makeInstance(Math.pow(this.toFixnum(), a.toFixnum()), errbacks);
    }
  };

  Rational.prototype.exp = function(errbacks){
    var res = Math.exp(this.toFixnum());
    if (!isFinite(res))
      errbacks.throwDomainError('exp: argument too large: ' + this);
    return Roughnum.makeInstance(res, errbacks);
  };

  Rational.prototype.acos = function(errbacks){
    return acos(this.toFixnum(), errbacks);
  };

  Rational.prototype.asin = function(errbacks){
    return asin(this.toFixnum(), errbacks);
  };

  // sign: Number -> {-1, 0, 1}
  var sign = function(n, errbacks) {
    if (lessThan(n, 0, errbacks)) {
      return -1;
    } else if (greaterThan(n, 0, errbacks)) {
      return 1;
    } else {
      return 0;
    }
  };

  // Roughnums

  var Roughnum = function(n, errbacks) {
    if (!(typeof(n) === 'number'))
      errbacks.throwGeneralError('roughnum constructor got unsuitable arg ' + n);
    this.n = n;
  };

  Roughnum.makeInstance = function(n, errbacks) {
    if (typeof(n) === 'number' && !isFinite(n)) {
      errbacks.throwDomainError('roughnum overflow error');
    }
    return new Roughnum(n, errbacks);
  };

  Roughnum.prototype.isFinite = function() {
    //actually always true, as we don't store overflows
    return (isFinite(this.n));
  };

  Roughnum.prototype.toRational = function(errbacks) {
    if (!isFinite(this.n)) {
      // this _should_ be dead, as we don't store overflows
      errbacks.throwInternalError("toRational: no exact representation for " + this);
    }

    return fromString(this.n.toString(), errbacks);
  };

  Roughnum.prototype.toExact = Roughnum.prototype.toRational;

  Roughnum.prototype.toString = function() {
    return '~' + this.n.toString();
  };

  Roughnum.prototype.equals = function(other, errbacks) {
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

  Roughnum.prototype.add = function(other, errbacks) {
    return Roughnum.makeInstance(this.n + other.n, errbacks);
  };

  Roughnum.prototype.subtract = function(other, errbacks) {
    return Roughnum.makeInstance(this.n - other.n, errbacks);
  };

  Roughnum.prototype.negate = function(errbacks) {
    return Roughnum.makeInstance(-this.n, errbacks);
  };

  Roughnum.prototype.multiply = function(other, errbacks) {
    return Roughnum.makeInstance(this.n * other.n, errbacks);
  };

  Roughnum.prototype.divide = function(other, errbacks) {
    return Roughnum.makeInstance(this.n / other.n, errbacks);
  };

  Roughnum.prototype.toFixnum = function() {
    return this.n;
  };

  Roughnum.prototype.toRoughnum = function(errbacks) {
    return this;
  };

  Roughnum.prototype.numerator = function() {
    var stringRep = this.n.toString();
    var match = stringRep.match(/^(.*)\.(.*)$/);
    if (match) {
      var afterDecimal = parseInt(match[2]);
      var factorToInt = Math.pow(10, match[2].length);
      var extraFactor = _integerGcd(factorToInt, afterDecimal);
      var multFactor = factorToInt / extraFactor;
      return Roughnum.makeInstance( Math.round(this.n * multFactor) );
    } else {
      return this;
    }
  };

  Roughnum.prototype.denominator = function() {
    var stringRep = this.n.toString();
    var match = stringRep.match(/^(.*)\.(.*)$/);
    if (match) {
      var afterDecimal = parseInt(match[2]);
      var factorToInt = Math.pow(10, match[2].length);
      var extraFactor = _integerGcd(factorToInt, afterDecimal);
      return Roughnum.makeInstance( Math.round(factorToInt/extraFactor) );
    } else {
      return Roughnum.makeInstance(1);
    }
  };

  Roughnum.prototype.floor = function(errbacks) {
    return Math.floor(this.n);
  };

  Roughnum.prototype.ceiling = function(errbacks) {
    return Math.ceil(this.n);
  };

  Roughnum.prototype.round = function(errbacks){
    var negativep = (this.n < 0);
    var n = this.n;
    if (negativep) n = -n;
    var res = Math.round(n);
    if (negativep) res = -res;
    return res;
  };

  Roughnum.prototype.roundEven = function(errbacks) {
    var negativep = (this.n < 0);
    var n = this.n;
    if (negativep) n = -n;
    var res = Math.round(n);
    if ((Math.abs(n - res) === 0.5) && (res % 2 === 1))
      res -= 1;
    return res;
  };

  Roughnum.prototype.greaterThan = function(other, errbacks) {
    return this.n > other.n;
  };

  Roughnum.prototype.greaterThanOrEqual = function(other, errbacks) {
    return this.n >= other.n;
  };

  Roughnum.prototype.lessThan = function(other, errbacks) {
    return this.n < other.n;
  };

  Roughnum.prototype.lessThanOrEqual = function(other, errbacks) {
    return this.n <= other.n;
  };

  Roughnum.prototype.integerSqrt = function(errbacks) {
    if (isInteger(this)) {
      if(this.n >= 0) {
        return Roughnum.makeInstance(Math.floor(Math.sqrt(this.n)), errbacks);
      } else {
        errbacks.throwDomainError('integerSqrt of negative roughnum', this.n);
      }
    } else {
      errbacks.throwDomainError("integerSqrt: can only be applied to an integer", this);
    }
  };

  Roughnum.prototype.sqrt = function(errbacks) {
    return Roughnum.makeInstance(Math.sqrt(this.n), errbacks);
  };

  Roughnum.prototype.abs = function(errbacks) {
    return Roughnum.makeInstance(Math.abs(this.n), errbacks);
  };

  Roughnum.prototype.log = function(errbacks){
    if (this.n < 0)
      errbacks.throwDomainError('log of negative roughnum', this.n);
    else
      return Roughnum.makeInstance(Math.log(this.n), errbacks);
  };

  Roughnum.prototype.tan = function(errbacks){
    return Roughnum.makeInstance(Math.tan(this.n), errbacks);
  };

  Roughnum.prototype.atan = function(errbacks){
    return Roughnum.makeInstance(Math.atan(this.n), errbacks);
  };

  Roughnum.prototype.cos = function(errbacks){
    return Roughnum.makeInstance(Math.cos(this.n), errbacks);
  };

  Roughnum.prototype.sin = function(errbacks){
    return Roughnum.makeInstance(Math.sin(this.n), errbacks);
  };

  Roughnum.prototype.expt = function(a, errbacks){
    if (this.n === 1) {
      return this;
    } else {
      return Roughnum.makeInstance(Math.pow(this.n, a.n), errbacks);
    }
  };

  Roughnum.prototype.exp = function(errbacks){
    var res = Math.exp(this.n);
    if (!isFinite(res))
      errbacks.throwDomainError('exp: argument too large: ' + this);
    return Roughnum.makeInstance(res);
  };

  Roughnum.prototype.acos = function(errbacks){
    return acos(this.n, errbacks);
  };

  Roughnum.prototype.asin = function(errbacks){
    return asin(this.n, errbacks);
  };

  var rationalRegexp = new RegExp("^([+-]?\\d+)/(\\d+)$");
  var digitRegexp = new RegExp("^[+-]?\\d+$");
  var flonumRegexp = new RegExp("^([-+]?)(\\d+\)((?:\\.\\d+)?)((?:[Ee][-+]?\\d+)?)$");


  var roughnumDecRegexp = new RegExp("^~([-+]?\\d+(?:\\.\\d+)?(?:[Ee][-+]?\\d+)?)$");

  var roughnumRatRegexp = new RegExp("^~([+-]?\\d+)/(\\d+)$");

  var scientificPattern = new RegExp("^([+-]?\\d*\\.?\\d*)[Ee]([+]?\\d+)$");

  // fromString: string -> (pyretnum | false)
  var fromString = function(x, errbacks) {
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
      return Rational.makeInstance(fromString(aMatch[1]),
                                   fromString(aMatch[2]), errbacks);
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
      var finalNum = _integerAdd(_integerMultiply(beforeDecimal, denominatorTen), afterDecimal);
      if (negativeP) {
        finalNum = negate(finalNum, errbacks);
      }
      //
      if (!equals(exponent, 1)) {
        if (exponentNegativeP) {
          finalDen = _integerMultiply(finalDen, exponent);
        } else {
          finalNum = _integerMultiply(finalNum, exponent);
        }
      }
      return Rational.makeInstance(finalNum, finalDen, errbacks);
    }

    aMatch = x.match(roughnumRatRegexp);
    if (aMatch) {
      return toRoughnum(Rational.makeInstance(fromString(aMatch[1]), fromString(aMatch[2])),
        errbacks);
    }

    aMatch = x.match(roughnumDecRegexp);
    if (aMatch) {
      return Roughnum.makeInstance(Number(aMatch[1]), errbacks);
    }

    return false; // if all else fails

  };

  ///////////////////////////////////////////////////////////

  // recognizing numbers in (We)Scheme syntax:

    var hashModifiersRegexp = new RegExp("^(#[ei]#[bodx]|#[bodx]#[ei]|#[bodxei])(.*)$")

    function schemeRationalRegexp(digits) { return new RegExp("^([+-]?["+digits+"]+)/(["+digits+"]+)$"); }

    function matchComplexRegexp(radix, x, errbacks) {
	var sign = "[+-]";
	var maybeSign = "[+-]?";
	var digits = digitsForRadix(radix, errbacks)
	var expmark = "["+expMarkForRadix(radix, errbacks)+"]"
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

    function digitsForRadix(radix, errbacks) {
	return radix === 2  ? "01" :
	       radix === 8  ? "0-7" :
	       radix === 10 ? "0-9" :
	       radix === 16 ? "0-9a-fA-F" :
	       errbacks.throwInternalError("digitsForRadix: invalid radix", this, radix)
    }
    function expMarkForRadix(radix, errbacks) {
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
    var fromSchemeString = function(x, exactness, errbacks) {

	var radix = 10
	var exactness = typeof exactness === 'undefined' ? Exactness.def :
			exactness === true               ? Exactness.on :
			exactness === false              ? Exactness.off :
	   /* else */  errbacks.throwInternalError( "exactness must be true or false"
                                                   , this
                                                   , r) ;

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
			 errbacks.throwInternalError("invalid exactness flag", this, r)
	    }
	    if (radixFlag) {
		var f = radixFlag[1].charAt(1)
		radix = f === 'b' ? 2 :
            f === 'o' ? 8 :
            f === 'd' ? 10 :
            f === 'x' ? 16 :
			 // this case is unreachable
			errbacks.throwInternalError("invalid radix flag", this, r)
	    }
	}

	var numberString = hMatch ? hMatch[2] : x
	// if the string begins with a hash modifier, then it must parse as a
	// number, an invalid parse is an error, not false. False is returned
	// when the item could potentially have been read as a symbol.
	var mustBeANumberp = hMatch ? true : false

	return fromSchemeStringRaw(numberString, radix, exactness, mustBeANumberp, errbacks)
    };

    function fromSchemeStringRaw(x, radix, exactness, mustBeANumberp, errbacks) {
	var cMatch = matchComplexRegexp(radix, x, errbacks);
	if (cMatch) {
          throw "Complex Numbers are not supported in Pyret";
	}

        return fromSchemeStringRawNoComplex(x, radix, exactness, mustBeANumberp, errbacks)
    }

    function fromSchemeStringRawNoComplex(x, radix, exactness, mustBeANumberp, errbacks) {
	var aMatch = x.match(schemeRationalRegexp(digitsForRadix(radix, errbacks)));
	if (aMatch) {
	  return Rational.makeInstance( fromSchemeStringRawNoComplex( aMatch[1]
                                                                      , radix
                                                                      , exactness
                                                                      , errbacks
                                                                    )
                                        , fromSchemeStringRawNoComplex( aMatch[2]
                                                                        , radix
                                                                        , exactness
                                                                        , errbacks
                                                                      )
                                        , errbacks);
	}

        if (x === '+nan.0' ||
            x === '-nan.0' ||
            x === '+inf.0' ||
            x === '-inf.0' ||
            x === '-0.0') {
          return Roughnum.makeInstance(Infinity);
        }

	var fMatch = x.match(schemeFlonumRegexp(digitsForRadix(radix, errbacks)))
	if (fMatch) {
	    var integralPart = fMatch[3] !== undefined ? fMatch[3] : fMatch[5];
	    var fractionalPart = fMatch[4] !== undefined ? fMatch[4] : fMatch[6];
	    return parseFloat( fMatch[1]
                               , integralPart
                               , fractionalPart
                               , radix
                               , exactness
                               , errbacks
                             )
	}

	var sMatch = x.match(schemeScientificPattern( digitsForRadix(radix, errbacks)
					      , expMarkForRadix(radix, errbacks)
					      ))
	if (sMatch) {
	    var coefficient = fromSchemeStringRawNoComplex(sMatch[1], radix, exactness, errbacks)
	    var exponent = fromSchemeStringRawNoComplex(sMatch[2], radix, exactness, errbacks)
	    return multiply(coefficient, expt(radix, exponent, errbacks), errbacks);
	}

	// Finally, integer tests.
	if (x.match(schemeDigitRegexp(digitsForRadix(radix, errbacks)))) {
	    var n = parseInt(x, radix);
	    if (isOverflow(n)) {
		return makeBignum(x);
	    } else if (exactness.intAsExactp()) {
		return n;
	    } else {
		return Roughnum.makeInstance(n)
	    }
	} else if (mustBeANumberp) {
	    if(x.length===0) errbacks.throwGeneralError("no digits");
	    errbacks.throwGeneralError("bad number: " + x, this);
	} else {
	    return false;
	}
    };

    function parseFloat(sign, integralPart, fractionalPart, radix, exactness, errbacks) {
	var sign = (sign == "-" ? -1 : 1);
	var integralPartValue = integralPart === ""  ? 0  :
				exactness.intAsExactp() ? parseExactInt(integralPart, radix, errbacks) :
							  parseInt(integralPart, radix)

	var fractionalNumerator = fractionalPart === "" ? 0 :
				  exactness.intAsExactp() ? parseExactInt(fractionalPart, radix, errbacks) :
							    parseInt(fractionalPart, radix)
	/* unfortunately, for these next two calculations, `expt` and `divide` */
	/* will promote to Bignum and Rational, respectively, but we only want */
	/* these if we're parsing in exact mode */
	var fractionalDenominator = exactness.intAsExactp() ? expt(radix, fractionalPart.length, errbacks) :
							      Math.pow(radix, fractionalPart.length)
	var fractionalPartValue = fractionalPart === "" ? 0 :
				  exactness.intAsExactp() ? divide(fractionalNumerator, fractionalDenominator, errbacks) :
							    fractionalNumerator / fractionalDenominator

	var forceInexact = function(o) {
	    return typeof o === "number" ? Roughnum.makeInstance(o, errbacks) :
					   o.toRoughnum(errbacks);
	}

	return exactness.floatAsInexactp() ? forceInexact(multiply(sign, add( integralPartValue, fractionalPartValue))) :
					     multiply(sign, add(integralPartValue, fractionalPartValue));
    }

    function parseExactInt(str, radix, errbacks) {
	return fromSchemeStringRawNoComplex(str, radix, Exactness.on, true, errbacks);
    }

  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////

  // The code below comes from Tom Wu's BigInteger implementation:

  // Copyright (c) 2005  Tom Wu
  // All Rights Reserved.
  // See "LICENSE" for details.

  // Basic JavaScript BN library - subset useful for RSA encryption.

  // Bits per digit
  var dbits;

  // JavaScript engine analysis
  var canary = 0xdeadbeefcafe;
  var j_lm = ((canary&0xffffff)==0xefcafe);

  // (public) Constructor
  function BigInteger(a,b,c) {
    if(a != null)
      if("number" == typeof a) this.fromNumber(a,b,c);
    else if(b == null && "string" != typeof a) this.fromString(a,256);
    else this.fromString(a,b);
  }

  // return new, unset BigInteger
  function nbi() { return new BigInteger(null); }

  // am: Compute w_j += (x*this_i), propagate carries,
  // c is initial carry, returns final carry.
  // c < 3*dvalue, x < 2*dvalue, this_i < dvalue
  // We need to select the fastest one that works in this environment.

  // am1: use a single mult and divide to get the high bits,
  // max digit bits should be 26 because
  // max internal value = 2*dvalue^2-2*dvalue (< 2^53)
  function am1(i,x,w,j,c,n) {
    while(--n >= 0) {
      var v = x*this[i++]+w[j]+c;
      c = Math.floor(v/0x4000000);
      w[j++] = v&0x3ffffff;
    }
    return c;
  }
  // am2 avoids a big mult-and-extract completely.
  // Max digit bits should be <= 30 because we do bitwise ops
  // on values up to 2*hdvalue^2-hdvalue-1 (< 2^31)
  function am2(i,x,w,j,c,n) {
    var xl = x&0x7fff, xh = x>>15;
    while(--n >= 0) {
      var l = this[i]&0x7fff;
      var h = this[i++]>>15;
      var m = xh*l+h*xl;
      l = xl*l+((m&0x7fff)<<15)+w[j]+(c&0x3fffffff);
      c = (l>>>30)+(m>>>15)+xh*h+(c>>>30);
      w[j++] = l&0x3fffffff;
    }
    return c;
  }
  // Alternately, set max digit bits to 28 since some
  // browsers slow down when dealing with 32-bit numbers.
  function am3(i,x,w,j,c,n) {
    var xl = x&0x3fff, xh = x>>14;
    while(--n >= 0) {
      var l = this[i]&0x3fff;
      var h = this[i++]>>14;
      var m = xh*l+h*xl;
      l = xl*l+((m&0x3fff)<<14)+w[j]+c;
      c = (l>>28)+(m>>14)+xh*h;
      w[j++] = l&0xfffffff;
    }
    return c;
  }
  if(j_lm && (typeof(navigator) !== 'undefined' && navigator.appName == "Microsoft Internet Explorer")) {
    BigInteger.prototype.am = am2;
    dbits = 30;
  }
  else if(j_lm && (typeof(navigator) !== 'undefined' && navigator.appName != "Netscape")) {
    BigInteger.prototype.am = am1;
    dbits = 26;
  }
  else { // Mozilla/Netscape seems to prefer am3
    BigInteger.prototype.am = am3;
    dbits = 28;
  }

  BigInteger.prototype.DB = dbits;
  BigInteger.prototype.DM = ((1<<dbits)-1);
  BigInteger.prototype.DV = (1<<dbits);

  var BI_FP = 52;
  BigInteger.prototype.FV = Math.pow(2,BI_FP);
  BigInteger.prototype.F1 = BI_FP-dbits;
  BigInteger.prototype.F2 = 2*dbits-BI_FP;

  // Digit conversions
  var BI_RM = "0123456789abcdefghijklmnopqrstuvwxyz";
  var BI_RC = [];
  var rr,vv;
  rr = "0".charCodeAt(0);
  for(vv = 0; vv <= 9; ++vv) BI_RC[rr++] = vv;
  rr = "a".charCodeAt(0);
  for(vv = 10; vv < 36; ++vv) BI_RC[rr++] = vv;
  rr = "A".charCodeAt(0);
  for(vv = 10; vv < 36; ++vv) BI_RC[rr++] = vv;

  function int2char(n) { return BI_RM.charAt(n); }
  function intAt(s,i) {
    var c = BI_RC[s.charCodeAt(i)];
    return (c==null)?-1:c;
  }

  // (protected) copy this to r
  function bnpCopyTo(r) {
    for(var i = this.t-1; i >= 0; --i) r[i] = this[i];
    r.t = this.t;
    r.s = this.s;
  }

  // (protected) set from integer value x, -DV <= x < DV
  function bnpFromInt(x) {
    this.t = 1;
    this.s = (x<0)?-1:0;
    if(x > 0) this[0] = x;
    else if(x < -1) this[0] = x+DV;
    else this.t = 0;
  }

  // return bigint initialized to value
  function nbv(i) { var r = nbi(); r.fromInt(i); return r; }

  // (protected) set from string and radix
  function bnpFromString(s,b) {
    var k;
    if(b == 16) k = 4;
    else if(b == 8) k = 3;
    else if(b == 256) k = 8; // byte array
    else if(b == 2) k = 1;
    else if(b == 32) k = 5;
    else if(b == 4) k = 2;
    else { this.fromRadix(s,b); return; }
    this.t = 0;
    this.s = 0;
    var i = s.length, mi = false, sh = 0;
    while(--i >= 0) {
      var x = (k==8)?s[i]&0xff:intAt(s,i);
      if(x < 0) {
        if(s.charAt(i) == "-") mi = true;
        continue;
      }
      mi = false;
      if(sh == 0)
        this[this.t++] = x;
      else if(sh+k > this.DB) {
        this[this.t-1] |= (x&((1<<(this.DB-sh))-1))<<sh;
        this[this.t++] = (x>>(this.DB-sh));
      }
      else
        this[this.t-1] |= x<<sh;
      sh += k;
      if(sh >= this.DB) sh -= this.DB;
    }
    if(k == 8 && (s[0]&0x80) != 0) {
      this.s = -1;
      if(sh > 0) this[this.t-1] |= ((1<<(this.DB-sh))-1)<<sh;
    }
    this.clamp();
    if(mi) BigInteger.ZERO.subTo(this,this);
  }

  // (protected) clamp off excess high words
  function bnpClamp() {
    var c = this.s&this.DM;
    while(this.t > 0 && this[this.t-1] == c) --this.t;
  }

  // (public) return string representation in given radix
  function bnToString(b) {
    if(this.s < 0) return "-"+this.negate().toString(b);
    var k;
    if(b == 16) k = 4;
    else if(b == 8) k = 3;
    else if(b == 2) k = 1;
    else if(b == 32) k = 5;
    else if(b == 4) k = 2;
    else return this.toRadix(b);
    var km = (1<<k)-1, d, m = false, r = [], i = this.t;
    var p = this.DB-(i*this.DB)%k;
    if(i-- > 0) {
      if(p < this.DB && (d = this[i]>>p) > 0) { m = true; r.push(int2char(d)); }
      while(i >= 0) {
        if(p < k) {
          d = (this[i]&((1<<p)-1))<<(k-p);
          d |= this[--i]>>(p+=this.DB-k);
        }
        else {
          d = (this[i]>>(p-=k))&km;
          if(p <= 0) { p += this.DB; --i; }
        }
        if(d > 0) m = true;
        if(m) r.push(int2char(d));
      }
    }
    return m?r.join(""):"0";
  }

  // (public) -this
  function bnNegate() { var r = nbi(); BigInteger.ZERO.subTo(this,r); return r; }

  // (public) |this|
  function bnAbs() { return (this.s<0)?this.negate():this; }

  // (public) return + if this > a, - if this < a, 0 if equal
  function bnCompareTo(a) {
    var r = this.s-a.s;
    if(r != 0) return r;
    var i = this.t;
    if ( this.s < 0 ) {
      r = a.t - i;
    }
    else {
      r = i - a.t;
    }
    if(r != 0) return r;
    while(--i >= 0) if((r=this[i]-a[i]) != 0) return r;
    return 0;
  }

  // returns bit length of the integer x
  function nbits(x) {
    var r = 1, t;
    if((t=x>>>16) != 0) { x = t; r += 16; }
    if((t=x>>8) != 0) { x = t; r += 8; }
    if((t=x>>4) != 0) { x = t; r += 4; }
    if((t=x>>2) != 0) { x = t; r += 2; }
    if((t=x>>1) != 0) { x = t; r += 1; }
    return r;
  }

  // (public) return the number of bits in "this"
  function bnBitLength() {
    if(this.t <= 0) return 0;
    return this.DB*(this.t-1)+nbits(this[this.t-1]^(this.s&this.DM));
  }

  // (protected) r = this << n*DB
  function bnpDLShiftTo(n,r) {
    var i;
    for(i = this.t-1; i >= 0; --i) r[i+n] = this[i];
    for(i = n-1; i >= 0; --i) r[i] = 0;
    r.t = this.t+n;
    r.s = this.s;
  }

  // (protected) r = this >> n*DB
  function bnpDRShiftTo(n,r) {
    for(var i = n; i < this.t; ++i) r[i-n] = this[i];
    r.t = Math.max(this.t-n,0);
    r.s = this.s;
  }

  // (protected) r = this << n
  function bnpLShiftTo(n,r) {
    var bs = n%this.DB;
    var cbs = this.DB-bs;
    var bm = (1<<cbs)-1;
    var ds = Math.floor(n/this.DB), c = (this.s<<bs)&this.DM, i;
    for(i = this.t-1; i >= 0; --i) {
      r[i+ds+1] = (this[i]>>cbs)|c;
      c = (this[i]&bm)<<bs;
    }
    for(i = ds-1; i >= 0; --i) r[i] = 0;
    r[ds] = c;
    r.t = this.t+ds+1;
    r.s = this.s;
    r.clamp();
  }

  // (protected) r = this >> n
  function bnpRShiftTo(n,r) {
    r.s = this.s;
    var ds = Math.floor(n/this.DB);
    if(ds >= this.t) { r.t = 0; return; }
    var bs = n%this.DB;
    var cbs = this.DB-bs;
    var bm = (1<<bs)-1;
    r[0] = this[ds]>>bs;
    for(var i = ds+1; i < this.t; ++i) {
      r[i-ds-1] |= (this[i]&bm)<<cbs;
      r[i-ds] = this[i]>>bs;
    }
    if(bs > 0) r[this.t-ds-1] |= (this.s&bm)<<cbs;
    r.t = this.t-ds;
    r.clamp();
  }

  // (protected) r = this - a
  function bnpSubTo(a,r) {
    var i = 0, c = 0, m = Math.min(a.t,this.t);
    while(i < m) {
      c += this[i]-a[i];
      r[i++] = c&this.DM;
      c >>= this.DB;
    }
    if(a.t < this.t) {
      c -= a.s;
      while(i < this.t) {
        c += this[i];
        r[i++] = c&this.DM;
        c >>= this.DB;
      }
      c += this.s;
    }
    else {
      c += this.s;
      while(i < a.t) {
        c -= a[i];
        r[i++] = c&this.DM;
        c >>= this.DB;
      }
      c -= a.s;
    }
    r.s = (c<0)?-1:0;
    if(c < -1) r[i++] = this.DV+c;
    else if(c > 0) r[i++] = c;
    r.t = i;
    r.clamp();
  }

  // (protected) r = this * a, r != this,a (HAC 14.12)
  // "this" should be the larger one if appropriate.
  function bnpMultiplyTo(a,r) {
    var x = this.abs(), y = a.abs();
    var i = x.t;
    r.t = i+y.t;
    while(--i >= 0) r[i] = 0;
    for(i = 0; i < y.t; ++i) r[i+x.t] = x.am(0,y[i],r,i,0,x.t);
    r.s = 0;
    r.clamp();
    if(this.s != a.s) BigInteger.ZERO.subTo(r,r);
  }

  // (protected) r = this^2, r != this (HAC 14.16)
  function bnpSquareTo(r) {
    var x = this.abs();
    var i = r.t = 2*x.t;
    while(--i >= 0) r[i] = 0;
    for(i = 0; i < x.t-1; ++i) {
      var c = x.am(i,x[i],r,2*i,0,1);
      if((r[i+x.t]+=x.am(i+1,2*x[i],r,2*i+1,c,x.t-i-1)) >= x.DV) {
        r[i+x.t] -= x.DV;
        r[i+x.t+1] = 1;
      }
    }
    if(r.t > 0) r[r.t-1] += x.am(i,x[i],r,2*i,0,1);
    r.s = 0;
    r.clamp();
  }

  // (protected) divide this by m, quotient and remainder to q, r (HAC 14.20)
  // r != q, this != m.  q or r may be null.
  function bnpDivRemTo(m,q,r) {
    var pm = m.abs();
    if(pm.t <= 0) return;
    var pt = this.abs();
    if(pt.t < pm.t) {
      if(q != null) q.fromInt(0);
      if(r != null) this.copyTo(r);
      return;
    }
    if(r == null) r = nbi();
    var y = nbi(), ts = this.s, ms = m.s;
    var nsh = this.DB-nbits(pm[pm.t-1]);    // normalize modulus
    if(nsh > 0) { pm.lShiftTo(nsh,y); pt.lShiftTo(nsh,r); }
    else { pm.copyTo(y); pt.copyTo(r); }
    var ys = y.t;
    var y0 = y[ys-1];
    if(y0 == 0) return;
    var yt = y0*(1<<this.F1)+((ys>1)?y[ys-2]>>this.F2:0);
    var d1 = this.FV/yt, d2 = (1<<this.F1)/yt, e = 1<<this.F2;
    var i = r.t, j = i-ys, t = (q==null)?nbi():q;
    y.dlShiftTo(j,t);
    if(r.compareTo(t) >= 0) {
      r[r.t++] = 1;
      r.subTo(t,r);
    }
    BigInteger.ONE.dlShiftTo(ys,t);
    t.subTo(y,y);   // "negative" y so we can replace sub with am later
    while(y.t < ys) y[y.t++] = 0;
    while(--j >= 0) {
      // Estimate quotient digit
      var qd = (r[--i]==y0)?this.DM:Math.floor(r[i]*d1+(r[i-1]+e)*d2);
      if((r[i]+=y.am(0,qd,r,j,0,ys)) < qd) {    // Try it out
        y.dlShiftTo(j,t);
        r.subTo(t,r);
        while(r[i] < --qd) r.subTo(t,r);
      }
    }
    if(q != null) {
      r.drShiftTo(ys,q);
      if(ts != ms) BigInteger.ZERO.subTo(q,q);
    }
    r.t = ys;
    r.clamp();
    if(nsh > 0) r.rShiftTo(nsh,r);  // Denormalize remainder
    if(ts < 0) BigInteger.ZERO.subTo(r,r);
  }

  // (public) this mod a
  function bnMod(a) {
    var r = nbi();
    this.abs().divRemTo(a,null,r);
    if(this.s < 0 && r.compareTo(BigInteger.ZERO) > 0) a.subTo(r,r);
    return r;
  }

  // Modular reduction using "classic" algorithm
  function Classic(m) { this.m = m; }
  function cConvert(x) {
    if(x.s < 0 || x.compareTo(this.m) >= 0) return x.mod(this.m);
    else return x;
  }
  function cRevert(x) { return x; }
  function cReduce(x) { x.divRemTo(this.m,null,x); }
  function cMulTo(x,y,r) { x.multiplyTo(y,r); this.reduce(r); }
  function cSqrTo(x,r) { x.squareTo(r); this.reduce(r); }

  Classic.prototype.convert = cConvert;
  Classic.prototype.revert = cRevert;
  Classic.prototype.reduce = cReduce;
  Classic.prototype.mulTo = cMulTo;
  Classic.prototype.sqrTo = cSqrTo;

  // (protected) return "-1/this % 2^DB"; useful for Mont. reduction
  // justification:
  //         xy == 1 (mod m)
  //         xy =  1+km
  //   xy(2-xy) = (1+km)(1-km)
  // x[y(2-xy)] = 1-k^2m^2
  // x[y(2-xy)] == 1 (mod m^2)
  // if y is 1/x mod m, then y(2-xy) is 1/x mod m^2
  // should reduce x and y(2-xy) by m^2 at each step to keep size bounded.
  // JS multiply "overflows" differently from C/C++, so care is needed here.
  function bnpInvDigit() {
    if(this.t < 1) return 0;
    var x = this[0];
    if((x&1) == 0) return 0;
    var y = x&3;        // y == 1/x mod 2^2
    y = (y*(2-(x&0xf)*y))&0xf;  // y == 1/x mod 2^4
    y = (y*(2-(x&0xff)*y))&0xff;    // y == 1/x mod 2^8
    y = (y*(2-(((x&0xffff)*y)&0xffff)))&0xffff; // y == 1/x mod 2^16
    // last step - calculate inverse mod DV directly;
    // assumes 16 < DB <= 32 and assumes ability to handle 48-bit ints
    y = (y*(2-x*y%this.DV))%this.DV;        // y == 1/x mod 2^dbits
    // we really want the negative inverse, and -DV < y < DV
    return (y>0)?this.DV-y:-y;
  }

  // Montgomery reduction
  function Montgomery(m) {
    this.m = m;
    this.mp = m.invDigit();
    this.mpl = this.mp&0x7fff;
    this.mph = this.mp>>15;
    this.um = (1<<(m.DB-15))-1;
    this.mt2 = 2*m.t;
  }

  // xR mod m
  function montConvert(x) {
    var r = nbi();
    x.abs().dlShiftTo(this.m.t,r);
    r.divRemTo(this.m,null,r);
    if(x.s < 0 && r.compareTo(BigInteger.ZERO) > 0) this.m.subTo(r,r);
    return r;
  }

  // x/R mod m
  function montRevert(x) {
    var r = nbi();
    x.copyTo(r);
    this.reduce(r);
    return r;
  }

  // x = x/R mod m (HAC 14.32)
  function montReduce(x) {
    while(x.t <= this.mt2)  // pad x so am has enough room later
      x[x.t++] = 0;
    for(var i = 0; i < this.m.t; ++i) {
      // faster way of calculating u0 = x[i]*mp mod DV
      var j = x[i]&0x7fff;
      var u0 = (j*this.mpl+(((j*this.mph+(x[i]>>15)*this.mpl)&this.um)<<15))&x.DM;
      // use am to combine the multiply-shift-add into one call
      j = i+this.m.t;
      x[j] += this.m.am(0,u0,x,i,0,this.m.t);
      // propagate carry
      while(x[j] >= x.DV) { x[j] -= x.DV; x[++j]++; }
    }
    x.clamp();
    x.drShiftTo(this.m.t,x);
    if(x.compareTo(this.m) >= 0) x.subTo(this.m,x);
  }

  // r = "x^2/R mod m"; x != r
  function montSqrTo(x,r) { x.squareTo(r); this.reduce(r); }

  // r = "xy/R mod m"; x,y != r
  function montMulTo(x,y,r) { x.multiplyTo(y,r); this.reduce(r); }

  Montgomery.prototype.convert = montConvert;
  Montgomery.prototype.revert = montRevert;
  Montgomery.prototype.reduce = montReduce;
  Montgomery.prototype.mulTo = montMulTo;
  Montgomery.prototype.sqrTo = montSqrTo;

  // (protected) true iff this is even
  function bnpIsEven() { return ((this.t>0)?(this[0]&1):this.s) == 0; }

  // (protected) this^e, e < 2^32, doing sqr and mul with "r" (HAC 14.79)
  function bnpExp(e, z, errbacks) {
    if (greaterThan(e, 0xffffffff, errbacks)) {
      errbacks.throwDomainError('expt: exponent ' + e + ' too large');
    }
    if (lessThan(e, 1, errbacks)) {
      return BigInteger.ONE;
    }
    var r = nbi(), r2 = nbi(), g = z.convert(this), i = nbits(e)-1;
    g.copyTo(r);
    while(--i >= 0) {
      z.sqrTo(r,r2);
      if((e&(1<<i)) > 0) z.mulTo(r2,g,r);
      else { var t = r; r = r2; r2 = t; }
    }
    return z.revert(r);
  }

  // (public) this^e % m, 0 <= e < 2^32
  function bnModPowInt(e, m, errbacks) {
    var z;
    if(e < 256 || m.isEven()) z = new Classic(m); else z = new Montgomery(m);
    return this.bnpExp(e, z, errbacks);
  }

  // protected
  BigInteger.prototype.copyTo = bnpCopyTo;
  BigInteger.prototype.fromInt = bnpFromInt;
  BigInteger.prototype.fromString = bnpFromString;
  BigInteger.prototype.clamp = bnpClamp;
  BigInteger.prototype.dlShiftTo = bnpDLShiftTo;
  BigInteger.prototype.drShiftTo = bnpDRShiftTo;
  BigInteger.prototype.lShiftTo = bnpLShiftTo;
  BigInteger.prototype.rShiftTo = bnpRShiftTo;
  BigInteger.prototype.subTo = bnpSubTo;
  BigInteger.prototype.multiplyTo = bnpMultiplyTo;
  BigInteger.prototype.squareTo = bnpSquareTo;
  BigInteger.prototype.divRemTo = bnpDivRemTo;
  BigInteger.prototype.invDigit = bnpInvDigit;
  BigInteger.prototype.isEven = bnpIsEven;
  BigInteger.prototype.bnpExp = bnpExp; // renamed from exp, because we need the latter for Pyret

  // public
  BigInteger.prototype.toString = bnToString;
  BigInteger.prototype.negate = bnNegate;
  BigInteger.prototype.abs = bnAbs;
  BigInteger.prototype.compareTo = bnCompareTo;
  BigInteger.prototype.bitLength = bnBitLength;
  BigInteger.prototype.mod = bnMod;
  BigInteger.prototype.modPowInt = bnModPowInt;

  // "constants"
  BigInteger.ZERO = nbv(0);
  BigInteger.ONE = nbv(1);

  // Copyright (c) 2005-2009  Tom Wu
  // All Rights Reserved.
  // See "LICENSE" for details.

  // Extended JavaScript BN functions, required for RSA private ops.

  // Version 1.1: new BigInteger("0", 10) returns "proper" zero

  // (public)
  function bnClone() { var r = nbi(); this.copyTo(r); return r; }

  // (public) return value as integer
  function bnIntValue() {
    if(this.s < 0) {
      if(this.t == 1) return this[0]-this.DV;
      else if(this.t == 0) return -1;
    }
    else if(this.t == 1) return this[0];
    else if(this.t == 0) return 0;
    // assumes 16 < DB < 32
    return ((this[1]&((1<<(32-this.DB))-1))<<this.DB)|this[0];
  }

  // (public) return value as byte
  function bnByteValue() { return (this.t==0)?this.s:(this[0]<<24)>>24; }

  // (public) return value as short (assumes DB>=16)
  function bnShortValue() { return (this.t==0)?this.s:(this[0]<<16)>>16; }

  // (protected) return x s.t. r^x < DV
  function bnpChunkSize(r) { return Math.floor(Math.LN2*this.DB/Math.log(r)); }

  // (public) 0 if this == 0, 1 if this > 0
  function bnSigNum() {
    if(this.s < 0) return -1;
    else if(this.t <= 0 || (this.t == 1 && this[0] <= 0)) return 0;
    else return 1;
  }

  // (protected) convert to radix string
  function bnpToRadix(b) {
    if(b == null) b = 10;
    if(this.signum() == 0 || b < 2 || b > 36) return "0";
    var cs = this.chunkSize(b);
    var a = Math.pow(b,cs);
    var d = nbv(a), y = nbi(), z = nbi(), r = "";
    this.divRemTo(d,y,z);
    while(y.signum() > 0) {
      r = (a+z.intValue()).toString(b).substr(1) + r;
      y.divRemTo(d,y,z);
    }
    return z.intValue().toString(b) + r;
  }

  // (protected) convert from radix string
  function bnpFromRadix(s,b) {
    this.fromInt(0);
    if(b == null) b = 10;
    var cs = this.chunkSize(b);
    var d = Math.pow(b,cs), mi = false, j = 0, w = 0;
    for(var i = 0; i < s.length; ++i) {
      var x = intAt(s,i);
      if(x < 0) {
        if(s.charAt(i) == "-" && this.signum() == 0) mi = true;
        continue;
      }
      w = b*w+x;
      if(++j >= cs) {
        this.dMultiply(d);
        this.dAddOffset(w,0);
        j = 0;
        w = 0;
      }
    }
    if(j > 0) {
      this.dMultiply(Math.pow(b,j));
      this.dAddOffset(w,0);
    }
    if(mi) BigInteger.ZERO.subTo(this,this);
  }

  // (protected) alternate constructor
  function bnpFromNumber(a,b,c) {
    if("number" == typeof b) {
      // new BigInteger(int,int,RNG)
      if(a < 2) this.fromInt(1);
      else {
        this.fromNumber(a,c);
        if(!this.testBit(a-1))  // force MSB set
          this.bitwiseTo(BigInteger.ONE.shiftLeft(a-1),op_or,this);
        if(this.isEven()) this.dAddOffset(1,0); // force odd
        while(!this.isProbablePrime(b)) {
          this.dAddOffset(2,0);
          if(this.bitLength() > a) this.subTo(BigInteger.ONE.shiftLeft(a-1),this);
        }
      }
    }
    else {
      // new BigInteger(int,RNG)
      var x = [], t = a&7;
      x.length = (a>>3)+1;
      b.nextBytes(x);
      if(t > 0) x[0] &= ((1<<t)-1); else x[0] = 0;
      this.fromString(x,256);
    }
  }

  // (public) convert to bigendian byte array
  function bnToByteArray() {
    var i = this.t, r = [];
    r[0] = this.s;
    var p = this.DB-(i*this.DB)%8, d, k = 0;
    if(i-- > 0) {
      if(p < this.DB && (d = this[i]>>p) != (this.s&this.DM)>>p)
        r[k++] = d|(this.s<<(this.DB-p));
      while(i >= 0) {
        if(p < 8) {
          d = (this[i]&((1<<p)-1))<<(8-p);
          d |= this[--i]>>(p+=this.DB-8);
        }
        else {
          d = (this[i]>>(p-=8))&0xff;
          if(p <= 0) { p += this.DB; --i; }
        }
        if((d&0x80) != 0) d |= -256;
        if(k == 0 && (this.s&0x80) != (d&0x80)) ++k;
        if(k > 0 || d != this.s) r[k++] = d;
      }
    }
    return r;
  }

  function bnEquals(a) { return(this.compareTo(a)==0); }
  function bnMin(a) { return(this.compareTo(a)<0)?this:a; }
  function bnMax(a) { return(this.compareTo(a)>0)?this:a; }

  // (protected) r = this op a (bitwise)
  function bnpBitwiseTo(a,op,r) {
    var i, f, m = Math.min(a.t,this.t);
    for(i = 0; i < m; ++i) r[i] = op(this[i],a[i]);
    if(a.t < this.t) {
      f = a.s&this.DM;
      for(i = m; i < this.t; ++i) r[i] = op(this[i],f);
      r.t = this.t;
    }
    else {
      f = this.s&this.DM;
      for(i = m; i < a.t; ++i) r[i] = op(f,a[i]);
      r.t = a.t;
    }
    r.s = op(this.s,a.s);
    r.clamp();
  }

  // (public) this & a
  function op_and(x,y) { return x&y; }
  function bnAnd(a) { var r = nbi(); this.bitwiseTo(a,op_and,r); return r; }

  // (public) this | a
  function op_or(x,y) { return x|y; }
  function bnOr(a) { var r = nbi(); this.bitwiseTo(a,op_or,r); return r; }

  // (public) this ^ a
  function op_xor(x,y) { return x^y; }
  function bnXor(a) { var r = nbi(); this.bitwiseTo(a,op_xor,r); return r; }

  // (public) this & ~a
  function op_andnot(x,y) { return x&~y; }
  function bnAndNot(a) { var r = nbi(); this.bitwiseTo(a,op_andnot,r); return r; }

  // (public) ~this
  function bnNot() {
    var r = nbi();
    for(var i = 0; i < this.t; ++i) r[i] = this.DM&~this[i];
    r.t = this.t;
    r.s = ~this.s;
    return r;
  }

  // (public) this << n
  function bnShiftLeft(n) {
    var r = nbi();
    if(n < 0) this.rShiftTo(-n,r); else this.lShiftTo(n,r);
    return r;
  }

  // (public) this >> n
  function bnShiftRight(n) {
    var r = nbi();
    if(n < 0) this.lShiftTo(-n,r); else this.rShiftTo(n,r);
    return r;
  }

  // return index of lowest 1-bit in x, x < 2^31
  function lbit(x) {
    if(x == 0) return -1;
    var r = 0;
    if((x&0xffff) == 0) { x >>= 16; r += 16; }
    if((x&0xff) == 0) { x >>= 8; r += 8; }
    if((x&0xf) == 0) { x >>= 4; r += 4; }
    if((x&3) == 0) { x >>= 2; r += 2; }
    if((x&1) == 0) ++r;
    return r;
  }

  // (public) returns index of lowest 1-bit (or -1 if none)
  function bnGetLowestSetBit() {
    for(var i = 0; i < this.t; ++i)
      if(this[i] != 0) return i*this.DB+lbit(this[i]);
    if(this.s < 0) return this.t*this.DB;
    return -1;
  }

  // return number of 1 bits in x
  function cbit(x) {
    var r = 0;
    while(x != 0) { x &= x-1; ++r; }
    return r;
  }

  // (public) return number of set bits
  function bnBitCount() {
    var r = 0, x = this.s&this.DM;
    for(var i = 0; i < this.t; ++i) r += cbit(this[i]^x);
    return r;
  }

  // (public) true iff nth bit is set
  function bnTestBit(n) {
    var j = Math.floor(n/this.DB);
    if(j >= this.t) return(this.s!=0);
    return((this[j]&(1<<(n%this.DB)))!=0);
  }

  // (protected) this op (1<<n)
  function bnpChangeBit(n,op) {
    var r = BigInteger.ONE.shiftLeft(n);
    this.bitwiseTo(r,op,r);
    return r;
  }

  // (public) this | (1<<n)
  function bnSetBit(n) { return this.changeBit(n,op_or); }

  // (public) this & ~(1<<n)
  function bnClearBit(n) { return this.changeBit(n,op_andnot); }

  // (public) this ^ (1<<n)
  function bnFlipBit(n) { return this.changeBit(n,op_xor); }

  // (protected) r = this + a
  function bnpAddTo(a,r) {
    var i = 0, c = 0, m = Math.min(a.t,this.t);
    while(i < m) {
      c += this[i]+a[i];
      r[i++] = c&this.DM;
      c >>= this.DB;
    }
    if(a.t < this.t) {
      c += a.s;
      while(i < this.t) {
        c += this[i];
        r[i++] = c&this.DM;
        c >>= this.DB;
      }
      c += this.s;
    }
    else {
      c += this.s;
      while(i < a.t) {
        c += a[i];
        r[i++] = c&this.DM;
        c >>= this.DB;
      }
      c += a.s;
    }
    r.s = (c<0)?-1:0;
    if(c > 0) r[i++] = c;
    else if(c < -1) r[i++] = this.DV+c;
    r.t = i;
    r.clamp();
  }

  // (public) this + a
  function bnAdd(a) { var r = nbi(); this.addTo(a,r); return r; }

  // (public) this - a
  function bnSubtract(a) { var r = nbi(); this.subTo(a,r); return r; }

  // (public) this * a
  function bnMultiply(a) { var r = nbi(); this.multiplyTo(a,r); return r; }

  // (public) this / a
  function bnDivide(a) { var r = nbi(); this.divRemTo(a,r,null); return r; }

  // (public) this % a
  function bnRemainder(a) { var r = nbi(); this.divRemTo(a,null,r); return r; }

  // (public) [this/a,this%a]
  function bnDivideAndRemainder(a) {
    var q = nbi(), r = nbi();
    this.divRemTo(a,q,r);
    return [q,r];
  }

  // (protected) this *= n, this >= 0, 1 < n < DV
  function bnpDMultiply(n) {
    this[this.t] = this.am(0,n-1,this,0,0,this.t);
    ++this.t;
    this.clamp();
  }

  // (protected) this += n << w words, this >= 0
  function bnpDAddOffset(n,w) {
    if(n == 0) return;
    while(this.t <= w) this[this.t++] = 0;
    this[w] += n;
    while(this[w] >= this.DV) {
      this[w] -= this.DV;
      if(++w >= this.t) this[this.t++] = 0;
      ++this[w];
    }
  }

  // A "null" reducer
  function NullExp() {}
  function nNop(x) { return x; }
  function nMulTo(x,y,r) { x.multiplyTo(y,r); }
  function nSqrTo(x,r) { x.squareTo(r); }

  NullExp.prototype.convert = nNop;
  NullExp.prototype.revert = nNop;
  NullExp.prototype.mulTo = nMulTo;
  NullExp.prototype.sqrTo = nSqrTo;

  // (public) this^e
  function bnPow(e, errbacks) {
    return this.bnpExp(e,new NullExp(), errbacks);
  }

  // (protected) r = lower n words of "this * a", a.t <= n
  // "this" should be the larger one if appropriate.
  function bnpMultiplyLowerTo(a,n,r) {
    var i = Math.min(this.t+a.t,n);
    r.s = 0; // assumes a,this >= 0
    r.t = i;
    while(i > 0) r[--i] = 0;
    var j;
    for(j = r.t-this.t; i < j; ++i) r[i+this.t] = this.am(0,a[i],r,i,0,this.t);
    for(j = Math.min(a.t,n); i < j; ++i) this.am(0,a[i],r,i,0,n-i);
    r.clamp();
  }

  // (protected) r = "this * a" without lower n words, n > 0
  // "this" should be the larger one if appropriate.
  function bnpMultiplyUpperTo(a,n,r) {
    --n;
    var i = r.t = this.t+a.t-n;
    r.s = 0; // assumes a,this >= 0
    while(--i >= 0) r[i] = 0;
    for(i = Math.max(n-this.t,0); i < a.t; ++i)
      r[this.t+i-n] = this.am(n-i,a[i],r,0,0,this.t+i-n);
    r.clamp();
    r.drShiftTo(1,r);
  }

  // Barrett modular reduction
  function Barrett(m) {
    // setup Barrett
    this.r2 = nbi();
    this.q3 = nbi();
    BigInteger.ONE.dlShiftTo(2*m.t,this.r2);
    this.mu = this.r2.divide(m);
    this.m = m;
  }

  function barrettConvert(x) {
    if(x.s < 0 || x.t > 2*this.m.t) return x.mod(this.m);
    else if(x.compareTo(this.m) < 0) return x;
    else { var r = nbi(); x.copyTo(r); this.reduce(r); return r; }
  }

  function barrettRevert(x) { return x; }

  // x = x mod m (HAC 14.42)
  function barrettReduce(x) {
    x.drShiftTo(this.m.t-1,this.r2);
    if(x.t > this.m.t+1) { x.t = this.m.t+1; x.clamp(); }
    this.mu.multiplyUpperTo(this.r2,this.m.t+1,this.q3);
    this.m.multiplyLowerTo(this.q3,this.m.t+1,this.r2);
    while(x.compareTo(this.r2) < 0) x.dAddOffset(1,this.m.t+1);
    x.subTo(this.r2,x);
    while(x.compareTo(this.m) >= 0) x.subTo(this.m,x);
  }

  // r = x^2 mod m; x != r
  function barrettSqrTo(x,r) { x.squareTo(r); this.reduce(r); }

  // r = x*y mod m; x,y != r
  function barrettMulTo(x,y,r) { x.multiplyTo(y,r); this.reduce(r); }

  Barrett.prototype.convert = barrettConvert;
  Barrett.prototype.revert = barrettRevert;
  Barrett.prototype.reduce = barrettReduce;
  Barrett.prototype.mulTo = barrettMulTo;
  Barrett.prototype.sqrTo = barrettSqrTo;

  // (public) this^e % m (HAC 14.85)
  function bnModPow(e,m) {
    var i = e.bitLength(), k, r = nbv(1), z;
    if(i <= 0) return r;
    else if(i < 18) k = 1;
    else if(i < 48) k = 3;
    else if(i < 144) k = 4;
    else if(i < 768) k = 5;
    else k = 6;
    if(i < 8)
      z = new Classic(m);
    else if(m.isEven())
      z = new Barrett(m);
    else
      z = new Montgomery(m);

    // precomputation
    var g = [], n = 3, k1 = k-1, km = (1<<k)-1;
    g[1] = z.convert(this);
    if(k > 1) {
      var g2 = nbi();
      z.sqrTo(g[1],g2);
      while(n <= km) {
        g[n] = nbi();
        z.mulTo(g2,g[n-2],g[n]);
        n += 2;
      }
    }

    var j = e.t-1, w, is1 = true, r2 = nbi(), t;
    i = nbits(e[j])-1;
    while(j >= 0) {
      if(i >= k1) w = (e[j]>>(i-k1))&km;
      else {
        w = (e[j]&((1<<(i+1))-1))<<(k1-i);
        if(j > 0) w |= e[j-1]>>(this.DB+i-k1);
      }

      n = k;
      while((w&1) == 0) { w >>= 1; --n; }
      if((i -= n) < 0) { i += this.DB; --j; }
      if(is1) { // ret == 1, don't bother squaring or multiplying it
        g[w].copyTo(r);
        is1 = false;
      }
      else {
        while(n > 1) { z.sqrTo(r,r2); z.sqrTo(r2,r); n -= 2; }
        if(n > 0) z.sqrTo(r,r2); else { t = r; r = r2; r2 = t; }
        z.mulTo(r2,g[w],r);
      }

      while(j >= 0 && (e[j]&(1<<i)) == 0) {
        z.sqrTo(r,r2); t = r; r = r2; r2 = t;
        if(--i < 0) { i = this.DB-1; --j; }
      }
    }
    return z.revert(r);
  }

  // (public) gcd(this,a) (HAC 14.54)
  function bnGCD(a) {
    var x = (this.s<0)?this.negate():this.clone();
    var y = (a.s<0)?a.negate():a.clone();
    if(x.compareTo(y) < 0) { var t = x; x = y; y = t; }
    var i = x.getLowestSetBit(), g = y.getLowestSetBit();
    if(g < 0) return x;
    if(i < g) g = i;
    if(g > 0) {
      x.rShiftTo(g,x);
      y.rShiftTo(g,y);
    }
    while(x.signum() > 0) {
      if((i = x.getLowestSetBit()) > 0) x.rShiftTo(i,x);
      if((i = y.getLowestSetBit()) > 0) y.rShiftTo(i,y);
      if(x.compareTo(y) >= 0) {
        x.subTo(y,x);
        x.rShiftTo(1,x);
      }
      else {
        y.subTo(x,y);
        y.rShiftTo(1,y);
      }
    }
    if(g > 0) y.lShiftTo(g,y);
    return y;
  }

  // (protected) this % n, n < 2^26
  function bnpModInt(n) {
    if(n <= 0) return 0;
    var d = this.DV%n, r = (this.s<0)?n-1:0;
    if(this.t > 0)
      if(d == 0) r = this[0]%n;
    else for(var i = this.t-1; i >= 0; --i) r = (d*r+this[i])%n;
    return r;
  }

  // (public) 1/this % m (HAC 14.61)
  function bnModInverse(m) {
    var ac = m.isEven();
    if((this.isEven() && ac) || m.signum() == 0) return BigInteger.ZERO;
    var u = m.clone(), v = this.clone();
    var a = nbv(1), b = nbv(0), c = nbv(0), d = nbv(1);
    while(u.signum() != 0) {
      while(u.isEven()) {
        u.rShiftTo(1,u);
        if(ac) {
          if(!a.isEven() || !b.isEven()) { a.addTo(this,a); b.subTo(m,b); }
          a.rShiftTo(1,a);
        }
        else if(!b.isEven()) b.subTo(m,b);
        b.rShiftTo(1,b);
      }
      while(v.isEven()) {
        v.rShiftTo(1,v);
        if(ac) {
          if(!c.isEven() || !d.isEven()) { c.addTo(this,c); d.subTo(m,d); }
          c.rShiftTo(1,c);
        }
        else if(!d.isEven()) d.subTo(m,d);
        d.rShiftTo(1,d);
      }
      if(u.compareTo(v) >= 0) {
        u.subTo(v,u);
        if(ac) a.subTo(c,a);
        b.subTo(d,b);
      }
      else {
        v.subTo(u,v);
        if(ac) c.subTo(a,c);
        d.subTo(b,d);
      }
    }
    if(v.compareTo(BigInteger.ONE) != 0) return BigInteger.ZERO;
    if(d.compareTo(m) >= 0) return d.subtract(m);
    if(d.signum() < 0) d.addTo(m,d); else return d;
    if(d.signum() < 0) return d.add(m); else return d;
  }

  var lowprimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509];
  var lplim = (1<<26)/lowprimes[lowprimes.length-1];

  // (public) test primality with certainty >= 1-.5^t
  function bnIsProbablePrime(t) {
    var i, x = this.abs();
    if(x.t == 1 && x[0] <= lowprimes[lowprimes.length-1]) {
      for(i = 0; i < lowprimes.length; ++i)
        if(x[0] == lowprimes[i]) return true;
      return false;
    }
    if(x.isEven()) return false;
    i = 1;
    while(i < lowprimes.length) {
      var m = lowprimes[i], j = i+1;
      while(j < lowprimes.length && m < lplim) m *= lowprimes[j++];
      m = x.modInt(m);
      while(i < j) if(m%lowprimes[i++] == 0) return false;
    }
    return x.millerRabin(t);
  }

  // (protected) true if probably prime (HAC 4.24, Miller-Rabin)
  function bnpMillerRabin(t) {
    var n1 = this.subtract(BigInteger.ONE);
    var k = n1.getLowestSetBit();
    if(k <= 0) return false;
    var r = n1.shiftRight(k);
    t = (t+1)>>1;
    if(t > lowprimes.length) t = lowprimes.length;
    var a = nbi();
    for(var i = 0; i < t; ++i) {
      a.fromInt(lowprimes[i]);
      var y = a.modPow(r,this);
      if(y.compareTo(BigInteger.ONE) != 0 && y.compareTo(n1) != 0) {
        var j = 1;
        while(j++ < k && y.compareTo(n1) != 0) {
          y = y.modPowInt(2,this);
          if(y.compareTo(BigInteger.ONE) == 0) return false;
        }
        if(y.compareTo(n1) != 0) return false;
      }
    }
    return true;
  }

  // protected
  BigInteger.prototype.chunkSize = bnpChunkSize;
  BigInteger.prototype.toRadix = bnpToRadix;
  BigInteger.prototype.fromRadix = bnpFromRadix;
  BigInteger.prototype.fromNumber = bnpFromNumber;
  BigInteger.prototype.bitwiseTo = bnpBitwiseTo;
  BigInteger.prototype.changeBit = bnpChangeBit;
  BigInteger.prototype.addTo = bnpAddTo;
  BigInteger.prototype.dMultiply = bnpDMultiply;
  BigInteger.prototype.dAddOffset = bnpDAddOffset;
  BigInteger.prototype.multiplyLowerTo = bnpMultiplyLowerTo;
  BigInteger.prototype.multiplyUpperTo = bnpMultiplyUpperTo;
  BigInteger.prototype.modInt = bnpModInt;
  BigInteger.prototype.millerRabin = bnpMillerRabin;

  // public
  BigInteger.prototype.clone = bnClone;
  BigInteger.prototype.intValue = bnIntValue;
  BigInteger.prototype.byteValue = bnByteValue;
  BigInteger.prototype.shortValue = bnShortValue;
  BigInteger.prototype.signum = bnSigNum;
  BigInteger.prototype.toByteArray = bnToByteArray;
  BigInteger.prototype.equals = bnEquals;
  BigInteger.prototype.min = bnMin;
  BigInteger.prototype.max = bnMax;
  BigInteger.prototype.and = bnAnd;
  BigInteger.prototype.or = bnOr;
  BigInteger.prototype.xor = bnXor;
  BigInteger.prototype.andNot = bnAndNot;
  BigInteger.prototype.not = bnNot;
  BigInteger.prototype.shiftLeft = bnShiftLeft;
  BigInteger.prototype.shiftRight = bnShiftRight;
  BigInteger.prototype.getLowestSetBit = bnGetLowestSetBit;
  BigInteger.prototype.bitCount = bnBitCount;
  BigInteger.prototype.testBit = bnTestBit;
  BigInteger.prototype.setBit = bnSetBit;
  BigInteger.prototype.clearBit = bnClearBit;
  BigInteger.prototype.flipBit = bnFlipBit;
  BigInteger.prototype.add = bnAdd;
  BigInteger.prototype.subtract = bnSubtract;
  BigInteger.prototype.multiply = bnMultiply;
  BigInteger.prototype.divide = bnDivide;
  BigInteger.prototype.remainder = bnRemainder;
  BigInteger.prototype.divideAndRemainder = bnDivideAndRemainder;
  BigInteger.prototype.modPow = bnModPow;
  BigInteger.prototype.modInverse = bnModInverse;
  BigInteger.prototype.pow = bnPow;
  BigInteger.prototype.gcd = bnGCD;
  BigInteger.prototype.isProbablePrime = bnIsProbablePrime;

  // BigInteger interfaces not implemented in jsbn:

  // BigInteger(int signum, byte[] magnitude)
  // double doubleValue()
  // float floatValue()
  // int hashCode()
  // long longValue()
  // static BigInteger valueOf(long val)

  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////
  // END OF copy-and-paste of jsbn.

  BigInteger.NEGATIVE_ONE = BigInteger.ONE.negate();

  // Other methods we need to add for compatibilty with js-numbers numeric tower.

  // add is implemented above.
  // subtract is implemented above.
  // multiply is implemented above.
  // equals is implemented above.
  // abs is implemented above.
  // negate is defined above.

  // makeBignum: string -> BigInteger
  var makeBignum = function(s) {
    if (typeof(s) === 'number') { s = s + ''; }
    s = expandExponent(s);
    return new BigInteger(s, 10);
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
    var a = splitIntIntoMantissaExpt(this);
    //console.log('bigint.tofixnum of', this);
    //console.log('split = ', a);
    var r = Number(String(a[0]) + 'e' + String(a[1]));
    //console.log('returning', r);
    return r;
  }

  BigInteger.prototype.toRoughnum = function(errbacks) {
    return Roughnum.makeInstance(this.toFixnum(), errbacks);
  };

  BigInteger.prototype.greaterThan = function(other, errbacks) {
    return this.compareTo(other, errbacks) > 0;
  };

  BigInteger.prototype.greaterThanOrEqual = function(other, errbacks) {
    return this.compareTo(other, errbacks) >= 0;
  };

  BigInteger.prototype.lessThan = function(other, errbacks) {
    return this.compareTo(other, errbacks) < 0;
  };

  BigInteger.prototype.lessThanOrEqual = function(other, errbacks) {
    return this.compareTo(other, errbacks) <= 0;
  };

  // divide: pyretnum -> pyretnum
  // WARNING NOTE: we override the old version of divide.
  BigInteger.prototype.divide = function(other, errbacks) {
    var quotientAndRemainder = bnDivideAndRemainder.call(this, other);
    if (quotientAndRemainder[1].compareTo(BigInteger.ZERO) === 0) {
      return quotientAndRemainder[0];
    } else {
      var result = add(quotientAndRemainder[0],
                       Rational.makeInstance(quotientAndRemainder[1], other, errbacks), errbacks);
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
    var searchIter = function(n, guess, errbacks) {
      while(!(lessThanOrEqual(sqr(guess),n, errbacks) &&
              lessThan(n,sqr(add(guess, 1, errbacks), errbacks), errbacks))) {
        guess = floor(divide(add(guess,
                                 floor(divide(n, guess, errbacks), errbacks), errbacks),
                             2, errbacks), errbacks);
      }
      return guess;
    };

    // integerSqrt: -> pyretnum
    BigInteger.prototype.integerSqrt = function(errbacks) {
      var n;
      if(sign(this) >= 0) {
        return searchIter(this, this, errbacks);
      } else {
        errbacks.throwDomainError('integerSqrt of negative bignum ' + this);
      }
    };
  })();

  (function() {
    // Get an approximation using integerSqrt, and then start another
    // Newton-Raphson search if necessary.
    BigInteger.prototype.sqrt = function(errbacks) {
      var approx = this.integerSqrt(errbacks), fix;
      if (eqv(sqr(approx, errbacks), this, errbacks)) {
        return approx;
      }
      fix = toFixnum(this);
      if (isFinite(fix)) {
        return Roughnum.makeInstance(Math.sqrt(fix), errbacks);
      } else {
        return approx;
      }
    };
  })();

  // sqrt: -> pyretnum
  // http://en.wikipedia.org/wiki/Newton's_method#Square_root_of_a_number
  // Produce the square root.

  // floor: -> pyretnum
  // Produce the floor.
  BigInteger.prototype.floor = function(errbacks) {
    return this;
  }

  // ceiling: -> pyretnum
  // Produce the ceiling.
  BigInteger.prototype.ceiling = function(errbacks) {
    return this;
  }

  // round: -> pyretnum
  // Round to the nearest integer.
  BigInteger.prototype.round = function(errbacks) {
    return this;
  };

  BigInteger.prototype.roundEven = function(errbacks) {
    return this;
  };

  // log: -> pyretnum
  // Produce the log.
  BigInteger.prototype.log = function(errbacks) {
    return log(this.toFixnum(), errbacks);
  };

  // tan: -> pyretnum
  // Produce the tan.
  BigInteger.prototype.tan = function(errbacks) {
    return tan(this.toFixnum(), errbacks);
  };

  // atan: -> pyretnum
  // Produce the arc tangent.
  BigInteger.prototype.atan = function(errbacks) {
    return atan(this.toFixnum(), errbacks);
  };

  // cos: -> pyretnum
  // Produce the cosine.
  BigInteger.prototype.cos = function(errbacks) {
    return cos(this.toFixnum(), errbacks);
  };

  // sin: -> pyretnum
  // Produce the sine.
  BigInteger.prototype.sin = function(errbacks) {
    return sin(this.toFixnum(), errbacks);
  };

  // expt: pyretnum -> pyretnum
  // Produce the power to the input.
  BigInteger.prototype.expt = function(n, errbacks) {
    return bnPow.call(this, n, errbacks);
  };

  // exp: -> pyretnum
  // Produce e raised to the given power.
  BigInteger.prototype.exp = function(errbacks) {
    var res = Math.exp(this.toFixnum());
    if (!isFinite(res))
      errbacks.throwDomainError('exp: argument too large: ' + this);
    return Roughnum.makeInstance(res, errbacks);
  };

  // acos: -> pyretnum
  // Produce the arc cosine.
  BigInteger.prototype.acos = function(errbacks) {
    return acos(this.toFixnum(), errbacks);
  };

  // asin: -> pyretnum
  // Produce the arc sine.
  BigInteger.prototype.asin = function(errbacks) {
    return asin(this.toFixnum(), errbacks);
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
  // An optional limit on the decimal expansion can be provided, in which
  // case the search cuts off if we go past the limit.
  // If this happens, the third argument returned becomes '...' to indicate
  // that the search was prematurely cut off.
  var toRepeatingDecimal = (function() {
    var getResidue = function(r, d, limit, errbacks) {
      var digits = [];
      var seenRemainders = {};
      seenRemainders[r] = true;
      while(true) {
        if (limit-- <= 0) {
          return [digits.join(''), '...']
        }

        var nextDigit = quotient(
          multiply(r, 10, errbacks), d, errbacks);
        var nextRemainder = remainder(
          multiply(r, 10, errbacks),
          d, errbacks);
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
        var nextDigit = quotient(multiply(r, 10, errbacks), d, errbacks);
        var nextRemainder = remainder(
          multiply(r, 10, errbacks),
          d, errbacks);
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

    return function(n, d, options, errbacks) {
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
      if (equals(d, 0, errbacks)) {
        errbacks.throwDomainError('toRepeatingDecimal: d equals 0');
      }
      if (lessThan(d, 0, errbacks)) {
        errbacks.throwDomainError('toRepeatingDecimal: d < 0');
      }
      var sign = (lessThan(n, 0) ? "-" : "");
      n = abs(n, errbacks);
      var beforeDecimalPoint = sign + quotient(n, d, errbacks);
      var afterDecimals = getResidue(remainder(n, d, errbacks), d, limit, errbacks);
      return [beforeDecimalPoint].concat(afterDecimals);
    };
  })();
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
  function toStringDigits(n, digits, errbacks) {
    if (!isInteger(digits)) {
      errbacks.throwDomainError('num-to-string-digits: digits should be an integer');
    }
    var tenDigits = expt(10, digits, errbacks);
    var d = toFixnum(digits);
    n = divide(round(multiply(n, tenDigits, errbacks), errbacks), tenDigits, errbacks);
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
    // n is not an integer implies that d >= 1
    var decimal = toRepeatingDecimal(n.numerator(), n.denominator(), undefined, errbacks);
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

  return Numbers;
});
