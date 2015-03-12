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

// Removes Complex, Exact, Inexact, Floatnum.
// Adds Roughnum.

// Scheme numbers.

//var jsnums = {};

// We try to stick with the unboxed fixnum representation for
// integers, since that's what scheme programs commonly deal with, and
// we want that common type to be lightweight.

// A boxed-scheme-number is either BigInteger, Rational, or Roughnum
// An integer-scheme-number is either fixnum or BigInteger.

define(function() {
  'use strict';
  // Abbreviation
  var Numbers = {};
  //var Numbers = jsnums;

  // makeNumericBinop: (fixnum fixnum -> any) (scheme-number scheme-number -> any) -> (scheme-number scheme-number) X
  // Creates a binary function that works either on fixnums or boxnums.
  // Applies the appropriate binary function, ensuring that both scheme numbers are
  // lifted to the same kind.
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

      return onBoxednums(x, y);
    };
  };

  // fromFixnum: fixnum -> scheme-number
  var fromFixnum = function(x) {
    if (isNaN(x) || (! isFinite(x))) {
      return Roughnum.makeInstance(x);
    }
    var nf = Math.floor(x);
    if (nf === x) {
      if (isOverflow(nf)) {
        return makeBignum(expandExponent(x+''));
      } else {
        return nf;
      }
    } else {
      //  used to return float, now rational
      var stringRep = x.toString();
      var match = stringRep.match(/^(.*)\.(.*)$/);
      if (match) {
        var afterDecimal = parseInt(match[2]);
        var factorToInt = Math.pow(10, match[2].length);
        var extraFactor = _integerGcd(factorToInt, afterDecimal);
        var multFactor = factorToInt / extraFactor;
        return Rational.makeInstance(Math.round(x*multFactor), Math.round(factorToInt/extraFactor));
      } else {
        return Rational.makeInstance(x, 1);
      }

    }
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

  // liftFixnumInteger: fixnum-integer boxed-scheme-number -> boxed-scheme-number
  // Lifts up fixnum integers to a boxed type.

  var liftFixnumInteger = function(x, other) {
    if (other instanceof Roughnum)
      return new Roughnum(x);
    else if (other instanceof BigInteger)
      return makeBignum(x);
    else
      return new Rational(x, 1);
  };

  // throwRuntimeError: string (scheme-number | undefined) (scheme-number | undefined) -> void
  // Throws a runtime error with the given message string.
  var throwRuntimeError = function(msg, x, y) {
    Numbers['onThrowRuntimeError'](msg, x, y);
  };

  // onThrowRuntimeError: string (scheme-number | undefined) (scheme-number | undefined) -> void
  // By default, will throw a new Error with the given message.
  // Override Numbers['onThrowRuntimeError'] if you need to do something special.
  var onThrowRuntimeError = function(msg, x, y) {
    throw new Error(msg);
  };

  // isSchemeNumber: any -> boolean
  // Returns true if the thing is a scheme number.
  var isSchemeNumber = function(thing) {
    return (typeof(thing) === 'number'
            || (thing instanceof Rational ||
                thing instanceof Roughnum ||
                thing instanceof BigInteger));
  };

  // isRational: scheme-number -> boolean
  var isRational = function(n) {
    return (typeof(n) === 'number' ||
            (isSchemeNumber(n) && n.isRational()));
  };

  // isReal: scheme-number -> boolean
  var isReal = function(n) {
    return (typeof(n) === 'number' ||
            (isSchemeNumber(n) && n.isReal()));
  };

  // isExact: scheme-number -> boolean
  var isExact = function(n) {
    return (typeof(n) === 'number' ||
            (isSchemeNumber(n) && n.isExact()));
  };

  // isInteger: scheme-number -> boolean
  var isInteger = function(n) {
    return (typeof(n) === 'number' ||
            (isSchemeNumber(n) && n.isInteger()));
  };

  var isRoughnum = function(n) {
    if (typeof(n) === 'number') {
      return false;
    } else {
      return (isSchemeNumber(n) && n.isRoughnum());
    }
  };

  var isPositive = function(n) {
    if (typeof(n) === 'number') {
      return n > 0;
    } else {
      return (isSchemeNumber(n) && n.isPositive());
    }
  };

  var isNonPositive = function(n) {
    if (typeof(n) === 'number') {
      return n <= 0;
    } else {
      return (isSchemeNumber(n) && n.isNonPositive());
    }
  };

  var isNegative = function(n) {
    if (typeof(n) === 'number') {
      return n < 0;
    } else {
      return (isSchemeNumber(n) && n.isNegative());
    }
  };

  var isNonNegative = function(n) {
    if (typeof(n) === 'number') {
      return n >= 0;
    } else {
      return (isSchemeNumber(n) && n.isNonNegative());
    }
  };

  // toFixnum: scheme-number -> javascript-number
  var toFixnum = function(n) {
    if (typeof(n) === 'number')
      return n;
    return n.toFixnum();
  };

  // toExact: scheme-number -> scheme-number
  // this is used in runtime-anf.js -- can't phase it out immediately
  var toExact = function(n) {
    if (typeof(n) === 'number')
      return n;
    return n.toExact();
  };

  var toRational = toExact;

  // toRoughnum: scheme-number -> scheme-number

  var toRoughnum = function(n) {
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(n);
    } else {
      return n.toRoughnum();
    }
  };

  //////////////////////////////////////////////////////////////////////

  // add: scheme-number scheme-number -> scheme-number
  var add = function(x, y) {
    var sum;
    if (typeof(x) === 'number' && typeof(y) === 'number') {
      sum = x + y;
      if (isOverflow(sum)) {
        return (makeBignum(x)).add(makeBignum(y));
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

  // subtract: scheme-number scheme-number -> scheme-number
  var subtract = makeNumericBinop(
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

  // mulitply: scheme-number scheme-number -> scheme-number
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

  // divide: scheme-number scheme-number -> scheme-number
  var divide = makeNumericBinop(
    function(x, y) {
      if (_integerIsZero(y))
        throwRuntimeError("/: division by zero", x, y);
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
      return x.divide(y);
    },
    {
      isXSpecialCase: function(x) {
        return equalsAnyZero(x);
      },
      onXSpecialCase: function(x, y) {
        if (equalsAnyZero(y)) {
          throwRuntimeError("/: division by zero", x, y);
        }
        return 0;
      },
      isYSpecialCase: function(y) {
        return equalsAnyZero(y);
      },
      onYSpecialCase: function(x, y) {
        throwRuntimeError("/: division by zero", x, y);
      }
    });

  // equals: scheme-number scheme-number -> boolean
  var equals = makeNumericBinop(
    function(x, y) {
      return x === y;
    },
    function(x, y) {
      return x.equals(y);
    });

  var equalsAnyZero = function(x) {
    if (typeof(x) === 'number') return x === 0;
    if (isRoughnum(x)) return x.n === 0;
    return x.equals(0);
  };

  // eqv: scheme-number scheme-number -> boolean
  var eqv = function(x, y) {
    if (x === y)
      return true;
    if (typeof(x) === 'number' && typeof(y) === 'number')
      return x === y;
    var ex = isExact(x), ey = isExact(y);
    return (((ex && ey) || (!ex && !ey)) && equals(x, y));
  };

  // approxEqual: scheme-number scheme-number scheme-number -> boolean
  var approxEquals = function(x, y, delta) {
    return lessThanOrEqual(abs(subtract(x, y)),
                           delta);
  };

  // used for within
  var roughlyEquals = function(x, y, delta) {
    if (isNegative(delta)) {
      throwRuntimeError("negative tolerance", delta);
    }

    if (x === y) return true;
    var ratx = isRoughnum(x) ? x.toRational() : x;
    var raty = isRoughnum(y) ? y.toRational() : y;
    var ratdelta = isRoughnum(delta) ? delta.toRational() : delta;
    return approxEquals(ratx, raty, ratdelta);
  };

  // greaterThanOrEqual: scheme-number scheme-number -> boolean
  var greaterThanOrEqual = makeNumericBinop(
    function(x, y) {
      return x >= y;
    },
    function(x, y) {
      return x.greaterThanOrEqual(y);
    });

  // lessThanOrEqual: scheme-number scheme-number -> boolean
  var lessThanOrEqual = makeNumericBinop(
    function(x, y){
      return x <= y;
    },
    function(x, y) {
      return x.lessThanOrEqual(y);
    });

  // greaterThan: scheme-number scheme-number -> boolean
  var greaterThan = makeNumericBinop(
    function(x, y){
      return x > y;
    },
    function(x, y) {
      return x.greaterThan(y);
    });

  // lessThan: scheme-number scheme-number -> boolean
  var lessThan = makeNumericBinop(
    function(x, y){
      return x < y;
    },
    function(x, y) {
      return x.lessThan(y);
    });

  // expt: scheme-number scheme-number -> scheme-number
  var expt = makeNumericBinop(
    function(x, y) {
      var pow = Math.pow(x, y);
      if (isOverflow(pow)) {
        return (makeBignum(x)).expt(makeBignum(y));
      } else {
        return pow;
      }
    },
    function(x, y) {
      return x.expt(y);
    },
    {
      isXSpecialCase: function(x) {
        return eqv(x, 0) || eqv(x,1);
      },
      onXSpecialCase: function(x, y) {
        if (eqv(x, 0)) {
          if (eqv(y, 0)) {
            return 1;
          } else if (lessThan(y, 0)) {
            throwRuntimeError("expt: division by zero");
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
          return expt(divide(1, x), negate(y));
        }
      }
    });

  // exp: scheme-number -> scheme-number
  var exp = function(n) {
    if ( eqv(n, 0) ) {
      return 1;
    }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.exp(n));
    }
    return n.exp();
  };

  // modulo: scheme-number scheme-number -> scheme-number
  var modulo = function(m, n) {
    if (! isInteger(m)) {
      throwRuntimeError('modulo: the first argument '
                        + m + " is not an integer.", m, n);
    }
    if (! isInteger(n)) {
      throwRuntimeError('modulo: the second argument '
                        + n + " is not an integer.", m, n);
    }
    if (_integerIsZero(n)) {
      throwRuntimeError('modulo: the second argument is zero');
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

  // numerator: scheme-number -> scheme-number
  var numerator = function(n) {
    if (typeof(n) === 'number')
      return n;
    return n.numerator();
  };

  // denominator: scheme-number -> scheme-number
  var denominator = function(n) {
    if (typeof(n) === 'number')
      return 1;
    return n.denominator();
  };

  // sqrt: scheme-number -> scheme-number
  var sqrt = function(n) {
    if (typeof(n) === 'number') {
      if (n >= 0) {
        var result = Math.sqrt(n);
        if (Math.floor(result) === result) {
          return result;
        } else {
          return Roughnum.makeInstance(result);
        }
      } else {
        throwRuntimeError("sqrt of negative number", n);
      }
    }
    return n.sqrt();
  };

  // abs: scheme-number -> scheme-number
  var abs = function(n) {
    if (typeof(n) === 'number') {
      return Math.abs(n);
    }
    return n.abs();
  };

  // floor: scheme-number -> scheme-number
  var floor = function(n) {
    if (typeof(n) === 'number')
      return Math.floor(n);
    return n.floor();
  };

  // ceiling: scheme-number -> scheme-number
  var ceiling = function(n) {
    if (typeof(n) === 'number')
      return Math.ceil(n);
    return n.ceiling();
  };

  // NB: all of these trig-gy generic functions should now return roughnum rather than float
  // (except for an arg of 0, etc)

  // log: scheme-number -> scheme-number
  var log = function(n) {
    if ( eqv(n, 1) ) {
      return 0;
    }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.log(n));
    }
    return n.log();
  };

  // tan: scheme-number -> scheme-number
  var tan = function(n) {
    if (eqv(n, 0)) { return 0; }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.tan(n));
    }
    return n.tan();
  };

  // atan: scheme-number -> scheme-number
  var atan = function(n) {
    if (eqv(n, 0)) { return 0; }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.atan(n));
    }
    return n.atan();
  };

  // cos: scheme-number -> scheme-number
  var cos = function(n) {
    if (eqv(n, 0)) { return 1; }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.cos(n));
    }
    return n.cos();
  };

  // sin: scheme-number -> scheme-number
  var sin = function(n) {
    if (eqv(n, 0)) { return 0; }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.sin(n));
    }
    return n.sin();
  };

  // acos: scheme-number -> scheme-number
  var acos = function(n) {
    if (eqv(n, 1)) { return 0; }
    if (lessThan(n, -1) || greaterThan(n, 1)) {
      throwRuntimeError('acos: arg out of domain', n);
    }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.acos(n));
    }
    return n.acos();
  };

  // asin: scheme-number -> scheme-number
  var asin = function(n) {
    if (eqv(n, 0)) { return 0; }
    if (lessThan(n, -1) || greaterThan(n, 1)) {
      throwRuntimeError('asin: arg out of domain', n);
    }
    if (typeof(n) === 'number') {
      return Roughnum.makeInstance(Math.asin(n));
    }
    return n.asin();
  };

  // round: scheme-number -> scheme-number
  var round = function(n) {
    if (typeof(n) === 'number') {
      return n;
    }
    return n.round();
  };

  // sqr: scheme-number -> scheme-number
  var sqr = function(x) {
    return multiply(x, x);
  };

  // integerSqrt: scheme-number -> scheme-number
  var integerSqrt = function(x) {
    if (! isInteger(x)) {
      throwRuntimeError('integer-sqrt: the argument ' + x.toString() +
                        " is not an integer.", x);
    }
    if (typeof (x) === 'number') {
      if(x < 0) {
        throwRuntimeError('integerSqrt of negative number', x);
      } else {
        return Math.floor(Math.sqrt(x));
      }
    }
    return x.integerSqrt();
  };

  // gcd: scheme-number [scheme-number ...] -> scheme-number
  var gcd = function(first, rest) {
    if (! isInteger(first)) {
      throwRuntimeError('gcd: the argument ' + first.toString() +
                        " is not an integer.", first);
    }
    var a = abs(first), t, b;
    for(var i = 0; i < rest.length; i++) {
      b = abs(rest[i]);
      if (! isInteger(b)) {
        throwRuntimeError('gcd: the argument ' + b.toString() +
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

  // lcm: scheme-number [scheme-number ...] -> scheme-number
  var lcm = function(first, rest) {
    if (! isInteger(first)) {
      throwRuntimeError('lcm: the argument ' + first.toString() +
                        " is not an integer.", first);
    }
    var result = abs(first);
    if (_integerIsZero(result)) { return 0; }
    for (var i = 0; i < rest.length; i++) {
      if (! isInteger(rest[i])) {
        throwRuntimeError('lcm: the argument ' + rest[i].toString() +
                          " is not an integer.", rest[i]);
      }
      var divisor = _integerGcd(result, rest[i]);
      if (_integerIsZero(divisor)) {
        return 0;
      }
      result = divide(multiply(result, rest[i]), divisor);
    }
    return result;
  };

  var quotient = function(x, y) {
    if (! isInteger(x)) {
      throwRuntimeError('quotient: the first argument ' + x.toString() +
                        " is not an integer.", x);
    }
    if (! isInteger(y)) {
      throwRuntimeError('quotient: the second argument ' + y.toString() +
                        " is not an integer.", y);
    }
    return _integerQuotient(x, y);
  };

  var remainder = function(x, y) {
    if (! isInteger(x)) {
      throwRuntimeError('remainder: the first argument ' + x.toString() +
                        " is not an integer.", x);
    }
    if (! isInteger(y)) {
      throwRuntimeError('remainder: the second argument ' + y.toString() +
                        " is not an integer.", y);
    }
    return _integerRemainder(x, y);
  };

  // Implementation of the hyperbolic functions
  // http://en.wikipedia.org/wiki/Hyperbolic_cosine
  var cosh = function(x) {
    if (eqv(x, 0)) {
      return Roughnum.makeInstance(1.0);
    }
    return divide(add(exp(x), exp(negate(x))),
                  2);
  };

  var sinh = function(x) {
    return divide(subtract(exp(x), exp(negate(x))),
                  2);
  };

  //////////////////////////////////////////////////////////////////////

  // Helpers

  // IsFinite: scheme-number -> boolean
  // Returns true if the scheme number is finite or not.
  var isSchemeNumberFinite = function(n) {
    if (typeof(n) === 'number') {
      return isFinite(n);
    } else {
      return n.isFinite();
    }
  };

  // isOverflow: javascript-number -> boolean
  // Returns true if we consider the number an overflow.
  var MIN_FIXNUM = -(9e15);
  var MAX_FIXNUM = (9e15);
  var isOverflow = function(n) {
    return (n < MIN_FIXNUM ||  MAX_FIXNUM < n);
  };

  // negate: scheme-number -> scheme-number
  // multiplies a number times -1.
  var negate = function(n) {
    if (typeof(n) === 'number') {
      return -n;
    }
    return n.negate();
  };

  // halve: scheme-number -> scheme-number
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
  // Helper to collect the common logic for coersing integer fixnums or bignums to a
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
      return onBignums(m, n);
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
      return onBignums(m);
    });
  };

  // _integerModulo: integer-scheme-number integer-scheme-number -> integer-scheme-number
  var _integerModulo = makeIntegerBinop(
    function(m, n) {
      return m % n;
    },
    function(m, n) {
      return bnMod.call(m, n);
    });

  // _integerGcd: integer-scheme-number integer-scheme-number -> integer-scheme-number
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

  // _integerIsZero: integer-scheme-number -> boolean
  // Returns true if the number is zero.
  var _integerIsZero = makeIntegerUnOp(
    function(n){
      return n === 0;
    },
    function(n) {
      return bnEquals.call(n, BigInteger.ZERO);
    }
  );

  // _integerIsOne: integer-scheme-number -> boolean
  var _integerIsOne = makeIntegerUnOp(
    function(n) {
      return n === 1;
    },
    function(n) {
      return bnEquals.call(n, BigInteger.ONE);
    });

  // _integerIsNegativeOne: integer-scheme-number -> boolean
  var _integerIsNegativeOne = makeIntegerUnOp(
    function(n) {
      return n === -1;
    },
    function(n) {
      return bnEquals.call(n, BigInteger.NEGATIVE_ONE);
    });

  // _integerAdd: integer-scheme-number integer-scheme-number -> integer-scheme-number
  var _integerAdd = makeIntegerBinop(
    function(m, n) {
      return m + n;
    },
    function(m, n) {
      return bnAdd.call(m, n);
    });

  // _integerSubtract: integer-scheme-number integer-scheme-number -> integer-scheme-number
  var _integerSubtract = makeIntegerBinop(
    function(m, n) {
      return m - n;
    },
    function(m, n) {
      return bnSubtract.call(m, n);
    });

  // _integerMultiply: integer-scheme-number integer-scheme-number -> integer-scheme-number
  var _integerMultiply = makeIntegerBinop(
    function(m, n) {
      return m * n;
    },
    function(m, n) {
      return bnMultiply.call(m, n);
    });

  //_integerQuotient: integer-scheme-number integer-scheme-number -> integer-scheme-number
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

  // _integerDivideToFixnum: integer-scheme-number integer-scheme-number -> fixnum
  var _integerDivideToFixnum = makeIntegerBinop(
    function(m, n) {
      return m / n;
    },
    function(m, n) {
      return toFixnum(m) / toFixnum(n);
    },
    {ignoreOverflow: true,
     doNotCoerceToFloating: true});

  // _integerEquals: integer-scheme-number integer-scheme-number -> boolean
  var _integerEquals = makeIntegerBinop(
    function(m, n) {
      return m === n;
    },
    function(m, n) {
      return bnEquals.call(m, n);
    },
    {doNotCoerceToFloating: true});

  // _integerGreaterThan: integer-scheme-number integer-scheme-number -> boolean
  var _integerGreaterThan = makeIntegerBinop(
    function(m, n) {
      return m > n;
    },
    function(m, n) {
      return bnCompareTo.call(m, n) > 0;
    },
    {doNotCoerceToFloating: true});

  // _integerLessThan: integer-scheme-number integer-scheme-number -> boolean
  var _integerLessThan = makeIntegerBinop(
    function(m, n) {
      return m < n;
    },
    function(m, n) {
      return bnCompareTo.call(m, n) < 0;
    },
    {doNotCoerceToFloating: true});

  // _integerGreaterThanOrEqual: integer-scheme-number integer-scheme-number -> boolean
  var _integerGreaterThanOrEqual = makeIntegerBinop(
    function(m, n) {
      return m >= n;
    },
    function(m, n) {
      return bnCompareTo.call(m, n) >= 0;
    },
    {doNotCoerceToFloating: true});

  // _integerLessThanOrEqual: integer-scheme-number integer-scheme-number -> boolean
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
  // Produce true if this number can be coersed into an integer.

  // isRational: -> boolean
  // Produce true if the number is rational.

  // isReal: -> boolean
  // Produce true if the number is real.

  // isExact: -> boolean
  // Produce true if the number is exact

  // toExact: -> scheme-number
  // Produce an exact number.

  // toFixnum: -> javascript-number
  // Produce a javascript number.

  // greaterThan: scheme-number -> boolean
  // Compare against instance of the same type.

  // greaterThanOrEqual: scheme-number -> boolean
  // Compare against instance of the same type.

  // lessThan: scheme-number -> boolean
  // Compare against instance of the same type.

  // lessThanOrEqual: scheme-number -> boolean
  // Compare against instance of the same type.

  // add: scheme-number -> scheme-number
  // Add with an instance of the same type.

  // subtract: scheme-number -> scheme-number
  // Subtract with an instance of the same type.

  // multiply: scheme-number -> scheme-number
  // Multiply with an instance of the same type.

  // divide: scheme-number -> scheme-number
  // Divide with an instance of the same type.

  // numerator: -> scheme-number
  // Return the numerator.

  // denominator: -> scheme-number
  // Return the denominator.

  // integerSqrt: -> scheme-number
  // Produce the integer square root.

  // sqrt: -> scheme-number
  // Produce the square root.

  // abs: -> scheme-number
  // Produce the absolute value.

  // floor: -> scheme-number
  // Produce the floor.

  // ceiling: -> scheme-number
  // Produce the ceiling.

  // log: -> scheme-number
  // Produce the log.

  // atan: -> scheme-number
  // Produce the arc tangent.

  // cos: -> scheme-number
  // Produce the cosine.

  // sin: -> scheme-number
  // Produce the sine.

  // expt: scheme-number -> scheme-number
  // Produce the power to the input.

  // exp: -> scheme-number
  // Produce e raised to the given power.

  // acos: -> scheme-number
  // Produce the arc cosine.

  // asin: -> scheme-number
  // Produce the arc sine.

  // round: -> scheme-number
  // Round to the nearest integer.

  // equals: scheme-number -> boolean
  // Produce true if the given number of the same type is equal.

  //////////////////////////////////////////////////////////////////////

  // Rationals

  var Rational = function(n, d) {
    this.n = n;
    this.d = d;
  };

  Rational.makeInstance = function(n, d) {
    if (n === undefined)
      throwRuntimeError("n undefined", n, d);

    if (d === undefined) { d = 1; }

    if (_integerLessThan(d, 0)) {
      n = negate(n);
      d = negate(d);
    }

    var divisor = _integerGcd(abs(n), abs(d));
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
    return Rational.makeInstance(-this.n, this.d)
  };

  Rational.prototype.multiply = function(other) {
    return Rational.makeInstance(_integerMultiply(this.n, other.n),
                                 _integerMultiply(this.d, other.d));
  };

  Rational.prototype.divide = function(other) {
    if (_integerIsZero(this.d) || _integerIsZero(other.n)) {  // dead code!
      throwRuntimeError("/: division by zero", this, other);
    }
    return Rational.makeInstance(_integerMultiply(this.n, other.d),
                                 _integerMultiply(this.d, other.n));
  };

  Rational.prototype.toExact = function() {
    return this;
  };

  Rational.prototype.toRational = Rational.prototype.toExact;

  Rational.prototype.isExact = function() {
    return true;
  };

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
    return toExact(floor(result));
  };

  Rational.prototype.sqrt = function() {
    if (_integerGreaterThanOrEqual(this.n,  0)) {
      var newN = sqrt(this.n);
      var newD = sqrt(this.d);
      if (isExact(newN) && isExact(newD) &&
          equals(floor(newN), newN) &&
          equals(floor(newD), newD)) {
        return Rational.makeInstance(newN, newD);
      } else {
        return divide(newN, newD);
      }
    } else {
      throwRuntimeError('sqrt of negative rational', this.n, this.d);
    }
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

  Rational.prototype.log = function(){
    return Roughnum.makeInstance(Math.log(this.n / this.d));
  };

  Rational.prototype.tan = function(){
    return Roughnum.makeInstance(Math.tan(_integerDivideToFixnum(this.n, this.d)));
  };

  Rational.prototype.atan = function(){
    return Roughnum.makeInstance(Math.atan(_integerDivideToFixnum(this.n, this.d)));
  };

  Rational.prototype.cos = function(){
    return Roughnum.makeInstance(Math.cos(_integerDivideToFixnum(this.n, this.d)));
  };

  Rational.prototype.sin = function(){
    return Roughnum.makeInstance(Math.sin(_integerDivideToFixnum(this.n, this.d)));
  };

  /*
  Rational.prototype.expt = function(a){
    if (isInteger(a) && greaterThanOrEqual(a, 0)) {
      return fastExpt(this, a);
    }
    return Roughnum.makeInstance(Math.pow(_integerDivideToFixnum(this.n, this.d),
                                          _integerDivideToFixnum(a.n, a.d)));
  };*/

  var integerNthRootSearchIter = function(r, n, guess) {
    var guessPrev, guessToTheR;

    while(true) {
      guessToTheR = expt(guess, r);
      if (lessThanOrEqual(guessToTheR, n) &&
          lessThan(n, expt(add(guess, 1), r))) break;
      guessPrev = guess;
      guess = floor(subtract(guess, divide(subtract(guessToTheR, n),
            multiply(r, divide(guessToTheR, guess)))));
      if (equals(guess, guessPrev)) break;
    }

    return guess;
  };

  var integerNthRoot = function(n, r) {
    if (sign(n) >= 0 ||
        (sign(n) < 0 && _integerModulo(r, 2) === 1)) {
      return integerNthRootSearchIter(r, n, n);
    } else {
      throwRuntimeError(r + "th root of negative bigint " + n);
    }
  };

  var nthRoot = function(n, r) {
    var approx = integerNthRoot(n, r);
    if (eqv(expt(approx, r), n)) {
      return approx;
    } else {
      return Roughnum.makeInstance(Math.pow(toFixnum(n), toFixnum(divide(1,r))));
    }
  };

  Rational.prototype.expt = function(a) {
    if (isInteger(a) && greaterThanOrEqual(a, 0)) {
      return fastExpt(this, a);
    } else if (_integerLessThanOrEqual(a.d, 8)) {
      var nRaisedToAn = expt(this.n, a.n);
      var dRaisedToAn = expt(this.d, a.n);
      var newN = nthRoot(nRaisedToAn, a.d);
      var newD = nthRoot(dRaisedToAn, a.d);
      if (isExact(newN) && isExact(newD) &&
          equals(floor(newN), newN) &&
          equals(floor(newD), newD)) {
        return Rational.makeInstance(newN, newD);
      } else {
        return divide(newN, newD);
      }
    } else {
      return Roughnum.makeInstance(Math.pow(_integerDivideToFixnum(this.n, this.d),
            _integerDivideToFixnum(a.n, a.d)));
    }
  };

  Rational.prototype.exp = function(){
    return Roughnum.makeInstance(Math.exp(_integerDivideToFixnum(this.n, this.d)));
  };

  Rational.prototype.acos = function(){
    return acos(_integerDivideToFixnum(this.n, this.d));
  };

  Rational.prototype.asin = function(){
    return asin(_integerDivideToFixnum(this.n, this.d));
  };

  Rational.prototype.round = function() {
    // FIXME: not correct when values are bignums
    if (equals(this.d, 2)) {
      // Round to even if it's a n/2
      var v = _integerDivideToFixnum(this.n, this.d);
      var fl = Math.floor(v);
      var ce = Math.ceil(v);
      if (_integerIsZero(fl % 2)) {
        return fl;
      }
      else {
        return ce;
      }
    } else {
      return Math.round(this.n / this.d);
    }
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
      throwRuntimeError('roughnum constructor got unsuitable arg ' + n);
    this.n = n;
  };

  Roughnum.makeInstance = function(n) {
    if (typeof(n) === 'number' && (isNaN(n) || n === Number.POSITIVE_INFINITY || n === Number.NEGATIVE_INFINITY)) {
      throwRuntimeError('roughnum overflow error');
    }
    return new Roughnum(n);
  };

  Roughnum.prototype.isExact = function() { //needed?
    return false;
  };

  Roughnum.prototype.isFinite = function() {
    //actually, true, as we don't store overflows
    return (isFinite(this.n));
  };

  Roughnum.prototype.toExact = function() {
    // The precision of ieee is about 16 decimal digits, which we use here.
    if (! isFinite(this.n) || isNaN(this.n)) {
      throwRuntimeError("toExact: no exact representation for " + this, this);
    }

    var stringRep = this.n.toString();
    var match = stringRep.match(/^(.*)\.(.*)$/);
    if (match) {
      var intPart = parseInt(match[1]);
      var fracPart = parseInt(match[2]);
      var tenToDecimalPlaces = Math.pow(10, match[2].length);
      return Rational.makeInstance(Math.round(this.n * tenToDecimalPlaces),
                                   tenToDecimalPlaces);
    }
    else {
      return this.n;
    }
  };

  Roughnum.prototype.toRational = Roughnum.prototype.toExact;

  Roughnum.prototype.toString = function() {
    /*
      var partialResult = this.n.toString();
      if (! partialResult.match('\\.') && ! partialResult.match('e')) {
      return "~" + partialResult + ".0";
      } else {
      return "~" + partialResult;
      }
    */
    return '~' + this.n.toString();
  };

  Roughnum.prototype.equals = function(other, aUnionFind) {
    throwRuntimeError("roughnums cannot be compared for equality");
  };

  Roughnum.prototype.isRational = function() {
    return false;
  };

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
    return this.n; // lib seems to call any jsnum a fixnum, not just small ints?
  };

  Roughnum.prototype.toRoughnum = function() {
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

  Roughnum.prototype.floor = function() {
    return Math.floor(this.n);
  };

  Roughnum.prototype.ceiling = function() {
    return Math.ceil(this.n);
  };

  Roughnum.prototype.greaterThan = function(other) {
    return this.n > other.n;
  };

  Roughnum.prototype.greaterThanOrEqual = function(other) {
    return this.n > other.n;
  };

  Roughnum.prototype.lessThan = function(other) {
    return this.n < other.n;
  };

  Roughnum.prototype.lessThanOrEqual = function(other) {
    return this.n < other.n;
  };

  Roughnum.prototype.integerSqrt = function() {
    if (isInteger(this)) {
      if(this.n >= 0) {
        return Roughnum.makeInstance(Math.floor(Math.sqrt(this.n)));
      } else {
        throwRuntimeError('integerSqrt of negative roughnum', this.n);
      }
    } else {
      throwRuntimeError("integerSqrt: can only be applied to an integer", this);
    }
  };

  Roughnum.prototype.sqrt = function() {
    if (this.n < 0) {
      throwRuntimeError('sqrt of negative roughnum', this.n);
    } else {
      return Roughnum.makeInstance(Math.sqrt(this.n));
    }
  };

  Roughnum.prototype.abs = function() {
    return Roughnum.makeInstance(Math.abs(this.n));
  };

  Roughnum.prototype.log = function(){
    if (this.n < 0)
      throwRuntimeError('log of negative roughnum', this.n);
    else
      return Roughnum.makeInstance(Math.log(this.n));
  };

  // NB: the trig generic functions above (near l 580) should return
  // roughnum rather than float -- except for arg non-rough 0, which maps
  // perfectly to non-rough 0 or 1

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
      return Roughnum.makeInstance(Math.pow(this.n, a.n));
    }
  };

  Roughnum.prototype.exp = function(){
    return Roughnum.makeInstance(Math.exp(this.n));
  };

  Roughnum.prototype.acos = function(){
    return acos(this.n);
  };

  Roughnum.prototype.asin = function(){
    return asin(this.n);
  };

  Roughnum.prototype.round = function(){
    if (isFinite(this.n)) {
      if (Math.abs(Math.floor(this.n) - this.n) === 0.5) {
        if (Math.floor(this.n) % 2 === 0)
          return (Math.floor(this.n));
        return (Math.ceil(this.n));
      } else {
        return (Math.round(this.n));
      }
    } else {
      return this;
    }
  };

  var rationalRegexp = new RegExp("^([+-]?\\d+)/(\\d+)$");
  var digitRegexp = new RegExp("^[+-]?\\d+$");
  var flonumRegexp = new RegExp("^([-+]?)(\\d+\)((?:\\.\\d*)?)((?:[Ee][-+]?\\d+)?)$");
  var roughnumRegexp = new RegExp("^~([-+]?\\d*(?:\\.\\d*)?(?:[Ee][-+]?\\d+)?)$");
  var scientificPattern = new RegExp("^([+-]?\\d*\\.?\\d*)[Ee]([-+]?\\d+)$");

  // fromString: string -> (scheme-number | false)
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
      return Rational.makeInstance(fromString(aMatch[1]),
                                   fromString(aMatch[2]));
    }

    aMatch = x.match(flonumRegexp);
    if (aMatch) {
      var negativeP = (aMatch[1] === "-");
      //
      var beforeDecimalString = aMatch[2];
      var beforeDecimal = 0;
      if (beforeDecimalString !== '') {
        beforeDecimal = makeBignum(beforeDecimalString);
        if (negativeP) {
          beforeDecimal = - beforeDecimal;
        }
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
      //
      //var numDenGcd = _integerGcd(denominatorTen, afterDecimal);
      //var finalFracNum = _integerQuotient(afterDecimal, numDenGcd);
      //var finalDen = _integerQuotient(denominatorTen, numDenGcd);
      //if (negativeP) finalFracNum = - finalFracNum;
      //var finalNum = _integerAdd(_integerMultiply(beforeDecimal, finalDen), finalFracNum);

      var finalDen = denominatorTen;
      var finalNum = _integerAdd(_integerMultiply(beforeDecimal, denominatorTen),
                                 negativeP ? - afterDecimal : afterDecimal);
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

    aMatch = x.match(roughnumRegexp);
    if (aMatch) {
      return Roughnum.makeInstance(Number(aMatch[1]));
      //return Roughnum.makeInstance(fromString(aMatch[1]));
    }
    // if (x.match(scientificPattern)) {
    //  return Roughnum.makeInstance(Number(x));
    // }

    return false; // if all else fails

  };

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
  function bnpExp(e,z) {
    if(e > 0xffffffff || e < 1) return BigInteger.ONE;
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
  function bnModPowInt(e,m) {
    var z;
    if(e < 256 || m.isEven()) z = new Classic(m); else z = new Montgomery(m);
    return this.bnpExp(e,z);
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
  function bnPow(e) { return this.bnpExp(e,new NullExp()); }

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

  BigInteger.prototype.isReal = function() {
    return true;
  };

  BigInteger.prototype.isExact = function() {
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

  BigInteger.prototype.toExact = function() {
    return this;
  };

  BigInteger.prototype.toRational = BigInteger.prototype.toExact;

  BigInteger.prototype.toFixnum = function() {
    var str = this.toString();
    var expt = 0;
    var negativeP = false;
    var c0 = str[0];

    var i, slen, result;

    if (c0 === '-' || c0 === '+') {
      str = str.substring(1);
      if (c0 === '-') {
        negativeP = true;
      }
    }

    slen = str.length;

    for(i = slen - 1;  i >= 0; i--) {
      if (str[i] === '0') {
        expt++;
      } else {
        slen = i + 1;
        str = str.substring(0, slen);
        break;
      }
    }

    str = str + 'e' + expt;

    result = Number(str);

    if (negativeP) {
      result = - result;
    }

    return result;
  } ;

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

  // divide: scheme-number -> scheme-number
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
    // Classic implementation of Newton-Ralphson square-root search,
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

    // integerSqrt: -> scheme-number
    BigInteger.prototype.integerSqrt = function() {
      var n;
      if(sign(this) >= 0) {
        return searchIter(this, this);
      } else {
        throwRuntimeError('integerSqrt of negative bignum', this);
      }
    };
  })();

  (function() {
    // Get an approximation using integerSqrt, and then start another
    // Newton-Ralphson search if necessary.
    BigInteger.prototype.sqrt = function() {
      var approx = this.integerSqrt(), fix;
      if (eqv(sqr(approx), this)) {
        return approx;
      }
      fix = toFixnum(this);
      if (isFinite(fix)) {
        if (fix >= 0) {
          return Roughnum.makeInstance(Math.sqrt(fix));
        } else {
          throwRuntimeError('sqrt of negative bignum', fix);
        }
      } else {
        return approx;
      }
    };
  })();

  // sqrt: -> scheme-number
  // http://en.wikipedia.org/wiki/Newton's_method#Square_root_of_a_number
  // Produce the square root.

  // floor: -> scheme-number
  // Produce the floor.
  BigInteger.prototype.floor = function() {
    return this;
  }

  // ceiling: -> scheme-number
  // Produce the ceiling.
  BigInteger.prototype.ceiling = function() {
    return this;
  }

  // log: -> scheme-number
  // Produce the log.
  BigInteger.prototype.log = function(n) {
    return log(this.toFixnum());
  };

  // tan: -> scheme-number
  // Produce the tan.
  BigInteger.prototype.tan = function(n) {
    return tan(this.toFixnum());
  };

  // atan: -> scheme-number
  // Produce the arc tangent.
  BigInteger.prototype.atan = function(n) {
    return atan(this.toFixnum());
  };

  // cos: -> scheme-number
  // Produce the cosine.
  BigInteger.prototype.cos = function(n) {
    return cos(this.toFixnum());
  };

  // sin: -> scheme-number
  // Produce the sine.
  BigInteger.prototype.sin = function(n) {
    return sin(this.toFixnum());
  };

  // expt: scheme-number -> scheme-number
  // Produce the power to the input.
  BigInteger.prototype.expt = function(n) {
    return bnPow.call(this, n);
  };

  // exp: -> scheme-number
  // Produce e raised to the given power.
  BigInteger.prototype.exp = function() {
    var x = this.toFixnum();
    return Roughnum.makeInstance(Math.exp(x));
  };

  // acos: -> scheme-number
  // Produce the arc cosine.
  BigInteger.prototype.acos = function(n) {
    return acos(this.toFixnum());
  };

  // asin: -> scheme-number
  // Produce the arc sine.
  BigInteger.prototype.asin = function(n) {
    return asin(this.toFixnum());
  };

  // round: -> scheme-number
  // Round to the nearest integer.
  BigInteger.prototype.round = function(n) {
    return round(this.toFixnum());
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

    return function(n, d, options) {
      // default limit on decimal expansion; can be overridden
      var limit = 512;
      if (options && typeof(options.limit) !== 'undefined') {
        limit = options.limit;
      }
      if (! isInteger(n)) {
        throwRuntimeError('toRepeatingDecimal: n ' + n.toString() +
                          " is not an integer.");
      }
      if (! isInteger(d)) {
        throwRuntimeError('toRepeatingDecimal: d ' + d.toString() +
                          " is not an integer.");
      }
      if (equals(d, 0)) {
        throwRuntimeError('toRepeatingDecimal: d equals 0');
      }
      if (lessThan(d, 0)) {
        throwRuntimeError('toRepeatingDecimal: d < 0');
      }
      var sign = (lessThan(n, 0) ? "-" : "");
      n = abs(n);
      var beforeDecimalPoint = sign + quotient(n, d);
      var afterDecimals = getResidue(remainder(n, d), d, limit);
      return [beforeDecimalPoint].concat(afterDecimals);
    };
  })();
  //////////////////////////////////////////////////////////////////////

  // External interface of js-numbers:

  Numbers['fromFixnum'] = fromFixnum;
  Numbers['fromString'] = fromString;
  Numbers['makeBignum'] = makeBignum;
  Numbers['makeRational'] = Rational.makeInstance;
  Numbers['makeRoughnum'] = Roughnum.makeInstance;

  Numbers['onThrowRuntimeError'] = onThrowRuntimeError;
  Numbers['isSchemeNumber'] = isSchemeNumber;
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
  Numbers['log'] = log;
  Numbers['tan'] = tan;
  Numbers['atan'] = atan;
  Numbers['cos'] = cos;
  Numbers['sin'] = sin;
  Numbers['tan'] = tan;
  Numbers['acos'] = acos;
  Numbers['asin'] = asin;
  Numbers['cosh'] = cosh;
  Numbers['sinh'] = sinh;
  Numbers['round'] = round;
  Numbers['sqr'] = sqr;
  Numbers['gcd'] = gcd;
  Numbers['lcm'] = lcm;

  Numbers['toRepeatingDecimal'] = toRepeatingDecimal;

  // The following exposes the class representations for easier
  // integration with other projects.
  Numbers['BigInteger'] = BigInteger;
  Numbers['Rational'] = Rational;
  Numbers['Roughnum'] = Roughnum;

  Numbers['MIN_FIXNUM'] = MIN_FIXNUM;
  Numbers['MAX_FIXNUM'] = MAX_FIXNUM;

  return Numbers;
});
