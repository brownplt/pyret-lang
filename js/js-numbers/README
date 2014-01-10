js-numbers: a Javascript implementation of Scheme's numeric tower

Developer:
    Danny Yoo (dyoo@cs.wpi.edu)

License:
    BSD

Summary: js-numbers implements the "numeric tower" commonly associated
with the Scheme language.  The operations in this package
automatically coerse between fixnums, bignums, rationals, floating
point, and complex numbers.


Contributors: I want to thank the following people:

    Zhe Zhang
    Ethan Cecchetti
    Ugur Cekmez


Other sources:
    The bignum implementation (content from jsbn.js and jsbn2.js) used
    in js-numbers comes from Tom Wu's JSBN library at:

    http://www-cs-students.stanford.edu/~tjw/jsbn/



======================================================================

WARNING WARNING

This package is currently being factored out of an existing project,
Moby-Scheme.  As such, the code here is in major flux, and this is
nowhere near ready from public consumption yet.  We're still in the
middle of migrating over the test cases from Moby-Scheme over to this
package, and furthermore, I'm taking the time to redo some of the
implementation.  So this is going to be buggy for a bit.  Use at your
own risk.


======================================================================
Examples



[fill me in]


======================================================================
API


Loading js-numbers.js will define a toplevel namespace called

    jsnums

which contains following constants and functions:



pi: scheme-number

e: scheme-number

nan: scheme-number
    Not-A-Number

inf: scheme-number
    infinity

negative_inf: scheme-number
    negative infinity

negative_zero: scheme-number
    The value -0.0.

zero: scheme-number

one: scheme-number

negative_one: scheme-number

i: scheme-number
    The square root of -1.

negative_i: scheme-number
    The negative of i.

fromString: string -> (scheme-number | false)
    Convert from a string to a scheme-number.  If we find the number is malformed,
    returns false.

fromFixnum: javascript-number -> scheme-number
    Convert from a javascript number to a scheme number.  If the
    number looks like an integer, represents as an exact integer.
    Otherwise, represents as a float.  If you need more precision over
    the representation, use makeFloat or makeRational instead.

makeRational: javascript-number javascript-number? -> scheme-number
    Low level constructor: Constructs a rational with the given
    numerator and denominator.  If only one argument is given, assumes
    the denominator is 1.  The numerator and denominator must be
    integers.

makeFloat: javascript-number -> scheme-number
    Low level constructor: constructs a floating-point number.

makeBignum: string -> scheme-number
    Low level constructor: constructs a bignum.

makeComplex: scheme-number scheme-number? -> scheme-number
    Constructs a complex number; the real and imaginary parts of the
    input must be basic scheme numbers (i.e. not complex).  If only one
    argument is given, assumes the imaginary part is 0.

makeComplexPolar: scheme-number scheme-number -> scheme-number
    Constructs a complex number; the radius and theta must be basic
    scheme numbers (i.e. not complex).

isSchemeNumber: any -> boolean
    Produces true if the thing is a scheme number.

isRational: scheme-number -> boolean
    Produces true if the number is rational.

isReal: scheme-number -> boolean
    Produces true if the number is a real.

isExact: scheme-number -> boolean
    Produces true if the number is being represented exactly.

isInexact: scheme-number -> boolean
    Produces true if the number is inexact.

isInteger: scheme-number -> boolean
    Produces true if the number is an integer.

toFixnum: scheme-number -> javascript-number
    Produces the javascript number closest in interpretation to the
    given scheme-number.

toExact: scheme-number -> scheme-number
    Converts the number to an exact scheme-number.

toInexact: scheme-number -> scheme-number
    Converts the number to an inexact scheme-number.

add: scheme-number scheme-number -> scheme-number
    Adds the two numbers together.

subtract: scheme-number scheme-number -> scheme-number
    Subtracts the first number from the second.

mulitply: scheme-number scheme-number -> scheme-number
    Multiplies the two numbers together.

divide: scheme-number scheme-number -> scheme-number
    Divides the first number by the second.

equals: scheme-number scheme-number -> boolean
    Produces true if the two numbers are equal.

eqv: scheme-number scheme-number -> boolean
    Produces true if the two numbers are equivalent.

approxEquals: scheme-number scheme-number scheme-number -> boolean
    Produces true if the two numbers are approximately equal, within the
    bounds of the third argument.

greaterThanOrEqual: scheme-number scheme-number -> boolean
    Produces true if the first number is greater than or equal to the second.

lessThanOrEqual: scheme-number scheme-number -> boolean
    Produces true if the first number is less than or equal to the second.

greaterThan: scheme-number scheme-number -> boolean
    Produces true if the first number is greater than the second.

lessThan: scheme-number scheme-number -> boolean
    Produces true if the first number is less than the second.

expt: scheme-number scheme-number -> scheme-number
    Produces the first number exponentiated to the second number.

exp: scheme-number -> scheme-number
    Produces e exponentiated to the given number.

modulo: scheme-number scheme-number -> scheme-number
    Produces the modulo of the two numbers.

numerator: scheme-number -> scheme-number
    Produces the numerator of the rational number.

denominator: scheme-number -> scheme-number
    Produces the denominator of the rational number.

quotient: scheme-number scheme-number -> scheme-number
    Produces the quotient.  Both inputs must be integers.

remainder: scheme-number scheme-number -> scheme-number
    Produces the remainder.  Both inputs must be integers.

sqrt: scheme-number -> scheme-number
    Produces the square root.

abs: scheme-number -> scheme-number
    Produces the absolute value.

floor: scheme-number -> scheme-number
    Produces the floor.

round: scheme-number -> scheme-number
    Produces the number rounded to the nearest integer.

ceiling: scheme-number -> scheme-number
    Produces the ceiling.

conjugate: scheme-number -> scheme-number
    Produces the complex conjugate.

magnitude: scheme-number -> scheme-number
    Produces the complex magnitude.

log: scheme-number -> scheme-number
    Produces the natural log (base e) of the given number.

angle: scheme-number -> scheme-number
    Produces the complex angle.

cos: scheme-number -> scheme-number
    Produces the cosine.

sin: scheme-number -> scheme-number
    Produces the sin.

tan: scheme-number -> scheme-number
    Produces the tangent.

asin: scheme-number -> scheme-number
    Produces the arc sine.

acos: scheme-number -> scheme-number
    Produces the arc cosine.

atan: scheme-number -> scheme-number
    Produces the arc tangent.

cosh: scheme-number -> scheme-number
    Produces the hyperbolic cosine.

sinh: scheme-number -> scheme-number
    Produces the hyperbolic sine.

realPart: scheme-number -> scheme-number
    Produces the real part of the complex number.

imaginaryPart: scheme-number -> scheme-number
    Produces the imaginary part of the complex number.

sqr: scheme-number -> scheme-number
    Produces the square.

integerSqrt: scheme-number -> scheme-number
    Produces the integer square root.

gcd: scheme-number [scheme-number ...] -> scheme-number
    Produces the greatest common divisor.

lcm: scheme-number [scheme-number ...] -> scheme-number
    Produces the least common mulitple.


toRepeatedDecimal: scheme-number scheme-number {limit: number}? -> [string, string, string]
    Produces a string representation of the decimal expansion; the first and second
    argument must be integers.  Returns an array of three parts: the portion before
    the decimal, the non-repeating part, and then the repeating part. 

    If the expansion goes beyond the limit (by default, 256 characters), then
    the expansion will be cut off, and the third portion will be '...'.


======================================================================
Test suite

Open tests/index.html, which should run our test suite over all the
public functions in js-numbers.

If you notice a good test case is missing, please let the developer
know, and we'll be happy to add it in.



======================================================================
TODO

* Absorb implementations of:

  atan2, cosh, sinh, sgn

* Add real documentation.


======================================================================

Related work

There appears to be another Scheme numeric tower implementation that
just came out in the last month or so, by Matt Might and John Tobey:

    https://github.com/jtobey/javascript-bignum
    http://silentmatt.com/biginteger/




======================================================================
History

February 2010: initial refactoring from the moby-scheme source tree.

June 2010: got implementation of integer-sqrt from Ugur Cekmez;
brought in some fixes from Ethan Cecchetti.