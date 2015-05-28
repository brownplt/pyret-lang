#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(append-gen-docs
  '(module "numbers"
    (path "src/js/base/runtime-anf.js")
    (data-spec
      (name "Number")
      (variants)
      (shared))
    (data-spec
      (name "Exactnum")
      (variants)
      (shared))
    (data-spec
      (name "Roughnum")
      (variants)
      (shared))
    (data-spec
      (name "NumInteger")
      (variants)
      (shared))
    (data-spec
      (name "NumRational")
      (variants)
      (shared))
    (data-spec
      (name "NumPositive")
      (variants)
      (shared))
    (data-spec
      (name "NumNegative")
      (variants)
      (shared))
    (data-spec
      (name "NumNonPositive")
      (variants)
      (shared))
    (data-spec
      (name "NumNonNegative")
      (variants)
      (shared))
  (fun-spec
    (name "num-equal")
    (arity 2)
    (args ("n1" "n2"))
    (doc ""))
  (fun-spec
    (name "num-max")
    (arity 2)
    (args ("n1" "n2"))
    (doc ""))
  (fun-spec
    (name "num-min")
    (arity 2)
    (args ("n1" "n2"))
    (doc ""))
  (fun-spec
    (name "num-abs")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-sin")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-cos")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-tan")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-asin")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-acos")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-atan")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-modulo")
    (arity 2)
    (args ("n" "divisor"))
    (doc ""))
  (fun-spec
    (name "num-truncate")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-sqrt")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-sqr")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-ceiling")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-floor")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-round")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-round-even")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-log")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-exp")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-expt")
    (arity 2)
    (args ("base" "exponent"))
    (doc ""))
  (fun-spec
    (name "num-to-rational")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-to-roughnum")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-is-integer")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-is-rational")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-is-roughnum")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-is-positive")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-is-negative")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-is-non-positive")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-is-non-negative")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-to-string")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-to-string-digits")
    (arity 2)
    (args ("n" "digits"))
    (doc ""))
  (fun-spec
    (name "num-within-abs")
    (arity 1)
    (args ("tol"))
    (doc ""))
  (fun-spec
    (name "num-within-rel")
    (arity 1)
    (args ("tol"))
    (doc ""))
    (fun-spec
      (name "within-abs")
      (arity 1)
      (args ("tol"))
      (doc ""))
    (fun-spec
      (name "within-abs-now")
      (arity 1)
      (args ("tol"))
      (doc ""))
    (fun-spec
      (name "within")
      (arity 1)
      (args ("tol"))
      (doc ""))
    (fun-spec
      (name "within-rel")
      (arity 1)
      (args ("tol"))
      (doc ""))
    (fun-spec
      (name "within-rel-now")
      (arity 1)
      (args ("tol"))
      (doc ""))
    (fun-spec
      (name "within-abs3")
      (arity 1)
      (args ("tol"))
      (return ,eq3fun)
      (doc ""))
    (fun-spec
      (name "within-abs-now3")
      (arity 1)
      (args ("tol"))
      (return ,eq3fun)
      (doc ""))
    (fun-spec
      (name "within-rel3")
      (arity 1)
      (args ("tol"))
      (return ,eq3fun)
      (doc ""))
    (fun-spec
      (name "within-rel-now3")
      (arity 1)
      (args ("tol"))
      (return ,eq3fun)
      (doc ""))
  (fun-spec
    (name "num-random")
    (arity 1)
    (args ("max"))
    (doc ""))
  (fun-spec
    (name "num-random-seed")
    (arity 1)
    (args ("seed"))
    (doc ""))
  (fun-spec
    (name "num-is-fixnum")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-exact")
    (arity 1)
    (args ("n"))
    (doc ""))
    ))

@docmodule["numbers" #:noimport #t #:friendly-title "Numbers"]{

Pyret numbers are of two kinds: exact
numbers and rough numbers (``roughnums''). Both are to base ten;
real; and finite.

Exact numbers are arbitrarily precise rational numbers: these
include integers and rational fractions.  For integers whose 
magnitude is less than @pyret{(2^53 - 1)},
Pyret internally uses JavaScript
fixnums, in order to optimize basic arithmetic.

Roughnums are numbers that are necessarily or
deliberately imprecise. These correspond to the same set of
values covered by JavaScript
fixnums (a.k.a. doubles), and thus cover a large but limited range
(magnitude less than @pyret{1.7976931348623157e308}).

Operations on exact numbers typically return
exacts. However, if the operation can yield irrationals, and it
is not possible to determine that a particular result is
definitely rational, that result is returned as a roughnum. Thus,
trigonometric functions on exact numbers typically yield roughnum
answers, except for well-known edge cases such as the sine or
cosine of zero. Fractional powers of rationals are usually roughnum,
except for small roots where it can be ascertained that an exact
root is possible.

Operations that are non-casting and with at least one argument that is roughnum
automatically coerce the result to be a roughnum. This is known
as roughnum contagion.

Exact numbers allow the usual comparison predicates. Roughnums do
too, with the significant exception that trying to compare
roughnums for equality throws an error.

An operation whose numerical result is not determinate or finite
throws an error, with the message signaling either an
overflow or some more specific problem. 

@section{Number Annotations}

@type-spec["Number" (list)]
The type of number values
@type-spec["Exactnum" (list)]
The type of exact number values
@type-spec["Roughnum" (list)]
The type of roughnum values
@type-spec["NumInteger" (list)]
The type of exact integer values
@type-spec["NumRational" (list)]
The type of exact rational number values. Same as @pyret{Exactnum}.
@type-spec["NumPositive" (list)]
The type of number values that are greater than zero
@type-spec["NumNegative" (list)]
The type of number values that are less than zero
@type-spec["NumNonPositive" (list)]
The type of number values that are less than or equal to zero
@type-spec["NumNonNegative" (list)]
The type of number values that are equal to or greater than zero

     @section{Number Literals}

Literal exact rationals can be integers,  fractions represented
with a solidus, or decimals, with an optional exponent. In the following, the numerals on
the same line denote the same Pyret number.

@examples{
42 +42
-42
22/7
-22/7
2.718281828 +2.718281828
-2.718281828
1/2 0.5
6.022e23 +6.022e23 6.022e+23 +6.022e+23
-6.022e23 -6.022e+23
-6.022e-23
}

Exact numbers are of arbitrary precision.

Literal roughnums are represented with a leading tilde. They are
either integers or decimals, with an optional exponent.

@examples{
~42 ~+42
~-42
~2.718281828 ~+2.718281828
~-2.718281828
~6.022e23 ~+6.022e23 ~6.022e+23 ~+6.022e+23
~-6.022e23 ~-6.022e+23
~-6.022e-23
}

Roughnums cannot be made arbitrarily precise. The absolute value
ranges between 0 and
1.7976931348623157e+308 (JS’s Number.MAX_VALUE) with a
granularity of 5e-324 (JS’s Number.MIN_VALUE).

     @section{Number Functions}
  @function["num-equal" #:contract (a-arrow N N B)]{
If both arguments are exact, returns a boolean.
If either argument is roughnum, raises an error.

@examples{
check:
  num-equal(2, 2) is true
  num-equal(2, 3) is false
  num-equal(1/2, 0.5) is true
  num-equal(1 / 2, 0.5) is true
  num-equal(1/3, 0.33) is false
  num-equal(1/3, ~0.33)
    raises "roughnums cannot be compared for equality"
end
}

Throws an error on non-numeric
arguments, which can be a useful alternative to @pyret-id["equal-always"
"equality"] in situations where the program shouldn't compare non-numbers.

  }
  @function["num-max" #:contract (a-arrow N N N) #:return N]{
Returns the larger of the two arguments.

@examples{
check:
  num-max(1, 2) is 2
  num-max(2, ~3) is ~3
  num-max(4, ~4) is ~4
  num-max(~4, 4) is 4
  num-max(-1.1, 0) is 0
end
}

  }
  @function["num-min" #:contract (a-arrow N N N) #:return N]{
Returns the smaller of the two arguments.

@examples{
check:
  num-min(1, 2) is 1
  num-min(2, ~3) is 2
  num-min(4, ~4) is ~4
  num-min(~4, 4) is 4
  num-min(-1.1, 0) is -1.1
end
}

  }
  @function["num-abs" #:contract (a-arrow N N) #:return N]{
Returns the absolute value of the argument. The result is exact only if the argument is.

@examples{
check:
  num-abs(2) is 2
  num-abs(-2.1) is 2.1
  num-abs(~2) is ~2
  num-abs(~-2.1) is ~2.1
end
}

  }
  @function["num-sin" #:contract (a-arrow N N) #:return N]{

Returns the sine of the argument, usually as a roughnum. If the argument is exact 0, the result is exact 0 too.

@examples{
check:
  num-sin(0) is 0
  num-sin(1) is%(within-abs(0.01)) 0.84
end
}
  }
  @function["num-cos" #:contract (a-arrow N N) #:return N]{

Returns the cosine of the argument, usually  as a roughnum. If
the argument is exact 0, the result is exact 1.

@examples{
check:
  num-cos(0) is 1
  num-cos(1) is%(within-abs(0.01)) 0.54
end
}
  }
  @function["num-tan" #:contract (a-arrow N N) #:return N]{
Returns the tangent of the argument, usually as a roughnum. However, if
the argument is exact 0, the result is exact 1.

@examples{
check:
  num-tan(0) is 0
  num-tan(1) is%(within-abs(0.01)) 1.56
end
}

  }
  @function["num-asin" #:contract (a-arrow N N) #:return N]{
Returns the arc sine of the argument, usually as a roughnum. However, if
the argument is exact 0, the result is exact 0.

@examples{
check:
  num-asin(0) is 0
  num-asin(0.84) is%(within-abs(0.01)) 1
end
}

  }
  @function["num-acos" #:contract (a-arrow N N)]{

Returns the arc cosine of the argument, usually as a roughnum. However, if
the argumet is exact 1, the result is exact 0.

@examples{
check:
  num-acos(1) is 0
  num-acos(0.54) is%(within-abs(0.01)) 1
end
}
  }
  @function["num-atan" #:contract (a-arrow N N) #:return N]{

Returns the arc tangent of the argument, usually as a roughnum. However, if
the argumet is exact 0, the result is exact 0.

@examples{
check:
  num-atan(0) is 0
  num-atan(1.56) is%(within-abs(0.01)) 1
end
}
  }
  @function["num-modulo" #:contract (a-arrow N N N) #:return N]{
Returns the modulo of the first argument with respect to the
second.

@examples{
check:
  num-modulo(5, 2) is 1
  num-modulo(-5, 2) is 1
  num-modulo(-5, -2) is -1
  num-modulo(7, 3) is 1
  num-modulo(0, 5) is 0
  num-modulo(-7, 3) is 2
end
}

It is useful for calculating if one number is a multiple of
another, by checking for a zero remainder.

@examples{
fun is-odd(n :: Number) -> Boolean:
  num-modulo(n, 2) == 0
where:
  is-odd(6) is true
  is-odd(3) is false
end
}

  }
  @function["num-truncate" #:contract (a-arrow N N) #:return N]{

Returns the integer part of its argument by cutting off any
decimal part. Does not do any rounding.

@examples{
check:
  num-truncate(3.14) is 3
  num-truncate(-3.14) is -3
  num-truncate(~3.14) is ~3
  num-truncate(~-3.14) is ~-3
end
}

  }
  @function["num-sqrt" #:contract (a-arrow N N) #:return N]{

Returns the square root.  If the argument is exact and a perfect
square, the result is exact.

@examples{
check:
  num-sqrt(4) is 2
  num-sqrt(5) is%(within-abs(0.001)) ~2.236
  num-sqrt(~4) is ~2
  num-sqrt(~5) is%(within-abs(0.001)) ~2.236
  num-sqrt(0.04) is 1/5
  num-sqrt(-1) raises "negative argument"
end
}
  }
  @function["num-sqr" #:contract (a-arrow N N) #:return N]{

Returns the square.

@examples{
check:
  num-sqr(4) is 16
  num-sqr(5) is 25
  num-sqr(-4) is 16
  num-sqr(~4) is ~16
  num-sqr(0.04) is 1/625
end
}

  }
  @function["num-ceiling" #:contract (a-arrow N N) #:return N]{

Returns the smallest integer greater than or equal to the
argument.
The result is exact even if the argument is rough.

@examples{
check:
  num-ceiling(4.2) is 5
  num-ceiling(-4.2) is -4
end
}

  }
  @function["num-floor" #:contract (a-arrow N N) #:return N]{

Returns the largest integer less than or equal to the argument.
The result is exact even if the argument is rough.

@examples{
check:
  num-floor(4.2) is 4
  num-floor(-4.2) is -5
end
}
  }
  @function["num-round" #:contract (a-arrow N N) #:return N]{

Returns the closest integer to the argument. The result is exact
even if
the argument is rough.

@examples{
check:
  num-round(4.2) is 4
  num-round(4.8) is 5
  num-round(-½4.2) is -4
  num-round(-4.8) is -5
end
}

If the argument is midway between integers, returns the integer
away from zero.

@examples{
check:
  num-round(3.5) is 4
  num-round(2.5) is 3
end
}

  }
  @function["num-round-even" #:contract (a-arrow N N) #:return N]{

Similar to @pyret{num-round}, except that if the argument is
midway between integers, returns the even integer.

@examples{
check:
  num-round-even(3.5) is 4
  num-round-even(2.5) is 2
end
}

  }  @function["num-log" #:contract (a-arrow N N) #:return N]{

Returns the natural logarithm (ln) of the argument, usually as a roughnum.
However, if the argument is exact 1, the
result is exact 0. If the argument is non-positive, an error is
thrown.

@examples{
check:
  num-log(1) is 0
  num-log(0) raises "non-positive argument"
  num-log(-1) raises "non-positive argument"
  num-log(2.718281828) is%(within-abs(0.01)) 1
  num-log(10) is%(within-abs(0.1)) 2.3
end
}

  }
  @function["num-exp" #:contract (a-arrow N N) #:return N]{

Returns e raised to the argument, usually as a roughnum.  However, if the
argument is exact 0, the result is
exact 1.

@examples{
check:
  num-exp(-1) is%(within-abs(0.0001)) (1 / num-exp(1))
  num-exp(0) is 1
  num-exp(1) is%(within-abs(0.0001)) 2.718281828
  num-exp(3) is%(within-abs(0.0001)) num-expt(2.718281828, 3)
  num-exp(710) raises "overflow"
end
}

  }
  @function["num-expt" #:contract (a-arrow N N N) #:return N]{

Returns the first argument raised to the second argument. The result is exact
if both arguments are exact, except that an error is thrown if
the first argument is zero and the second is negative.
Furthermore, if the first argument is exact 0 or 1,
or the second argument is exact 0, then the result is exact even if the other
argument is rough.

@examples{
check:
  num-expt(3, 0) is 1
  num-expt(1, 3) is 1
  num-expt(0, 0) is 1
  num-expt(0, 3) is 0
  num-expt(0, -3) raises "division by zero"
  num-expt(2, 3) is 8
  num-expt(2, -3) is 1/8
end
}

  }
  @function["num-to-rational" #:contract (a-arrow N N) #:return N]{

Same as @pyret{num-exact}.
first argumetn is zero and the second is negative.
  }
  @function["num-to-roughnum" #:contract (a-arrow N N) #:return N]{

Given an exact num, returns the roughnum version of it. Given a
roughnum, returns it directly.

@examples{
check:
  num-is-roughnum(num-to-roughnum(3.14)) is true
  num-is-roughnum(num-to-roughnum(~3.14)) is true
end
}
  }
  @function["num-is-integer" #:contract (a-arrow N B) #:return B]{
Returns true if argument is an exact integer.

@examples{
check:
  num-is-integer(2) is true
  num-is-integer(1/2) is false
  num-is-integer(1.609) is false
  num-is-integer(~2) is false
end
}

  }
  @function["num-is-rational" #:contract (a-arrow N B) #:return B]{

Returns true if argument is an exact rational.

@examples{
check:
  num-is-rational(2) is true
  num-is-rational(1/2) is true
  num-is-rational(1.609) is true
  num-is-rational(~2) is false
end
}

  }
  @function["num-is-roughnum" #:contract (a-arrow N B) #:return B]{
Returns true if argument is a roughnum.
@examples{
check:
  num-is-roughnum(2) is false
  num-is-roughnum(1/2) is false
  num-is-roughnum(1.609) is false
  num-is-roughnum(~2) is true
end
}

  }
  @function["num-is-positive" #:contract (a-arrow N B) #:return B]{

Returns true if argument is greater than zero.

@examples{
check:
  num-is-positive(~-2) is false
  num-is-positive(-2) is false
  num-is-positive(0) is false
  num-is-positive(-0) is false
  num-is-positive(2) is true
  num-is-positive(~2) is true
end
}
  }
  @function["num-is-negative" #:contract (a-arrow N B) #:return B]{

Returns true if argument is less than zero.

@examples{
check:
  num-is-negative(~-2) is true
  num-is-negative(-2) is true
  num-is-negative(0) is false
  num-is-negative(-0) is false
  num-is-negative(2) is false
  num-is-negative(~2) is false
end
}

  }
  @function["num-is-non-positive" #:contract (a-arrow N B) #:return B]{

Returns true if argument is less than or equal to zero.
@examples{
check:
  num-is-non-positive(~-2) is true
  num-is-non-positive(-2) is true
  num-is-non-positive(0) is true
  num-is-non-positive(-0) is true
  num-is-non-positive(2) is false
  num-is-non-positive(~2) is false
end
}

  }
  @function["num-is-non-negative" #:contract (a-arrow N B) #:return B]{

Returns true if argument is greater than or equal to zero.

@examples{
check:
  num-is-non-negative(~-2) is false
  num-is-non-negative(-2) is false
  num-is-non-negative(0) is true
  num-is-non-negative(-0) is true
  num-is-non-negative(2) is true
  num-is-non-negative(~2) is true
end
}
  }
  @function["num-to-string" #:contract (a-arrow N S) #:return S]{
Returns a string representing a literal form of the number.

@examples{
check:
  num-to-string(2.5) is "5/2"
  num-to-string(2) is "2"
  num-to-string(2/3) is "2/3"
  num-to-string(~2.718) is "~2.718"
  num-to-string(~6.022e23) is "~6.022e+23"
end
}
  }
  @function["num-to-string-digits" #:contract (a-arrow N N S) #:return S]{

Converts the number to a string, providing @pyret{digits} precision in the
output.  If @pyret{digits} is positive, provides that many digits to the right
of the decimal point (including adding zeroes beyond the actual precision of
the number).  If @pyret{digits} is negative, rounds that many positions to the
@emph{left} of the decimal, replacing them with zeroes.

Note that @pyret-id{num-to-string-digits} is only for formatting, and its
output's apparent precision may be unrelated to the actual precision of the
input number, which may have been an approximation, or unrepresentable in
decimal.

@examples{
check:
  num-to-string-digits(5432.1234, 2) is "5432.12"
  num-to-string-digits(0.123456789, 2) is "0.12"
  num-to-string-digits(5, 2) is "5.00"
  num-to-string-digits(555, -2) is "500"
end
}
  }
  @function["num-within-abs" #:contract (a-arrow N A)]{

Returns a predicate that checks if the difference of its two
arguments is less than @pyret{tol}.

@examples{
check:
   1  is%(num-within-abs(0.1))       1
   1  is%(num-within-abs(0.1))      ~1
  ~3  is%(num-within-abs(0.1))      ~3
  ~2  is-not%(num-within-abs(0.1))  ~3
  ~2  is%(num-within-abs(1.1))      ~3
  ~2  is-not%(num-within-abs(~1))   ~3
   2  is-not%(num-within-abs(1))    ~3
   5  is%(num-within-abs(4))         3

   num-within-abs(-0.1)(1, 1.05) raises "negative tolerance"
end
}

  }
  @function["num-within-rel" #:contract (a-arrow N A)]{

Returns a predicate that checks if the relative difference of its two
number arguments is less than @pyret{tol}.

This function is a.k.a. @pyret{num-within}.

@examples{
check:
  100000 is%(num-within-rel(0.1)) 95000
  100000 is-not%(num-within-rel(0.1)) 85000
end
}
  }

  @function["within" #:contract (a-arrow N A)]
  @function["within-abs" #:contract (a-arrow N A)]
  @function["within-rel" #:contract (a-arrow N A)]
  @function["within-abs-now" #:contract (a-arrow N A)]
  @function["within-rel-now" #:contract (a-arrow N A)]

  These comparison functions compare both numbers and structures, and are
  documented in @seclink["s:bounded-equalities"].

@section{Random Numbers}

  @function["num-random" #:contract (a-arrow N N) #:return N]{

  Returns a pseudo-random positive integer from @pyret{0} to @pyret{max - 1}.

@examples{
check:
  fun between(min, max):
    lam(v): (v >= min) and (v <= max) end
  end
  for each(i from range(0, 100)):
    n = num-random(i)
    print(n)
    n satisfies between(0, i - 1)
  end
end
}

  }
  @function["num-random-seed" #:contract (a-arrow N No) #:return No]{

  Sets the random seed.  Setting the seed to a particular number makes all
  future uses of random produce the same sequence of numbers.  Useful for
  testing and debugging functions that have random behavior.

  @examples{
check:
  num-random-seed(0)
  n = num-random(1000)
  n2 = num-random(1000)

  n is-not n2

  num-random-seed(0)
  n3 = num-random(1000)
  n3 is n
  n4 = num-random(1000)
  n4 is n2
end
}
  }

@section{Other Number Functions}

  A few other number functions are useful in limited cases that don't come up
  in most programs.

  @function["num-is-fixnum" #:contract (a-arrow N B) #:return B]{

Returns true if the argument is represented directly as a
primitive
JavaScript number (i.e., JavaScript double).

@examples{
check:
  num-is-fixnum(10) is true
  num-is-fixnum(~10) is false
  num-is-fixnum(1000000000000000) is true
  num-is-fixnum(10000000000000000) is false
  num-is-fixnum(1.5) is false
end
}

N.B. Pyret representes exact rationals that are non-integers as tuples, and hence
even small rationals such as 1.5 are considered non-fixnum,
although they could be represented as JavaScript doubles.

  }
  @function["num-exact" #:contract (a-arrow N N) #:return N]{

Given a roughnum, returns an exact number most equal to it. Given
an exact num, returns it directly.

@examples{
check:
  num-sqrt(2) is%(within-abs(0.000001)) ~1.4142135623730951
  num-exact(num-sqrt(2)) is 1767766952966369/1250000000000000
end
}
  }
}
