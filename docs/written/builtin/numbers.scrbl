#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(append-gen-docs
  '(module "numbers"
    (path "src/js/base/runtime-anf.js")
    (data-spec
      (name "Number")
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
    (name "num-exact")
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
    (name "num-is-fixnum")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-tostring")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-within")
    (arity 1)
    (args ("tol"))
    (doc ""))
  (fun-spec
    (name "num-within-rel")
    (arity 1)
    (args ("tol"))
    (doc ""))
  (fun-spec
    (name "within")
    (arity 1)
    (args ("tol"))
    (doc ""))
  (fun-spec
    (name "within-now")
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
))

@docmodule["numbers" #:noimport #t #:friendly-title "Numbers"]{
   @type-spec["Number" (list)]
       The type of number values

@type-spec["Exactnum" (list)]
The type of exact number values
@type-spec["Roughnum" (list)]
The type of roughnum values
@type-spec["NumInteger" (list)]
The type of exact integer values
@type-spec["NumRational" (list)]
The type of exact rational number values
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
If either argument is roughnum, raises an exception.

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

  }
  @function["num-max" #:contract (a-arrow N N N)]{
Returns the larger of the two arguments.

@examples{
check:
  num-max(1, 2) is 2
  num-max(2, ~3) is ~3
  num-max(4, ~4) is ~4
  num-max(~4, 4) is 4
end
}

  }
  @function["num-min" #:contract (a-arrow N N N)]{
Returns the smaller of the two arguments.

@examples{
check:
  num-min(1, 2) is 1
  num-min(2, ~3) is 2
  num-min(4, ~4) is ~4
  num-min(~4, 4) is 4
end
}

  }
  @function["num-abs" #:contract (a-arrow N N)]{
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
  @function["num-sin" #:contract (a-arrow N N)]{

Returns the sine of the argument  as a roughnum. If the argument is exact 0, the result is exact 0 too.

@examples{
check:
  num-sin(0) is 0
  num-sin(1) is%(within(0.01)) 0.84
end
}
  }
  @function["num-cos" #:contract (a-arrow N N)]{

Returns the cosine of the argument  as a roughnum. If
the argument is exact 0, the result is exact 1.

@examples{
check:
  num-cos(0) is 1
  num-cos(1) is%(within(0.01)) 0.54
end
}
  }
  @function["num-tan" #:contract (a-arrow N N)]{
Returns the tangent of the argument as a roughnum. However, if
the argument is exact 0, the result is exact 1.

@examples{
check:
  num-tan(0) is 0
  num-tan(1) is%(within(0.01)) 1.56
end
}

  }
  @function["num-asin" #:contract (a-arrow N N)]{
Returns the arc sine of the argument as a roughnum. However, if
the argument is exact 0, the result is exact 0.

@examples{
check:
  num-asin(0) is 0
  num-asin(0.84) is%(within(0.01)) 1
end
}

  }
  @function["num-acos" #:contract (a-arrow N N)]{

Returns the arc cosine of the argument as a roughnum. However, if
the argumet is exact 1, the result is exact 0.

@examples{
check:
  num-acos(1) is 0
  num-acos(0.54) is%(within(0.01)) 1
end
}
  }
  @function["num-atan" #:contract (a-arrow N N)]{

Returns the arc tangent of the argument as a roughnum. However, if
the argumet is exact 0, the result is exact 0.

@examples{
check:
  num-atan(0) is 0
  num-atan(1.56) is%(within(0.01)) 1
end
}
  }
  @function["num-modulo" #:contract (a-arrow N N N)]{
Returns the modulo of the first argument with respect to the
second.

@examples{
check:
  num-modulo(5, 2) is 1
  num-modulo(-5, 2) is 1
  num-modulo(-5, -2) is -1
end
}

  }
  @function["num-truncate" #:contract (a-arrow N N)]{

Returns the integer part of its argument.

@examples{
check:
  num-truncate(3.14) is 3
  num-truncate(-3.14) is -3
  num-truncate(~3.14) is ~3
  num-truncate(~-3.14) is ~-3
end
}

  }
  @function["num-sqrt" #:contract (a-arrow N N)]{

Returns the square root.  If the argument is exact and perfect
square, the result is exact.

@examples{
check:
  num-sqrt(4) is 2
  num-sqrt(5) is%(within(0.001)) ~2.236
  num-sqrt(~4) is ~2
  num-sqrt(~5) is%(within(0.001)) ~2.236
  num-sqrt(0.04) is 1/5
  num-sqrt(-1) raises "negative argument"
end
}
  }
  @function["num-sqr" #:contract (a-arrow N N)]{

Returns the square.

@examples{
check:
  num-sqr(4) is 16
  num-sqr(5) is 25
  num-sqr(~4) is ~16
  num-sqr(~5) is ~25
  num-sqr(0.04) is 1/625
end
}

  }
  @function["num-ceiling" #:contract (a-arrow N N)]{

Returns the smallest integer greater than or equal to the
argument.
The result is exact only if
the argument is.

@examples{
check:
  num-ceiling(4.2) is 5
  num-ceiling(-4.2) is -4
end
}

  }
  @function["num-floor" #:contract (a-arrow N N)]{

Returns the largest integer less than or equal to the argument.
The result is exact only if
the argument is.

@examples{
check:
  num-floor(4.2) is 4
  num-floor(-4.2) is -5
end
}
  }
  @function["num-round" #:contract (a-arrow N N)]{

Returns the closest integer to the argument. The result is exact only if
the argument is.

@examples{
check:
  num-round(4.2) is 4
  num-round(4.8) is 5
  num-round(-4.2) is -4
  num-round(-4.8) is -5
end
}

  }
  @function["num-log" #:contract (a-arrow N N)]{

Returns the natural logarithm of the argument, as a roughnum.
However, if the argument is exact 1, the
result is exact 0. If the argument is non-positive, an exception is
raised.

@examples{
check:
  num-log(1) is 0
  num-log(0) raises "non-positive argument"
  num-log(-1) raises "non-positive argument"
  num-log(2.718281828) is%(within(0.01)) 1
end
}

  }
  @function["num-exp" #:contract (a-arrow N N)]{

Returns e raised to the argument, as a roughnum.  However, if the
argument is exact 0, the result is
exact 1.

@examples{
check:
  num-exp(-1) is%(within(0.0001)) (1 / num-exp(1))
  num-exp(0) is 1
  num-exp(1) is%(within(0.0001)) 2.718281828
  num-exp(3) is%(within(0.0001)) num-expt(2.718281828, 3)
  num-exp(710) raises "overflow"
end
}

  }
  @function["num-expt" #:contract (a-arrow N N N)]{

Returns the first argument raised to the second argument. The result is exact
if both arguments are exact, except for an exception when the
first argumetn is zero and the second is negative.
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
  @function["num-exact" #:contract (a-arrow N N)]{

Returns the exact number equal to the number suggested by the
roughnum.

@examples{
check:
  num-sqrt(2) is%(within(0.000001)) ~1.4142135623730951
  num-exact(num-sqrt(2)) is 1767766952966369/1250000000000000
end
}
  }
  @function["num-is-integer" #:contract (a-arrow N B)]{
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
  @function["num-is-rational" #:contract (a-arrow N B)]{

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
  @function["num-is-roughnum" #:contract (a-arrow N B)]{
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
  @function["num-is-positive" #:contract (a-arrow N B)]{

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
  @function["num-is-negative" #:contract (a-arrow N B)]{

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
  @function["num-is-non-positive" #:contract (a-arrow N B)]{

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
  @function["num-is-non-negative" #:contract (a-arrow N B)]{

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
  @function["num-is-fixnum" #:contract (a-arrow N B)]{

Returns true if the argument is represented directly as a JS
double.

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
although they could be represented as JS doubles.

  }
  @function["num-tostring" #:contract (a-arrow N S)]{
Returns a string representing a literal form of the number.

@examples{
check:
  num-tostring(2.5) is "5/2"
  num-tostring(2) is "2"
  num-tostring(2/3) is "2/3"
  num-tostring(~2.718) is "~2.718"
  num-tostring(~6.022e23) is "~6.022e+23"
end
}
  }
  @function["num-within" #:contract (a-arrow N A)]{

Returns a predicate that checks if the difference of its two
arguments is less than @pyret{tol}.

@examples{
check:
   1  is%(num-within(0.1))       1
   1  is%(num-within(0.1))      ~1
  ~3  is%(num-within(0.1))      ~3
  ~2  is-not%(num-within(0.1))  ~3
  ~2  is%(num-within(1.1))      ~3
  ~2  is-not%(num-within(~1))   ~3
   2  is-not%(num-within(1))    ~3
   5  is%(num-within(4))         3

   num-within(-0.1)(1, 1.05) raises "negative tolerance"
end
}

  }
  @function["num-within-rel" #:contract (a-arrow N A)]{

Returns a predicate that checks if the relative difference of its two
number arguments is less than @pyret{tol}.

@examples{
check:
  100000 is%(num-within-rel(0.1)) 95000
  100000 is-not%(num-within-rel(0.1)) 85000
end
}
  }
  @function["within" #:contract (a-arrow N A)]{

Returns a predicate that checks if its arguments are guaranteed
to be structurally
equivalent and any numbers in corresponding positions are such
that their difference is less than @pyret{tol}.
Notably,
this predicate will fail if either argument contains mutable
objects.

@examples{
check:
  ~2  is-not%(within(0.1))  ~3
  ~2  is%(within(1.1))      ~3

   within(-0.1)(1, 1.05) raises "negative tolerance"

   l3 = [list: ~1]
   l4 = [list: 1.2]
   l3 is%(within(0.5))  l4
   l3 is-not%(within(0.1)) l4
   l3 is%(within(~0.5))  l4
   l3 is-not%(within(~0.1)) l4
end
}

  }
  @function["within-now" #:contract (a-arrow N A)]{

Returns a predicate that checks if its arguments are currently
structurally
equivalent and any numbers in corresponding positions are such
that their current difference is less than @pyret{tol}.
Notably,
if the arguments contain mutable objects, the predicate could
return false if those objects are mutated.

@examples{
check:
  b1 = box(5)
  b2 = box(5)
  l1 = [list: 2, b1]
  l2 = [list: 2.1, b2]

  l1 is-not%(within(0.3)) l2
  l1 is%(within-now(0.3)) l2

  b1!{v: 10}

  l1 is-not%(within-now(0.3)) l2
end
}

  }
  @function["within-rel" #:contract (a-arrow N A)]{

Returns a predicate that checks if its arguments are guaranteed
to be structurally
equivalent and any numbers in corresponding positions are such
that their relative difference is currently less than @pyret{tol}.
Notably,
this predicate will return false if either argument contains mutable
objects.

@examples{
check:
  l7 = [list: 1]
  l8 = [list: ~1.2]
  l7 is%(within-rel(0.5))  l8
  l7 is-not%(within-rel(0.1)) l8
  l7 is%(within-rel(~0.5))  l8
  l7 is-not%(within-rel(~0.1)) l8
end
}

  }
  @function["within-rel-now" #:contract (a-arrow N A)]{

Returns a predicate that checks if its arguments are currently
structurally
equivalent and any numbers in corresponding positions are such
that their relative difference is currently less than @pyret{tol}.
Notably,
if the arguments contain mutable objects, the predicate could
return false if those objects are mutated.

@examples{
check:
  b1 = box(5)
  b2 = box(5)
  l1 = [list: 2, b1]
  l2 = [list: 2.1, b2]

  l1 is%(within-rel-now(0.5)) l2
  l1 is-not%(within-rel(0.5)) l2

  b1!{v: 10}

  l1 is-not%(within-rel-now(0.5)) l2
end
}

  }
}
