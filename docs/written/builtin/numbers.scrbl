#lang scribble/base
@(require
  scriblib/footnote
  "../../scribble-api.rkt"
  "../abbrevs.rkt")

@(append-gen-docs
  '(module "numbers"
    (path "src/js/base/runtime-anf.js")
    (data-spec
      (name "Number")
      (variants)
      (shared))
  (fun-spec
    (name "nums-equal")
    (arity 2)
    (args ("n1" "n2"))
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
    (name "num-is-fixnum")
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
))


@docmodule["numbers" #:noimport #t #:friendly-title "Numbers"]{
  @type-spec["Number" (list)]

  The type of number values.  Includes both exact rational numbers (which
  includes exact integers) and floating-point numbers

  So, for example, this succeeds:

@examples{
check:
  1 / 3 is 10000000000000000000000000 / 30000000000000000000000000
end
# Succeeds
}

  But is false using pure floating point arithmetic:

@examples{
check:
  1.0 / 3.0 is 10000000000000000000000000.0 / 30000000000000000000000000.0
end
# Values not equal:
# 0.3333333333333333
# 0.33333333333333337
}

  @note{The syntax for floating-point literals may change in the future.}

  Pyret has inexact floating-point arithmetic because many operations simply
  cannot be represented exactly, like trigonometric functions and constants
  like e and π.
  

  @section{Number Functions}


  @function["num-max" #:contract (a-arrow N N N) #:return N]

  Returns the larger of two numbers.

@examples{
check:
  num-max(1, 2) is 2
  num-max(-1.1, 0) is 0
end
}

  @function["num-min" #:contract (a-arrow N N N) #:return N]

  Returns the smaller of two numbers.

@examples{
check:
  num-max(1, 2) is 2
  num-max(-1.1, 0) is 0
end
}


  @function["num-abs" #:contract (a-arrow N N) #:return N]

Returns the absolute value of the given number.

@examples{
check:
  num-abs(-1) is 1
  num-abs(1) is 1
end
}

  @function["num-sin" #:contract (a-arrow N N) #:return N]

Returns an approximation of the mathematical sin of the given number.

@examples{
fun within(delta):
  lam(v1, v2): num-abs(v1 - v2) <= delta end
end
check:
  num-sin(0) is 0
  num-sin(1) is%(within(0.1)) 0.84
end
}

  @function["num-cos" #:contract (a-arrow N N)]

@examples{
fun within(delta):
  lam(v1, v2): num-abs(v1 - v2) <= delta end
end
check:
  num-cos(0) is 1
  num-cos(1) is%(within(0.1)) 0.54
end
}

  @function["num-tan" #:contract (a-arrow N N)]
  @function["num-asin" #:contract (a-arrow N N)]
  @function["num-acos" #:contract (a-arrow N N)]
  @function["num-atan" #:contract (a-arrow N N)]

  @function["num-modulo" #:contract (a-arrow N N N)]

Performs modulo arithmetic on the given numbers.  This is useful for
calculating if one number is a multiple of another, by checking for a zero
remainder.

@examples{
fun is-odd(n :: Number) -> Boolean:
  num-modulo(n, 2) == 0
where:
  is-odd(6) is true
  is-odd(3) is false
end

check "More general modulo":
  num-modulo(7, 3) is 1
  num-modulo(0, 5) is 0
  num-modulo(-7, 3) is 2
end
}

  @function["num-truncate" #:contract (a-arrow N N)]

Cuts off any decimal part of the given number.  Does not do any rounding.

@examples{
check:
  num-truncate(1.1) is 1
  num-truncate(1.8) is 1
  num-truncate(-1.1) is -1
end
}

  @function["num-sqrt" #:contract (a-arrow N N)]

Take the square root of the given number.

@examples{
check:
  num-sqrt(4) is 2
  num-sqrt(1) is 1
end
}

  @function["num-sqr" #:contract (a-arrow N N)]

Squares the given number.

@examples{
check:
  num-sqr(12) is 144
  num-sqr(-4) is 16
end
}

  @function["num-ceiling" #:contract (a-arrow N N)]

Returns the largest integer within 1 of the given number.

@examples{
check:
  num-ceiling(12.3) is 13
  num-ceiling(-4.4) is -4
end
}

  @function["num-floor" #:contract (a-arrow N N)]

Returns the smallest integer within 1 of the given number.

@examples{
check:
  num-ceiling(12.3) is 13
  num-ceiling(-4.4) is -4
end
}

  @function["num-log" #:contract (a-arrow N N)]

Calculate the natural logarithm of the given number (also known as ln).  Gives
an error on inputs that are 0 or less.

@examples{
fun within(delta):
  lam(v1, v2): num-abs(v1 - v2) <= delta end
end
check:
  num-log(10) is%(within(0.1)) 2.3
  num-log(-1) raises "expected a number greater than 0"
end
}

  @function["num-exp" #:contract (a-arrow N N)]

Calculate an approximation of e@superscript{@pyret{n}}.

  @function["num-expt" #:contract (a-arrow N N N)]

Calculate an approximation of @pyret{base}@superscript{@pyret{n}}.

  @function["num-is-integer" #:contract (a-arrow N B) #:return B]

Check if the number is an exact integer.

  @function["num-to-string" #:contract (a-arrow N S) #:return S]

Converts the number to a string.

  @function["num-to-string-digits" #:contract (a-arrow N N S) #:return S]

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

  @function["nums-equal" #:contract (a-arrow N N B) #:return B]

Check if two numbers are equal.  Throws an exception on non-numeric
arguments, which can be a useful alternative to @pyret-id["equal-always"
"equality"] in situations where the program shouldn't compare non-numbers.



@section{Random Numbers}

  @function["num-random" #:contract (a-arrow N N) #:return N]

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

  @function["num-random-seed" #:contract (a-arrow N No) #:return No]

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

@section{Other Number Functions}

  A few other number functions are useful in limited cases that don't come up
  in most programs.

  @function["num-is-fixnum" #:contract (a-arrow N B) #:return B]

Check if the number is representable as a primitive JavaScript number.

  @function["num-exact" #:contract (a-arrow N N) #:return N]

Turn an inexact number into the closest exact number.


}
