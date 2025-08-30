#lang pyret

import constants as constants

provide *

check:
  fun negate(f): lam(x): not(f(x)) end end
  fun around(n, delta): lam(other): num-abs(other - n) < delta end end

  3 / (4 - 4) raises "division by zero"

  within-abs(-3)(1, 2) raises "negative tolerance"
  within(-3)(2, 3) raises "negative relative tolerance"
  within-rel(-3)(2, 3) raises "negative relative tolerance"

  min-number = ~0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005
  within-abs(min-number)(0, min-number) raises "roughnum tolerance too small"

  num-expt(0, -1) raises "expt: division by zero"

  num-exp(5000) raises "exp: argument too large"

  num-modulo(0.5, 5) raises "NumInteger"
  num-modulo(5, 0.5) raises "NumInteger"
  num-modulo(6, 0) raises "second argument is zero"

  num-sqrt(-3) raises "NumNonNegative"

  num-acos(-2) raises "acos: out of domain"
  num-acos(2) raises "acos: out of domain"

  num-asin(-2) raises "asin: out of domain"
  num-asin(2) raises "asin: out of domain"

  num-to-string-digits(3, 1/2) raises "NumInteger"
  num-to-string-digits(3, ~3) raises "NumInteger"


  num-equal(~3, ~4) raises "cannot be compared for equality"

  num-max(1, 3) is 3
  num-max("not-a-num", 3) raises ""
  num-max(-1, -1) is -1
  num-max(-1, -2) is -1
  num-max(0.1, 1 / 11) is 0.1
  num-max(0, 1) is 1

  num-min(1, 3) is 1
  num-min("not-a-num", 3) raises ""
  num-min(-1, -1) is -1
  num-min(-1, -2) is -2
  num-min(0.1, 1 / 11) is 1 / 11
  num-min(0, 1) is 0

  1 / 10 is 0.1
  3 is 3
  # TODO(joe): Something is up with parsing bignums end these fail
  # 3.000000000000000000000000000000000001 == 3 is false
  1000000000000000000000000000000000000000000000000000000001
    == 1000000000000000000000000000000000000000000000000000000000
      is false
  #2.99999999999999999999999999999999999999999999 ==
  #  2.999999999999999999999999999999999999999999999
  #    is false
  # TODO(joe): WTF is up with the compilation of this next line?
  # 0.000000000000000000000001 * 100000000000000000000000 is 1

  num-abs(-1) is 1
  num-abs(-0.3) is 0.3
  num-abs(0) is 0
  num-abs(1) is 1

  very-bignum = num-expt(10, 23456)

  # These are just sanity end the js-nums library has more rigorous tests
  # for the accuracy of the trig functions.  Here we just make sure the
  # Pyret functions are bound to plausible underlying operations
  num-sin(0) is 0
  num-sin(3.14 / 2) satisfies around(1, 0.1)
  num-sin(3.14) satisfies around(0, 0.1)
  num-sin(very-bignum) raises "roughnum overflow"

  num-cos(0) is 1
  num-cos(3.14 / 2) satisfies around(0, 0.1)
  num-cos(3.14) satisfies around(-1, 0.1)
  num-cos(very-bignum) raises "roughnum overflow"

  num-tan(0) is 0
  num-tan(3.14 / 4) satisfies around(1, 0.01)
  num-tan(very-bignum) raises "roughnum overflow"

  num-asin(0) is 0
  num-asin(1) satisfies around(1.57, 0.01)

  num-acos(0) satisfies around(1.57, 0.01)
  num-acos(1) is 0

  num-atan(0) is 0
  num-atan(1) satisfies around(0.78, 0.01)


  num-atan2( 2,  1) satisfies around(1.107, 0.001)
  num-atan2( 2, -1) satisfies around(2.034, 0.001)
  num-atan2(-2, -1) satisfies around(4.249, 0.001)
  num-atan2(-2,  1) satisfies around(5.176, 0.001)

  degree = (2 * num-asin(1)) / 180

  num-atan2(                       0,  0) raises "atan2: out of domain"

  num-atan2(                       0,  1) satisfies around(           0, 0.001)

  num-atan2(    num-tan(30 * degree),  1) satisfies around( 30 * degree, 0.001)
  num-atan2(    num-tan(45 * degree),  1) satisfies around( 45 * degree, 0.001)
  num-atan2(    num-tan(60 * degree),  1) satisfies around( 60 * degree, 0.001)
  num-atan2(    num-tan(88 * degree),  1) satisfies around( 88 * degree, 0.001)

  num-atan2(    num-tan(88 * degree), -1) satisfies around( 92 * degree, 0.001)
  num-atan2(    num-tan(60 * degree), -1) satisfies around(120 * degree, 0.001)
  num-atan2(    num-tan(45 * degree), -1) satisfies around(135 * degree, 0.001)
  num-atan2(    num-tan(30 * degree), -1) satisfies around(150 * degree, 0.001)

  num-atan2(                       0, -1) satisfies around(180 * degree, 0.001)

  num-atan2(0 - num-tan(30 * degree), -1) satisfies around(210 * degree, 0.001)
  num-atan2(0 - num-tan(45 * degree), -1) satisfies around(225 * degree, 0.001)
  num-atan2(0 - num-tan(60 * degree), -1) satisfies around(240 * degree, 0.001)
  num-atan2(0 - num-tan(88 * degree), -1) satisfies around(268 * degree, 0.001)

  num-atan2(0 - num-tan(88 * degree),  1) satisfies around(272 * degree, 0.001)
  num-atan2(0 - num-tan(60 * degree),  1) satisfies around(300 * degree, 0.001)
  num-atan2(0 - num-tan(45 * degree),  1) satisfies around(315 * degree, 0.001)
  num-atan2(0 - num-tan(30 * degree),  1) satisfies around(330 * degree, 0.001)


  num-modulo(17, 5) is 2
  num-modulo(15, -2) is -1
  num-modulo(-17, -5) is -2
  num-modulo(-17, 5) is 3

  num-remainder(5, 3) is 2
  num-remainder(5, -3) is 2
  num-remainder(-5, 3) is -2
  num-remainder(-5, -3) is -2

  num-remainder(4/3, 3/5) is 2/15
  num-remainder(4/3, -3/5) is 2/15
  num-remainder(-4/3, 3/5) is -2/15
  num-remainder(-4/3, -3/5) is -2/15

  num-truncate(5.5) is 5
  num-truncate(-5.5) is -5

  num-sqrt(9) is 3
  num-sqrt(10) satisfies around(3.1, 0.1)
  num-sqrt(8768762532 * 8768762532) is 8768762532

  num-ceiling(5.5) is 6
  num-ceiling(5.1) is 6
  num-ceiling(-5.5) is -5

  num-floor(5.5) is 5
  num-floor(5.1) is 5
  num-floor(-5.5) is -6

  num-log(0) raises "NumPositive"
  num-log(1) is 0
  num-log(num-exp(1)) satisfies around(1, 0.0001)
  num-log(num-expt(10, 36789)) is-roughly ~84709.80298615794 // value from Racket

  2 is num-exact(2)
  1 / 3 is num-exact(1 / 3)
  # NOTE(joe): This seems a big algorithm-dependent end mainly here
  # as a regression test so we know if this changes
  #num-exact(num-sqrt(2)) is 1767766952966369 / 1250000000000000
  num-exact(num-sqrt(2)) is 14142135623730951/10000000000000000

  2 satisfies num-is-integer
  1 / 3 satisfies negate(num-is-integer)
  num-sqrt(2) satisfies negate(num-is-integer)
  num-exp(3) satisfies negate(num-is-integer)
  num-log(1) satisfies num-is-integer

  num-expt(3, 2) is 9
  num-expt(2, -1) is 1 / 2
  # num-expt(4, 1 / 2) is 2 # sadly this is rough
  num-expt(4, 1 / 2) satisfies around(2, 0.00001)

  num-sqrt(9) is 3
  num-sqrt("nan") raises "Number"

  num-expt(3, 2) is 9
  num-expt("nan", 2) raises "Number"
  num-expt(2, "nan") raises "Number"
  num-expt(7, num-expt(10, 36789)) raises "too large"

  num-ceiling(2.5) is 3
  num-ceiling("nan") raises "Number"

  num-exp(3) satisfies around(20.08, 0.01)
  num-exp(1) satisfies around(2.71, 0.01)
  num-exp(0) is 1

  2 / 3 satisfies negate(num-is-fixnum)
  3 satisfies num-is-fixnum
  3.22222 satisfies negate(num-is-fixnum)
  3.22222222222222222222222222222222222222222 satisfies negate(num-is-fixnum)

  # Test currying of binops
  (_ * 4)(2) is 8
  (3 + _)(12) is 15
  (_ / _)(6, 3) is 2


  ~2e222 * ~2e222 raises "roughnum overflow"
end

check:
  randoms = for map(i from range(0, 15)):
    random(100)
  end

  randoms satisfies lists.all(lam(v): (v < 100) and (v >= 0) end, _)
  randoms violates lists.all(lam(v): v == randoms.first end, _)
end

check "random seed":
  num-random-seed(0)
  s = num-random(10000)

  num-random-seed(1)
  s2 = num-random(10000)

  num-random-seed(0)
  num-random(10000) is s
  num-random-seed(1)
  num-random(10000) is s2
end

check:
  num-to-string-digits(5432.1234, 2) is "5432.12"
  num-to-string-digits(0.123456789, 2) is "0.12"
  num-to-string-digits(5, 2) is "5.00"
  num-to-string-digits(555, -2) is "600"
  # NOTE(joe): This test is awaiting a fixed numeric library for rounding
  # num-to-string-digits(100000000000000000000000000000000000000001234 / 10000, 2) is
  #  "10000000000000000000000000000000000000000.12"
end

check "evangielis #337":
  num-equal(1.11 + 1, 2.11) is true
end

check "sk #345":
  fun square(n): n * n end
  fun f(x, y):
    (333.75 * num-expt(y, 6))
    +
    (square(x)
      *
      ((11 * square(x) * square(y))
       -
       num-expt(y, 6)
       -
       (121 * num-expt(y, 4))
       -
       2))
    +
    (5.5 * num-expt(y, 8))
    +
    (x / (2 * y))
  end
  v = f(77617, 33096)
  v is -54767 / 66192
end

check "floor/ceiling/round of bigrats":
  num-floor(+123123123123123123123123123.3) is 123123123123123123123123123
  num-floor(+123123123123123123123123123.5) is 123123123123123123123123123
  num-floor(+123123123123123123123123123.7) is 123123123123123123123123123
  num-floor(-123123123123123123123123123.3) is -123123123123123123123123124
  num-floor(-123123123123123123123123123.5) is -123123123123123123123123124
  num-floor(-123123123123123123123123123.7) is -123123123123123123123123124
  num-floor(+123123123123123123123123124.5) is 123123123123123123123123124
  num-floor(-123123123123123123123123124.5) is -123123123123123123123123125

  num-ceiling(+123123123123123123123123123.3) is 123123123123123123123123124
  num-ceiling(+123123123123123123123123123.5) is 123123123123123123123123124
  num-ceiling(+123123123123123123123123123.7) is 123123123123123123123123124
  num-ceiling(-123123123123123123123123123.3) is -123123123123123123123123123
  num-ceiling(-123123123123123123123123123.5) is -123123123123123123123123123
  num-ceiling(-123123123123123123123123123.7) is -123123123123123123123123123
  num-ceiling(+123123123123123123123123124.5) is 123123123123123123123123125
  num-ceiling(-123123123123123123123123124.5) is -123123123123123123123123124
end

check "non-erroring roughnum coercion for rationals and bigints whenever feasible":
  p = 81268721875292322346006428361277486504132538854011222666475817166249350496626931570388521644403314190415953881849740041567819628199876803591646157379114065524563985446908892262196425992509944671317287759517001454359426892629201797719306000630092659618487347668341846237234687585768929352898628996784695074874292706491575251948097349309856501057270192988393033514058115

  q = 31074826015292464136352850911368809028150373077645694092712513113641716099394624728813926091948046541979298815056591584707047923844529494021740153955742689843985239712176553523899854808158284912207177902877491879709295462823402583883946947080040968647549604073979681744304215760537682790820012842244687066165442575364044873225935129869506345618953111615635565744

  s = 31074826015292464136352850911368809028150373077645694092712513113641716099394624728813926091948046541979298815056591584707047923844529494021740153955742689843985239712176553523899854808158284912207177902877491879709295462823402583883946947080040968647549604073979681744304215760537682790820012842244687066165

  p-over-q = 81268721875292322346006428361277486504132538854011222666475817166249350496626931570388521644403314190415953881849740041567819628199876803591646157379114065524563985446908892262196425992509944671317287759517001454359426892629201797719306000630092659618487347668341846237234687585768929352898628996784695074874292706491575251948097349309856501057270192988393033514058115/31074826015292464136352850911368809028150373077645694092712513113641716099394624728813926091948046541979298815056591584707047923844529494021740153955742689843985239712176553523899854808158284912207177902877491879709295462823402583883946947080040968647549604073979681744304215760537682790820012842244687066165442575364044873225935129869506345618953111615635565744

  rough-p-over-q = ~81268721875292322346006428361277486504132538854011222666475817166249350496626931570388521644403314190415953881849740041567819628199876803591646157379114065524563985446908892262196425992509944671317287759517001454359426892629201797719306000630092659618487347668341846237234687585768929352898628996784695074874292706491575251948097349309856501057270192988393033514058115/31074826015292464136352850911368809028150373077645694092712513113641716099394624728813926091948046541979298815056591584707047923844529494021740153955742689843985239712176553523899854808158284912207177902877491879709295462823402583883946947080040968647549604073979681744304215760537682790820012842244687066165442575364044873225935129869506345618953111615635565744

  num-is-roughnum(num-to-roughnum(p / q)) is true
  num-to-roughnum(p / q) is%(within(0.001)) 2615259.1115

  num-is-roughnum(num-to-roughnum(p-over-q)) is true
  num-to-roughnum(p-over-q) is%(within(0.001)) 2615259.1115

  num-is-roughnum(rough-p-over-q) is true
  rough-p-over-q is%(within(0.001)) 2615259.1115

  num-is-roughnum(num-to-roughnum(q)) raises "" # q is just too big an int

  num-is-roughnum(num-to-roughnum(s)) is true # but s isn't
  num-to-roughnum(s) is%(within(0.001e307)) 3.10748e307
end

check "rough fractions -- proper, improper, integral -- recognized, provided denr != 0":
  num-is-roughnum(~1/2) is true
  ~1/2 is%(within(0.01)) ~0.5
  num-is-roughnum(~3/2) is true
  ~3/2 is%(within(0.01)) ~1.5
  num-is-roughnum(~10/2) is true
  ~10/2 is%(within(0.1)) ~5
  #
  num-is-roughnum(~+1/2) is true
  ~+1/2 is%(within(0.01)) ~+0.5
  num-is-roughnum(~+3/2) is true
  ~+3/2 is%(within(0.01)) ~+1.5
  num-is-roughnum(~+10/2) is true
  ~+10/2 is%(within(0.1)) ~+5
  #
  num-is-roughnum(~-1/2) is true
  ~-1/2 is%(within(0.01)) ~-0.5
  num-is-roughnum(~-3/2) is true
  ~-3/2 is%(within(0.01)) ~-1.5
  num-is-roughnum(~-10/2) is true
  ~-10/2 is%(within(0.1)) ~-5
end

check:
  constants.E is%(within(~0.0)) num-exp(1)
  constants.e is%(within(~0.0)) num-exp(1)
  constants.PI is%(within(~0.0)) num-acos(0) * 2
  constants.pi is%(within(~0.0)) num-acos(0) * 2
end
