include primitive-types
include number

check "num-equal":
  num-equal(2, 2) is true
  num-equal(2, 3) is false
  num-equal(1/2, 0.5) is true
  num-equal(1 / 2, 0.5) is true
  num-equal(1/3, 0.33) is false
  num-equal(1/3, ~0.33)
    raises "roughnums cannot be compared for equality"
end

check "num-max":
  num-max(1, 2) is 2
  num-max(2, ~3) is-roughly ~3
  num-max(4, ~4) is 4
  num-max(~4, 4) is-roughly ~4
  num-max(-1.1, 0) is 0
end

check "num-equal":
  num-min(1, 2) is 1
  num-min(2, ~3) is 2
  num-min(4, ~4) is 4
  num-min(~4, 4) is-roughly ~4
  num-min(-1.1, 0) is -1.1
end

check "num-abs":
  num-abs(2) is 2
  num-abs(-2.1) is 2.1
  num-abs(~2) is-roughly ~2
  num-abs(~-2.1) is-roughly ~2.1
end

check "num-sin":
  num-sin(0) is 0
  num-sin(1) is%(within-abs(0.01)) 0.84
end

check "num-cos":
  num-cos(0) is 1
  num-cos(1) is%(within-abs(0.01)) 0.54
end

check "num-tan":
  num-tan(0) is 0
  num-tan(1) is%(within-abs(0.01)) 1.56
end

check "num-asin":
  num-asin(0) is 0
  num-asin(0.84) is%(within-abs(0.01)) 1
end

check "num-acos":
  num-acos(1) is 0
  num-acos(0.54) is%(within-abs(0.01)) 1
end

check "num-atan":
  num-atan(0) is 0
  num-atan(1) is-roughly (3.141592 * 1/4) # 45 degrees = π/4 radians
  num-atan(-1) is-roughly (-3.141592 * 1/4) # 315 degrees = -π/4 radians
  num-atan(100000000000) is-roughly (3.141592 / 2) # 90 degrees = π/2 radians
  num-atan(-100000000000) is-roughly (-3.141592 / 2) # 270 degrees = -π/2 radians
end

check "num-atan2":
  num-atan2(0, 1) is 0
  num-atan2(1, 1) is-roughly (3.141592 * 1/4) # 45 degrees
  num-atan2(1, -1) is-roughly (3.141592 * 3/4) # 135 degrees
  num-atan2(-1, -1) is-roughly (3.141592 * 5/4) # 225 degrees
  num-atan2(-1, 1) is-roughly (3.141592 * 7/4) # 315 degrees
  num-atan2(1, 0) is-roughly (3.141592 * 1/2) # 90 degrees
  num-atan2(-1, 0) is-roughly (3.141592 * 3/2) # 270 degrees
end

check "num-modulo":
  num-modulo(5, 2) is 1
  num-modulo(-5, 2) is 1
  num-modulo(-5, -2) is -1
  num-modulo(7, 3) is 1
  num-modulo(0, 5) is 0
  num-modulo(-7, 3) is 2
end

check "num-truncate":
  num-truncate(3.14) is 3
  num-truncate(-3.14) is -3
  num-truncate(~3.14) is-roughly ~3
  num-truncate(~-3.14) is-roughly ~-3
end

check "num-sqrt":
  num-sqrt(4) is 2
  num-sqrt(5) is%(within-abs(0.001)) ~2.236
  num-sqrt(~4) is%(within-abs(0.001)) ~2
  num-sqrt(~5) is%(within-abs(0.001)) ~2.236
  num-sqrt(0.04) is 1/5
  num-sqrt(-1) raises "negative argument"
end

check "num-sqr":
  num-sqr(4) is 16
  num-sqr(5) is 25
  num-sqr(-4) is 16
  num-sqr(~4) is-roughly ~16
  num-sqr(0.04) is 1/625
end

check "num-ceiling":
  num-ceiling(4.2) is 5
  num-ceiling(-4.2) is -4
end

check "num-floor":
  num-floor(4.2) is 4
  num-floor(-4.2) is -5
end

check "num-round":
  num-round(4.2) is 4
  num-round(4.8) is 5
  num-round(-4.2) is -4
  num-round(-4.8) is -5
  num-round(3.5) is 4
  num-round(2.5) is 3
end

check "num-round-even":
  num-round-even(3.5) is 4
  num-round-even(2.5) is 2
end

check "num-log":
  num-log(1) is 0
  num-log(0) raises "non-positive argument"
  num-log(-1) raises "non-positive argument"
  num-log(2.718281828) is%(within-abs(0.01)) 1
  num-log(10) is%(within-abs(0.1)) 2.3
end

check "num-exp":
  num-exp(-1) is%(within-abs(0.0001)) (1 / num-exp(1))
  num-exp(0) is 1
  num-exp(1) is%(within-abs(0.0001)) 2.718281828
  num-exp(3) is%(within-abs(0.0001)) num-expt(2.718281828, 3)
  num-exp(710) raises "exp: argument too large: 710"
end

check "num-expt":
  num-expt(3, 0) is 1
  num-expt(1, 3) is 1
  num-expt(0, 0) is 1
  num-expt(0, 3) is 0
  num-expt(0, -3) raises "division by zero"
  num-expt(2, 3) is 8
  num-expt(2, -3) is 1/8
end

check "num-to-roughnum":
  num-is-roughnum(num-to-roughnum(3.14)) is true
  num-is-roughnum(num-to-roughnum(~3.14)) is true
end

check "num-is-integer":
  num-is-integer(2) is true
  num-is-integer(1/2) is false
  num-is-integer(1.609) is false
  num-is-integer(~2) is false
end

check "num-is-rational":
  num-is-rational(2) is true
  num-is-rational(1/2) is true
  num-is-rational(1.609) is true
  num-is-rational(~2) is false
end

check "num-is-roughnum":
  num-is-roughnum(2) is false
  num-is-roughnum(1/2) is false
  num-is-roughnum(1.609) is false
  num-is-roughnum(~2) is true
end

check "num-is-positive":
  num-is-positive(~-2) is false
  num-is-positive(-2) is false
  num-is-positive(0) is false
  num-is-positive(-0) is false
  num-is-positive(2) is true
  num-is-positive(~2) is true
end

check "num-is-negative":
  num-is-negative(~-2) is true
  num-is-negative(-2) is true
  num-is-negative(0) is false
  num-is-negative(-0) is false
  num-is-negative(2) is false
  num-is-negative(~2) is false
end

check "num-is-non-positive":
  num-is-non-positive(~-2) is true
  num-is-non-positive(-2) is true
  num-is-non-positive(0) is true
  num-is-non-positive(-0) is true
  num-is-non-positive(2) is false
  num-is-non-positive(~2) is false
end

check "num-is-non-negative":
  num-is-non-negative(~-2) is false
  num-is-non-negative(-2) is false
  num-is-non-negative(0) is true
  num-is-non-negative(-0) is true
  num-is-non-negative(2) is true
  num-is-non-negative(~2) is true
end

check "num-to-string":
  num-to-string(2.5) is "5/2"
  num-to-string(2) is "2"
  num-to-string(2/3) is "2/3"
  num-to-string(~2.718) is "~2.718"
  num-to-string(~6.022e23) is "~6.022e+23"
end

check "num-to-string-digits":
  num-to-string-digits(2/3, 3) is "0.667"
  num-to-string-digits(-2/3, 3) is "-0.667"
  num-to-string-digits(5, 2) is "5.00"
  num-to-string-digits(5, 0) is "5"
  num-to-string-digits(555, -2) is "600"
end

check "num-within-abs":
   1  is%(num-within-abs(0.1))       1
   1  is%(num-within-abs(0.1))      ~1
  ~3  is%(num-within-abs(0.1))      ~3
  ~2  is-not%(num-within-abs(0.1))  ~3
  ~2  is%(num-within-abs(1.1))      ~3
  ~2  is%(num-within-abs(~1))       ~3
   2  is%(num-within-abs(1))        ~3
   5  is%(num-within-abs(4))         3

   num-within-abs(-0.1)(1, 1.05) raises "negative tolerance"
end

check "num-within-rel":
  100000 is%(num-within-rel(0.1)) 95000
  100000 is-not%(num-within-rel(0.1)) 85000
end

check "num-is-fixnum":
  num-is-fixnum(10) is true
  num-is-fixnum(~10) is false
  num-is-fixnum(1000000000000000) is true
  num-is-fixnum(10000000000000000) is false
  num-is-fixnum(1.5) is false
end

check "num-to-rational":
  num-sqrt(2) is%(within-abs(0.000001)) ~1.4142135623730951
  num-exact(num-sqrt(2)) is 1.4142135623730951
  num-to-rational(num-sqrt(2)) is 1.4142135623730951
end
