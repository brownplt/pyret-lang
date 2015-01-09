check "roughnum":
  localwithin = lam(delta):
            lam(a, b):
              num-abs(a - b) < delta
            end
          end
  1 + 2 is 3
  1 + ~2 is%(localwithin(~0.005)) ~3
  num-sin(0) is 0
  num-sin(1) is%(localwithin(~0.01)) ~0.84
  1 < 2 is true
  ~1 < ~2 is true
  2 == (1 + 1) is true
  ~2 == (~1 + ~1) raises "roughnums cannot be compared for equality"
  ~2 is%(localwithin(~0.01)) (~1 + ~1)
  1.5 is 3/2
  num-sqrt(-1) raises "sqrt of negative number"
  num-is-integer(2) is true
  num-is-integer(2.0) is true
  num-is-integer(2.1) is false
  num-is-integer(6/3) is true
  num-is-integer(5/3) is false
  num-is-integer(~2) is false
  num-is-integer(~2.4) is false
  num-is-rational(2) is true
  num-is-rational(2.0) is true
  num-is-rational(2.1) is true
  num-is-rational(6/3) is true
  num-is-rational(5/3) is true
  num-is-rational(~2) is false
  num-is-rational(~2.4) is false
  num-is-roughnum(2) is false
  num-is-roughnum(2.0) is false
  num-is-roughnum(2.1) is false
  num-is-roughnum(6/3) is false
  num-is-roughnum(5/3) is false
  num-is-roughnum(~2) is true
  num-is-roughnum(~2.4) is true
  num-is-positive(2) is true
  num-is-positive(~2) is true
  num-is-positive(0) is false
  num-is-positive(~-2) is false
  num-is-positive(-2) is false
  num-is-negative(2) is false
  num-is-negative(~2) is false
  num-is-negative(0) is false
  num-is-negative(~-2) is true
  num-is-negative(-2) is true
  num-is-non-negative(2) is true
  num-is-non-negative(~2) is true
  num-is-non-negative(0) is true
  num-is-non-negative(~-2) is false
  num-is-non-negative(-2) is false
  num-is-non-positive(2) is false
  num-is-non-positive(~2) is false
  num-is-non-positive(0) is true
  num-is-non-positive(~-2) is true
  num-is-non-positive(-2) is true
  num-modulo(10,3) is 1
  num-modulo(10,0) raises "second argument is zero"
  num-expt(2, 3)     is                     8
  num-expt(2, 3)     is%(num-within(0.001)) 8
  num-expt(~2, 3)    is%(num-within(0.001)) 8
  num-expt(2, ~3)    is%(num-within(0.001)) 8
  num-expt(~2, ~3)   is%(num-within(0.001)) 8
  num-expt(2,-3)     is                     0.125
  num-expt(2,-3)     is%(num-within(0.001)) 0.125
  num-expt(2,~-3)    is%(num-within(0.001)) 0.125
  num-expt(~2,-3)    is%(num-within(0.001)) 0.125
  num-expt(~2,~-3)   is%(num-within(0.001)) 0.125
  num-expt(234,0)    is%(num-within(0.001)) 1
  num-expt(234,~0)   is%(num-within(0.001)) 1
  num-expt(~234,0)   is%(num-within(0.001)) 1
  num-expt(~234,~0)  is%(num-within(0.001)) 1
  num-expt(234,-0)   is%(num-within(0.001)) 1
  num-expt(234,~-0)  is%(num-within(0.001)) 1
  num-expt(~234,-0)  is%(num-within(0.001)) 1
  num-expt(~234,~-0) is%(num-within(0.001)) 1
  num-expt(0, -3)   raises "division by zero"
  num-expt(0, ~-3)  raises "division by zero"
  num-expt(~0, -3)  raises "division by zero"
  num-expt(~0, ~-3) raises "division by zero"
end

check "overflow rather than infinity":
  num-exp(710)          raises "overflow"
  num-exp(~710)         raises "overflow"
  num-exp(1000000000000000000000000) raises "overflow" # this tests for arg known to be bigint
  num-expt(2.718,~710)  raises "overflow"
  num-expt(~2.718,710)  raises "overflow"
  num-expt(~2.718,~710) raises "overflow"
  # note: because of arbitrary precision, num-expt(2.718,710) will NOT overflow
  99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 * ~9 raises "overflow"
end

check "long decimals should behave exactly":
  3.000000000000000000000000000000000001 is-not 3
  0.000000000000000000000001 * 1000000000000000000000000 is 1
  1000000000000000000000000000000000000000000000000000000001
    is-not 1000000000000000000000000000000000000000000000000000000000
  2.99999999999999999999999999999999999999999999 is-not
    2.999999999999999999999999999999999999999999999

  # this is just to test that num-sin, &c don't blow on bigints
  num-sin(10000000000000000) * 0 is 0
  num-cos(10000000000000000) * 0 is 0
  num-tan(10000000000000000) * 0 is 0
  num-asin(10000000000000000) * 0 is 0
  num-acos(10000000000000000) * 0 is 0
  num-atan(10000000000000000) * 0 is 0
  num-log(10000000000000000) * 0 is 0
  #num-exp(10000000000000000) * 0 is 0  # should error? JS gives +inf for Math.exp(710)
  num-round(10000000000000000) * 0 is 0
end
