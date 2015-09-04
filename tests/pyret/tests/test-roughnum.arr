import equality as equality

check "roughnum":
  local-within-abs = lam(delta):
            lam(a, b):
              num-abs(a - b) < delta
            end
          end
  1 + 2 is 3
  1 + ~2 is%(local-within-abs(~0.005)) ~3
  num-sin(0) is 0
  num-sin(1) is%(local-within-abs(~0.01)) ~0.84
  1 < 2 is true
  ~1 < ~2 is true
  2 == (1 + 1) is true
  ~2 == (~1 + ~1) raises "Roughnum"
  ~2 < ~2 is false
  ~2 <= ~2 is true
  ~2 == ~2 raises "Roughnum"
  ~2 >= ~2 is true
  ~2 > ~2 is false
  ~2 is%(local-within-abs(~0.01)) (~1 + ~1)
  #
  # but we can also use builtin num-within, within
  1 + ~2 is%(num-within-abs(~0.005)) ~3
  1 + ~2 is%(within-abs(~0.005)) ~3
  num-sin(1) is%(num-within-abs(~0.01)) ~0.84
  num-sin(1) is%(within-abs(~0.01)) ~0.84
  ~2 is%(num-within-abs(~0.01)) (~1 + ~1)
  ~2 is%(within-abs(~0.01)) (~1 + ~1)
  # see test-within.arr for more builtin-within--related tests
  #
  1.5 is 3/2
  #num-sqrt(-1) raises "negative argument"
  num-sqrt(-1) is 0+1i
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
end

check "number casting":
  num-to-rational(~3.14) is 3.14
  num-is-roughnum(num-to-roughnum(3.14)) is true
end

check "num-expt on roughs, zeroes, negs":
  num-expt(2, 3)     is                     8
  num-expt(2, 3)     is%(num-within-abs(0.001)) 8
  num-expt(~2, 3)    is%(num-within-abs(0.001)) 8
  num-expt(2, ~3)    is%(num-within-abs(0.001)) 8
  num-expt(~2, ~3)   is%(num-within-abs(0.001)) 8
  num-expt(2,-3)     is                     0.125
  num-expt(2,-3)     is%(num-within-abs(0.001)) 0.125
  num-expt(2,~-3)    is%(num-within-abs(0.001)) 0.125
  num-expt(~2,-3)    is%(num-within-abs(0.001)) 0.125
  num-expt(~2,~-3)   is%(num-within-abs(0.001)) 0.125
  num-expt(234,0)    is%(num-within-abs(0.001)) 1
  num-expt(234,~0)   is%(num-within-abs(0.001)) 1
  num-expt(~234,0)   is%(num-within-abs(0.001)) 1
  num-expt(~234,~0)  is%(num-within-abs(0.001)) 1
  num-expt(234,-0)   is%(num-within-abs(0.001)) 1
  num-expt(234,~-0)  is%(num-within-abs(0.001)) 1
  num-expt(~234,-0)  is%(num-within-abs(0.001)) 1
  num-expt(~234,~-0) is%(num-within-abs(0.001)) 1
  num-expt(0, -3)    raises "division by zero"
  num-expt(0, ~-3)   raises "division by zero"
  num-expt(~0, -3)   raises "division by zero"
  num-expt(~0, ~-3)  raises "division by zero"
end

check "overflow rather than infinity":
  # in JS, e^709 converges, e^710 doesn't
  num-exp(710)          raises "too large"
  num-exp(~710)         raises "too large"
  num-exp(1000000000000000000000000) raises "too large" # this tests for arg known to be bigint
  num-expt(2.718,~710)  raises "overflow"
  num-expt(~2.718,710)  raises "overflow"
  num-expt(~2.718,~710) raises "overflow"
  # note: because of arbitrary precision, num-expt(2.718,710) will NOT overflow
  99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 * ~9 raises "overflow"
end

check "long decimals (incl bigints) should behave exactly":
  3.000000000000000000000000000000000001 is-not 3
  0.000000000000000000000001 *
   1000000000000000000000000 is 1
  1000000000000000000000000000000000000000000000000000000001 is-not
  1000000000000000000000000000000000000000000000000000000000
  2.99999999999999999999999999999999999999999999 is-not
  2.999999999999999999999999999999999999999999999
end

check "trig-type functions on bigints should converge":
  num-sin(10000000000000000)   * 0 is 0
  num-cos(10000000000000000)   * 0 is 0
  num-tan(10000000000000000)   * 0 is 0
  num-atan(10000000000000000)  * 0 is 0
  num-log(10000000000000000)   * 0 is 0
end

check "comparing MAX_VALUEs":
   1.7976931348623157e+308 -  1.7976931348623157e+308 is                  0
end
check "comparing rough MAX_VALUEs":
  ~1.7976931348623157e+308 - ~1.7976931348623157e+308 is%(within-abs(0.001)) ~0
   1.7976931348623157e+308 - ~1.7976931348623157e+308 is%(within-abs(0.001)) ~0
end

check "identical":
  roughly-4 = ~4
  identical3(roughly-4, roughly-4) satisfies equality.is-Unknown
  identical3(~3, ~3) satisfies equality.is-Unknown
  identical3(~2, ~3) satisfies equality.is-Unknown
  identical3(~2, lam(): 5 end) satisfies equality.is-NotEqual
  identical3(lam(): 5 end, ~2) satisfies equality.is-NotEqual
  identical3(~2, 2) satisfies equality.is-NotEqual
  identical3(2, ~2) satisfies equality.is-NotEqual

  identical(~2, ~2) raises-satisfies error.is-equality-failure
  identical(~2, ~3) raises-satisfies error.is-equality-failure

  ~2 is-not<=> lam(): 5 end
  ~2 is-not<=> method(self): 5 end
  lam(): 5 end is-not<=> ~2
  method(self): 5 end is-not<=> ~2
end

check "fractional rational powers":
  num-expt(125, 1/3) is 5
  num-expt(125, -1/3) is 1/5
  num-expt(-125, 1/3) is -5
  num-expt(-125, -1/3) is -1/5
  num-expt(126, 1/3) is%(within(0.01)) 5
  num-expt(-126, 1/3) is%(within(0.01)) -5
  num-is-roughnum(num-expt(126, 1/3)) is true
  num-expt(256, 1/8) is 2
  num-expt(255, 1/8) is%(within(0.01)) 2
  num-is-roughnum(num-expt(255, 1/8)) is true
  num-expt(512, 1/9) is%(within(0.01)) 2
  num-is-roughnum(num-expt(512, 1/9)) is true
  num-expt(-125, 1/9) raises "negative"
end
