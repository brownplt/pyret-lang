import error as E
import contracts as C

fun is-unit-contract-fail(result):
  C.is-fail(result) and C.is-failure-at-arg(result.reason) and C.is-unit-fail(result.reason.reason)
end


check "Unit equality":
  2 == 2%<m> is false
  2%<s> == 2%<m> is false

  2%<m> is 2%<m>
  2%<1 / m> is 2%<m ^ -1>
  2%<1> is 2
  2%<1 * m> is 2%<m>
  2%<m / s / s> is 2%<m * (s ^ -2)>
  2 is 2%<m / m>
  2%<s * m> is 2%<m * s>
  2%<(((s * m) ^ 2) / t) ^ 2> is 2%<m * s * m * s * (((m * s) / t) ^ 2)>

  1/1%<m> is 1%<m>
  ~1/2%<m> is-roughly ~1/2%<m>

  # tests for adding/removing units
  (10 * 1%<m>) + 1%<m> is 11%<m>
  (10%<m> / 1%<m>) + 1 is 11

  0%<s> == 0%<m> is false

  2%<m> =~ 2%<m> is true
  2%<m> =~ 2%<s> is false
  2%<m> <=> 2%<m> is true
  2%<m> <=> 2%<s> is false
end

check "Arithmetic binops":
  2%<m> + 2%<m> is 4%<m>
  (2%<m> + 2%<s>) raises-satisfies E.is-incompatible-units
  (2%<m> + 0%<s>) raises-satisfies E.is-incompatible-units

  2%<m> - 2%<m> is 0%<m>
  (2%<m> - 2%<s>) raises-satisfies E.is-incompatible-units
  (2%<m> - 0%<s>) raises-satisfies E.is-incompatible-units

  2%<m> * 2%<m> is 4%<m ^ 2>
  2%<m> * 2%<m ^ -1> is 4
  2%<m> * 2%<s> is 4%<m * s>
  2 * 2%<m> is 4%<m>

  2%<m> / 2%<m> is 1
  2%<m> / 2%<s> is 1%<m / s>
  2%<m> / 2%<s ^ 2> is 1%<m / (s ^ 2)>
  2%<m> / 0%<s ^ 2> raises "/: division by zero, 2%<m> 0%<s ^ 2>"
end

check "Logical binops":
  2%<m> > 1%<m> is true
  2%<m> > 2%<m> is false
  2%<m> > 2%<s>  raises-satisfies E.is-incompatible-units

  2%<m> >= 2%<m> is true
  2%<m> >= 3%<m> is false
  2%<m> >= 2%<s>  raises-satisfies E.is-incompatible-units

  1%<m> < 2%<m> is true
  2%<m> < 2%<m> is false
  2%<m> < 2%<s> raises-satisfies E.is-incompatible-units

  2%<m> <= 2%<m> is true
  2%<m> <= 1%<m> is false
  2%<m> <= 2%<s> raises-satisfies E.is-incompatible-units
end

check "Other supported operations":
  num-equal(2%<m>, 2%<m>) is true
  num-equal(4/2%<m>, 2%<m>) is true
  num-equal(4/2, 2%<m>) is false

  num-to-roughnum(2%<m>) is-roughly ~2.0%<m>

  num-is-integer(2%<m>) is true
  num-is-integer(2.5%<m>) is false

  num-is-rational(2%<m>) is true
  num-is-rational(2.5%<m>) is true
  num-is-rational(1/2%<m>) is true
  num-is-rational(~2%<m>) is false

  num-is-roughnum(2%<m>) is false
  num-is-roughnum(1/2%<m>) is false
  num-is-roughnum(1.609%<m>) is false
  num-is-roughnum(~2%<m>) is true

  num-is-positive(2%<m>) is true
  num-is-positive(0%<m>) is false
  num-is-positive(-2%<m>) is false
  num-is-non-positive(2%<m>) is false
  num-is-non-positive(0%<m>) is true
  num-is-non-positive(-2%<m>) is true

  num-is-negative(2%<m>) is false
  num-is-negative(0%<m>) is false
  num-is-negative(-2%<m>) is true
  num-is-non-negative(2%<m>) is true
  num-is-non-negative(0%<m>) is true
  num-is-non-negative(-2%<m>) is false

  num-to-string(2.5%<s * (m ^ 2)>) is "5/2%<m ^ 2 * s>"
  num-to-string(2%<s * (m ^ 2)>) is "2%<m ^ 2 * s>"
  num-to-string(2/3%<s * (m ^ 2)>) is "2/3%<m ^ 2 * s>"
  num-to-string(~2.718%<s * (m ^ 2)>) is "~2.718%<m ^ 2 * s>"
  num-to-string(~6.022e23%<s * (m ^ 2)>) is "~6.022e+23%<m ^ 2 * s>"

  num-to-string-digits(2/3%<s * (m ^ 2)>, 3) is "0.667%<m ^ 2 * s>"
  num-to-string-digits(-2/3%<s * (m ^ 2)>, 3) is "-0.667%<m ^ 2 * s>"

  num-to-string-digits(5%<s * (m ^ 2)>, 2) is "5.00%<m ^ 2 * s>"
  num-to-string-digits(5%<s * (m ^ 2)>, 0) is "5%<m ^ 2 * s>"
  num-to-string-digits(555%<s * (m ^ 2)>, -2) is "600%<m ^ 2 * s>"

  num-max(2%<m>, 1%<m>) is 2%<m>
  num-max(2%<s>, 2%<m>) raises-satisfies E.is-incompatible-units
  num-min(2%<m>, 1%<m>) is 1%<m>
  num-min(2%<s>, 2%<m>) raises-satisfies E.is-incompatible-units

  num-abs(-2%<m>) is 2%<m>
  num-floor(3/2%<m>) is 1%<m>
  num-ceiling(3/2%<m>) is 2%<m>
  num-round(3/2%<m>) is 2%<m>
  num-round-even(3.5%<m>) is 4%<m>
  num-truncate(3.5%<m>) is 3%<m>

  num-sqr(3%<m>) is 9%<m ^ 2>
  num-expt(2%<m>, 3) is 8%<m ^ 3>
  num-expt(2%<m / s>, -3) is 1/8%<(s / m) ^ 3>
  num-sqrt(4%<m ^ 2>) is 2%<m>
  num-sqrt(4%<m ^ -2>) is 2%<1 / m>
end

check "Unsupported operations":
  num-sqrt(2%<m>) raises-satisfies E.is-invalid-unit-state
  num-expt(2%<m>, 0.5) raises-satisfies E.is-invalid-unit-state

  num-log(2%<m>) raises-satisfies is-unit-contract-fail
  num-atan(2%<m>) raises-satisfies is-unit-contract-fail
  num-atan2(2%<m>, 2) raises-satisfies is-unit-contract-fail
  num-atan2(2, 2%<m>) raises-satisfies is-unit-contract-fail
  num-asin(0%<m>) raises-satisfies is-unit-contract-fail
  num-acos(0%<m>) raises-satisfies is-unit-contract-fail
  num-sin(2%<m>) raises-satisfies is-unit-contract-fail
  num-tan(2%<m>) raises-satisfies is-unit-contract-fail
  num-cos(2%<m>) raises-satisfies is-unit-contract-fail
  num-exp(2%<m>) raises-satisfies is-unit-contract-fail
  num-expt(2%<m>, 3%<m>) raises-satisfies is-unit-contract-fail
  num-modulo(2%<m>, 2%<m>) raises-satisfies is-unit-contract-fail
  num-to-string-digits(2/3%<s * (m ^ 2)>, 3%<m>) raises-satisfies is-unit-contract-fail
end

check "Units with bigint powers":
  1%<u ^ 10000000000000001> == 1%<u ^ 10000000000000000> is false
  1%<u ^ 10000000000000001> == 1%<u ^ 10000000000000001> is true

  num-sqr(1%<u ^ 9000000000000000>) == 1%<u ^ 18000000000000000> is true
  num-sqrt(1%<u ^ 18000000000000000>) == 1%<u ^ 9000000000000000> is true
  num-sqr(1%<u ^ 9000000000000000>) == 1%<u ^ 180000000000000000> is false
  num-sqrt(1%<u ^ 18000000000000000>) == 1%<u ^ 90000000000000000> is false

  num-sqrt(num-sqrt(num-sqrt(num-sqr(num-sqr(num-sqr(1%<u ^ 10000000000000001>)))))) is 1%<u ^ 10000000000000001>
end

check "Within builtins":
  within(2%<m>) raises-satisfies is-unit-contract-fail

  within(0.5)(3%<m>, 5/2%<m>) is true
  within(0.5)(3%<m>, ~2.999999%<m>) is true
  within(0.5)(3%<m>, 2%<m>) is true
  within(0.5)(3%<m>, 1%<m>) is false
  within(0.5)([list: 1%<m>, 1%<m>], [list: 1%<m>, 4%<m>]) is false
  within(0.5)([list: 1%<m>, 2%<m>], [list: 1%<m>, 2.5%<m>]) is true

  within(0.5)(3%<m>, 2) is false
  within(0.5)(3%<m>, 2%<p>) is false

  within-abs(1%<m>)(2%<m>, 5/2%<m>) is true
  within-abs(1%<m>)(2%<m>, ~2.99999%<m>) is true
  within-abs(1%<m>)(2%<m>, 3%<m>) is true
  within-abs(1%<m>)(2%<m>, 4%<m>) is false
  within-abs(1%<m>)([list: 1%<m>, 2%<m>], [list: 1%<m>, 4%<m>]) is false
  within-abs(1%<m>)([list: 1%<m>, 3.5%<m>], [list: 1%<m>, 4%<m>]) is true

  within-abs(1)(2, 3%<m>) is false
  within-abs(1%<m>)(2, 3%<m>) is false
  within-abs(1)(2%<m>, 3%<p>) is false
  within-abs(1%<p>)(2%<m>, 3%<m>) is false

  num-within-rel(2%<m>) raises-satisfies is-unit-contract-fail

  num-within-rel(0.5)(3%<m>, 5/2%<m>) is true
  num-within-rel(0.5)(3%<m>, ~2.999999%<m>) is true
  num-within-rel(0.5)(3%<m>, 2%<m>) is true
  num-within-rel(0.5)(3%<m>, 1%<m>) is false

  num-within-rel(0.5)(3%<m>, 2) raises-satisfies E.is-incompatible-units
  num-within-rel(0.5)(3%<m>, 2%<p>) raises-satisfies E.is-incompatible-units

  num-within-abs(1%<m>)(2%<m>, 5/2%<m>) is true
  num-within-abs(1%<m>)(2%<m>, ~2.99999%<m>) is true
  num-within-abs(1%<m>)(2%<m>, 3%<m>) is true
  num-within-abs(1%<m>)(2%<m>, 4%<m>) is false

  num-within-abs(1)(2, 3%<m>) raises-satisfies E.is-incompatible-units
  num-within-abs(1%<m>)(2, 3%<m>) raises-satisfies E.is-incompatible-units
  num-within-abs(1)(2%<m>, 3%<p>) raises-satisfies E.is-incompatible-units
  num-within-abs(1%<p>)(2%<m>, 3%<m>) raises-satisfies E.is-incompatible-units
end
