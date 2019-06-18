import error as E

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

  # test poly 0
end

check "Arithmetic binops":
  2%<m> + 2%<m> is 4%<m>
  (2%<m> + 2%<s>) raises-satisfies E.is-incompatible-units

  2%<m> - 2%<m> is 0%<m>
  (2%<m> - 2%<s>) raises-satisfies E.is-incompatible-units

  2%<m> * 2%<m> is 4%<m ^ 2>
  2%<m> * 2%<m ^ -1> is 4
  2%<m> * 2%<s> is 4%<m * s>
  2 * 2%<m> is 4%<m>

  2%<m> / 2%<m> is 1
  2%<m> / 2%<s> is 1%<m / s>
  2%<m> / 2%<s ^ 2> is 1%<m / (s ^ 2)>
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

  num-to-string(2.5%<s * (m ^ 2)>) is "5/2<m ^ 2 * s>"
  num-to-string(2%<s * (m ^ 2)>) is "2<m ^ 2 * s>"
  num-to-string(2/3%<s * (m ^ 2)>) is "2/3<m ^ 2 * s>"
  num-to-string(~2.718%<s * (m ^ 2)>) is "~2.718<m ^ 2 * s>"
  num-to-string(~6.022e23%<s * (m ^ 2)>) is "~6.022e+23<m ^ 2 * s>"

  num-to-string-digits(2/3%<s * (m ^ 2)>, 3) is "0.667<m ^ 2 * s>"
  num-to-string-digits(-2/3%<s * (m ^ 2)>, 3) is "-0.667<m ^ 2 * s>"

  # TODO: Failing... hmmm....
  num-to-string-digits(5%<s * (m ^ 2)>, 2) is "5.00<m ^ 2 * s>"

  num-to-string-digits(5%<s * (m ^ 2)>, 0) is "5<m ^ 2 * s>"
  num-to-string-digits(555%<s * (m ^ 2)>, -2) is "600<m ^ 2 * s>"

  num-max(2%<m>, 1%<m>) is 2%<m>
  num-max(2%<s>, 2%<m>) raises ""
  num-min(2%<m>, 1%<m>) is 1%<m>
  num-min(2%<s>, 2%<m>) raises ""

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

check "Unsupported unops":
  num-sqrt(2%<m>) raises ""
  num-log(2%<m>) raises ""
  num-atan(2%<m>) raises ""
  num-atan2(2%<m>) raises ""
  num-asin(0%<m>) raises ""
  num-acos(0%<m>) raises ""
  num-sin(2%<m>) raises ""
  num-tan(2%<m>) raises ""
  num-cos(2%<m>) raises ""
  num-exp(2%<m>) raises ""
  num-expt(2%<m>, 3%<m>) raises ""
  num-expt(2%<m>, 0.5) raises ""
  num-modulo(2%<m>, 2%<m>) raises ""
  num-to-string-digits(2/3%<s * (m ^ 2)>, 3%<m>) raises ""
  num-within-abs(2%<m>) raises ""
  num-within-rel(2%<m>) raises ""
end
