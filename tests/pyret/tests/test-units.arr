check "Unit equality":
  2 == 2%<m> is false
  2%<s> == 2%<m> is false

  2%<m> is 2%<m>
  2 is 2%<m / m>
  2%<s * m> is 2%<m * s>
  2%<(((s * m) ^ 2) / t) ^ 2> is 2%<m * s * m * s * (((m * s) / t) ^ 2)>

  1/1%<m> is 1%<m>
  ~1/2%<m> is-roughly ~1/2%<m>

  # tests for adding/removing units
  (10 * 1%<m>) + 1%<m> is 11%<m>
  (10%<m> / 1%<m>) + 1 is 11
end

check "Arithmetic binops":
  2%<m> + 2%<m> is 4%<m>
  (2%<m> + 2%<s>) raises "Cannot perform + operation due to unit mis-match: m and s"

  2%<m> - 2%<m> is 0%<m>
  (2%<m> - 2%<s>) raises "Cannot perform - operation due to unit mis-match: m and s"

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
  2%<m> > 2%<s> raises "Cannot perform > operation due to unit mis-match: m and s"

  2%<m> >= 2%<m> is true
  2%<m> >= 3%<m> is false
  2%<m> >= 2%<s> raises "Cannot perform >= operation due to unit mis-match: m and s"

  1%<m> < 2%<m> is true
  2%<m> < 2%<m> is false
  2%<m> < 2%<s> raises "Cannot perform < operation due to unit mis-match: m and s"

  2%<m> <= 2%<m> is true
  2%<m> <= 1%<m> is false
  2%<m> <= 2%<s> raises "Cannot perform <= operation due to unit mis-match: m and s"
end

check "Other supported operations":
  #|
  TODO:
  numerator()
  denominator()
  What even is the best option for those?
  |#
  num-abs(-2%<m>) is 2%<m>
  num-floor(3/2%<m>) is 1%<m>
  num-expt(2%<m>, 3) is 8%<m ^ 3>
  num-round(3/2%<m>) is 2%<m>

  num-expt(2%<m>, 3%<m>) raises "expt: power cannot have a unit"
end

check "Unsupported unops":
  #|
  TODO:
  integerSqrt() - is there any way to trigger this or is this unused?
  |#
  num-sqrt(2%<m>) raises "The square root operation does not support units but was given an argument with the unit m"
  num-log(2%<m>) raises "The log operation does not support units but was given an argument with the unit m"
  num-atan(2%<m>) raises "The atan operation does not support units but was given an argument with the unit m"
  num-asin(0%<m>) raises "The asin operation does not support units but was given an argument with the unit m"
  num-acos(0%<m>) raises "The acos operation does not support units but was given an argument with the unit m"
  num-sin(2%<m>) raises "The sin operation does not support units but was given an argument with the unit m"
  num-tan(2%<m>) raises "The tan operation does not support units but was given an argument with the unit m"
  num-cos(2%<m>) raises "The cos operation does not support units but was given an argument with the unit m"
  num-exp(2%<m>) raises "The exp operation does not support units but was given an argument with the unit m"
end
