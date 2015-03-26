fun round-half-to-even(n):
  n-neg = (n < 0)
  n-abs = num-exact(num-abs(n))
  var n-round = num-round(n-abs)
  when num-abs(n-round - n-abs) == 1/2:
    when num-modulo(n-round, 2) == 1:
      n-round := n-round - 1
    end
  end
  when n-neg:
    n-round := 0 - n-round
  end
  n-round
end

check "rounding roughs":
  num-round(~3.5) is 4
  num-round(~3.3) is 3
  num-round(~3) is 3
  num-round(~2.7) is 3
  num-round(~2.5) is 3
  num-round(~-2.5) is -3
  num-round(~-2.7) is -3
  num-round(~-3) is -3
  num-round(~-3.3) is -3
  num-round(~-3.5) is -4
end

check "round of bigrats":
  num-round(10000000000000000) * 0 is 0
  num-round(+123123123123123123123123123.3) is 123123123123123123123123123
  num-round(+123123123123123123123123123.5) is 123123123123123123123123124
  num-round(+123123123123123123123123123.7) is 123123123123123123123123124
  num-round(-123123123123123123123123123.3) is -123123123123123123123123123
  num-round(-123123123123123123123123123.5) is -123123123123123123123123124
  num-round(-123123123123123123123123123.7) is -123123123123123123123123124
  num-round(+123123123123123123123123124.5) is 123123123123123123123123125
  num-round(-123123123123123123123123124.5) is -123123123123123123123123125
end

check "round-half-to-even":
  round-half-to-even(4) is 4
  round-half-to-even(3.7) is 4
  round-half-to-even(3.5) is 4
  round-half-to-even(3.3) is 3
  round-half-to-even(3) is 3
  round-half-to-even(2.5) is 2
  round-half-to-even(-2.5) is -2
  round-half-to-even(-3) is -3
  round-half-to-even(-3.3) is -3
  round-half-to-even(-3.5) is -4
  round-half-to-even(-3.7) is -4
  round-half-to-even(-4) is -4
  round-half-to-even(~3.5) is 4
  round-half-to-even(~2.5) is 2
end

check "round-even":
  num-round-even(4) is 4
  num-round-even(3.7) is 4
  num-round-even(3.5) is 4
  num-round-even(3.3) is 3
  num-round-even(3) is 3
  num-round-even(2.5) is 2
  num-round-even(-2.5) is -2
  num-round-even(-3) is -3
  num-round-even(-3.3) is -3
  num-round-even(-3.5) is -4
  num-round-even(-3.7) is -4
  num-round-even(-4) is -4
  num-round-even(~3.5) is 4
  num-round-even(~2.5) is 2
end
