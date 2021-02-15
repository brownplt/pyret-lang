fun round-half-to-even(n) block:
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

check "truncating roughs":
  num-truncate(~3.5) is 3
  num-truncate(~3.3) is 3
  num-truncate(~3) is 3
  num-truncate(~2.7) is 2
  num-truncate(~2.5) is 2
  num-truncate(~-2.5) is -2
  num-truncate(~-2.7) is -2
  num-truncate(~-3) is -3
  num-truncate(~-3.3) is -3
  num-truncate(~-3.5) is -3
end

check "truncating roughs at digits":
  for each(pow from [list: 0, -3, 3]) block:
    tenDigit = num-expt(10, pow)
    num-truncate-digits(~3.5 / tenDigit, pow) is 3 / tenDigit
    num-truncate-digits(~3.3 / tenDigit, pow) is 3 / tenDigit
    num-truncate-digits(~3 / tenDigit, pow) is 3 / tenDigit
    num-truncate-digits(~2.7 / tenDigit, pow) is 2 / tenDigit
    num-truncate-digits(~2.5 / tenDigit, pow) is 2 / tenDigit
    num-truncate-digits(~-2.5 / tenDigit, pow) is -2 / tenDigit
    num-truncate-digits(~-2.7 / tenDigit, pow) is -2 / tenDigit
    num-truncate-digits(~-3 / tenDigit, pow) is -3 / tenDigit
    num-truncate-digits(~-3.3 / tenDigit, pow) is -3 / tenDigit
    num-truncate-digits(~-3.5 / tenDigit, pow) is -3 / tenDigit
  end
end
check "truncating roughs at place":
  for each(pow from [list: 0, -3, 3]) block:
    tenDigit = num-expt(10, pow)
    num-truncate-place(~3.5 * tenDigit, pow) is 3 * tenDigit
    num-truncate-place(~3.3 * tenDigit, pow) is 3 * tenDigit
    num-truncate-place(~3 * tenDigit, pow) is 3 * tenDigit
    num-truncate-place(~2.7 * tenDigit, pow) is 2 * tenDigit
    num-truncate-place(~2.5 * tenDigit, pow) is 2 * tenDigit
    num-truncate-place(~-2.5 * tenDigit, pow) is -2 * tenDigit
    num-truncate-place(~-2.7 * tenDigit, pow) is -2 * tenDigit
    num-truncate-place(~-3 * tenDigit, pow) is -3 * tenDigit
    num-truncate-place(~-3.3 * tenDigit, pow) is -3 * tenDigit
    num-truncate-place(~-3.5 * tenDigit, pow) is -3 * tenDigit
  end
end

check "flooring roughs":
  num-floor(~3.5) is 3
  num-floor(~3.3) is 3
  num-floor(~3) is 3
  num-floor(~2.7) is 2
  num-floor(~2.5) is 2
  num-floor(~-2.5) is -3
  num-floor(~-2.7) is -3
  num-floor(~-3) is -3
  num-floor(~-3.3) is -4
  num-floor(~-3.5) is -4
end

check "flooring roughs at digits":
  for each(pow from [list: 0, -3, 3]) block:
    tenDigit = num-expt(10, pow)
    num-floor-digits(~3.5 / tenDigit, pow) is 3 / tenDigit
    num-floor-digits(~3.3 / tenDigit, pow) is 3 / tenDigit
    num-floor-digits(~3 / tenDigit, pow) is 3 / tenDigit
    num-floor-digits(~2.7 / tenDigit, pow) is 2 / tenDigit
    num-floor-digits(~2.5 / tenDigit, pow) is 2 / tenDigit
    num-floor-digits(~-2.5 / tenDigit, pow) is -3 / tenDigit
    num-floor-digits(~-2.7 / tenDigit, pow) is -3 / tenDigit
    num-floor-digits(~-3 / tenDigit, pow) is -3 / tenDigit
    num-floor-digits(~-3.3 / tenDigit, pow) is -4 / tenDigit
    num-floor-digits(~-3.5 / tenDigit, pow) is -4 / tenDigit
  end
end


check "flooring roughs at place":
  for each(pow from [list: 0, -3, 3]) block:
    tenDigit = num-expt(10, pow)
    num-floor-place(~3.5 * tenDigit, pow) is 3 * tenDigit
    num-floor-place(~3.3 * tenDigit, pow) is 3 * tenDigit
    num-floor-place(~3 * tenDigit, pow) is 3 * tenDigit
    num-floor-place(~2.7 * tenDigit, pow) is 2 * tenDigit
    num-floor-place(~2.5 * tenDigit, pow) is 2 * tenDigit
    num-floor-place(~-2.5 * tenDigit, pow) is -3 * tenDigit
    num-floor-place(~-2.7 * tenDigit, pow) is -3 * tenDigit
    num-floor-place(~-3 * tenDigit, pow) is -3 * tenDigit
    num-floor-place(~-3.3 * tenDigit, pow) is -4 * tenDigit
    num-floor-place(~-3.5 * tenDigit, pow) is -4 * tenDigit
  end
end


check "ceiling roughs":
  num-ceiling(~3.5) is 4
  num-ceiling(~3.3) is 4
  num-ceiling(~3) is 3
  num-ceiling(~2.7) is 3
  num-ceiling(~2.5) is 3
  num-ceiling(~-2.5) is -2
  num-ceiling(~-2.7) is -2
  num-ceiling(~-3) is -3
  num-ceiling(~-3.3) is -3
  num-ceiling(~-3.5) is -3
end

check "ceiling roughs at digits":
  for each(pow from [list: 0, -3, 3]) block:
    tenDigit = num-expt(10, pow)
    num-ceiling-digits(~3.5 / tenDigit, pow) is 4 / tenDigit
    num-ceiling-digits(~3.3 / tenDigit, pow) is 4 / tenDigit
    num-ceiling-digits(~3 / tenDigit, pow) is 3 / tenDigit
    num-ceiling-digits(~2.7 / tenDigit, pow) is 3 / tenDigit
    num-ceiling-digits(~2.5 / tenDigit, pow) is 3 / tenDigit
    num-ceiling-digits(~-2.5 / tenDigit, pow) is -2 / tenDigit
    num-ceiling-digits(~-2.7 / tenDigit, pow) is -2 / tenDigit
    num-ceiling-digits(~-3 / tenDigit, pow) is -3 / tenDigit
    num-ceiling-digits(~-3.3 / tenDigit, pow) is -3 / tenDigit
    num-ceiling-digits(~-3.5 / tenDigit, pow) is -3 / tenDigit
  end
end

check "ceiling roughs at place":
  for each(pow from [list: 0, -3, 3]) block:
    tenDigit = num-expt(10, pow)
    num-ceiling-place(~3.5 * tenDigit, pow) is 4 * tenDigit
    num-ceiling-place(~3.3 * tenDigit, pow) is 4 * tenDigit
    num-ceiling-place(~3 * tenDigit, pow) is 3 * tenDigit
    num-ceiling-place(~2.7 * tenDigit, pow) is 3 * tenDigit
    num-ceiling-place(~2.5 * tenDigit, pow) is 3 * tenDigit
    num-ceiling-place(~-2.5 * tenDigit, pow) is -2 * tenDigit
    num-ceiling-place(~-2.7 * tenDigit, pow) is -2 * tenDigit
    num-ceiling-place(~-3 * tenDigit, pow) is -3 * tenDigit
    num-ceiling-place(~-3.3 * tenDigit, pow) is -3 * tenDigit
    num-ceiling-place(~-3.5 * tenDigit, pow) is -3 * tenDigit
  end
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

check "rounding roughs at digits":
  for each(pow from [list: 0, -3, 3]) block:
    tenDigit = num-expt(10, pow)
    num-round-digits(~3.5 / tenDigit, pow) is 4 / tenDigit
    num-round-digits(~3.3 / tenDigit, pow) is 3 / tenDigit
    num-round-digits(~3 / tenDigit, pow) is 3 / tenDigit
    num-round-digits(~2.7 / tenDigit, pow) is 3 / tenDigit
    num-round-digits(~2.5 / tenDigit, pow) is 3 / tenDigit
    num-round-digits(~-2.5 / tenDigit, pow) is -3 / tenDigit
    num-round-digits(~-2.7 / tenDigit, pow) is -3 / tenDigit
    num-round-digits(~-3 / tenDigit, pow) is -3 / tenDigit
    num-round-digits(~-3.3 / tenDigit, pow) is -3 / tenDigit
    num-round-digits(~-3.5 / tenDigit, pow) is -4 / tenDigit
  end
end
check "rounding roughs at place":
  for each(pow from [list: 0, -3, 3]) block:
    tenDigit = num-expt(10, pow)
    num-round-place(~3.5 * tenDigit, pow) is 4 * tenDigit
    num-round-place(~3.3 * tenDigit, pow) is 3 * tenDigit
    num-round-place(~3 * tenDigit, pow) is 3 * tenDigit
    num-round-place(~2.7 * tenDigit, pow) is 3 * tenDigit
    num-round-place(~2.5 * tenDigit, pow) is 3 * tenDigit
    num-round-place(~-2.5 * tenDigit, pow) is -3 * tenDigit
    num-round-place(~-2.7 * tenDigit, pow) is -3 * tenDigit
    num-round-place(~-3 * tenDigit, pow) is -3 * tenDigit
    num-round-place(~-3.3 * tenDigit, pow) is -3 * tenDigit
    num-round-place(~-3.5 * tenDigit, pow) is -4 * tenDigit
  end
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

check "round-even at digits":
  for each(pow from [list: 0, -3, 3]) block:
    tenDigit = num-expt(10, pow)
    num-round-even-digits(4 / tenDigit, pow) is 4 / tenDigit
    num-round-even-digits(3.7 / tenDigit, pow) is 4 / tenDigit
    num-round-even-digits(3.5 / tenDigit, pow) is 4 / tenDigit
    num-round-even-digits(3.3 / tenDigit, pow) is 3 / tenDigit
    num-round-even-digits(3 / tenDigit, pow) is 3 / tenDigit
    num-round-even-digits(2.5 / tenDigit, pow) is 2 / tenDigit
    num-round-even-digits(-2.5 / tenDigit, pow) is -2 / tenDigit
    num-round-even-digits(-3 / tenDigit, pow) is -3 / tenDigit
    num-round-even-digits(-3.3 / tenDigit, pow) is -3 / tenDigit
    num-round-even-digits(-3.5 / tenDigit, pow) is -4 / tenDigit
    num-round-even-digits(-3.7 / tenDigit, pow) is -4 / tenDigit
    num-round-even-digits(-4 / tenDigit, pow) is -4 / tenDigit
    num-round-even-digits(~3.5 / tenDigit, pow) is 4 / tenDigit
    num-round-even-digits(~2.5 / tenDigit, pow) is 2 / tenDigit
  end
end

check "round-even at place":
  for each(pow from [list: 0, -3, 3]) block:
    tenDigit = num-expt(10, pow)
    # at units
    num-round-even-place(4 * tenDigit, pow) is 4 * tenDigit
    num-round-even-place(3.7 * tenDigit, pow) is 4 * tenDigit
    num-round-even-place(3.5 * tenDigit, pow) is 4 * tenDigit
    num-round-even-place(3.3 * tenDigit, pow) is 3 * tenDigit
    num-round-even-place(3 * tenDigit, pow) is 3 * tenDigit
    num-round-even-place(2.5 * tenDigit, pow) is 2 * tenDigit
    num-round-even-place(-2.5 * tenDigit, pow) is -2 * tenDigit
    num-round-even-place(-3 * tenDigit, pow) is -3 * tenDigit
    num-round-even-place(-3.3 * tenDigit, pow) is -3 * tenDigit
    num-round-even-place(-3.5 * tenDigit, pow) is -4 * tenDigit
    num-round-even-place(-3.7 * tenDigit, pow) is -4 * tenDigit
    num-round-even-place(-4 * tenDigit, pow) is -4 * tenDigit
    num-round-even-place(~3.5 * tenDigit, pow) is 4 * tenDigit
    num-round-even-place(~2.5 * tenDigit, pow) is 2 * tenDigit
  end
end
