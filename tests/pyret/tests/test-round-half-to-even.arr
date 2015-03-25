fun round-half-to-even(n):
  n-neg = (n < 0)
  n-abs = num-exact(num-abs(n))
  var n-round = num-round(n-abs)
  if num-abs(n-round - n-abs) == 1/2:
    if num-modulo(n-round, 2) == 1:
      n-round := n-round - 1
    else: nothing
    end
  else: nothing
  end
  if n-neg:
    n-round := 0 - n-round
  else: nothing
  end
  n-round
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
