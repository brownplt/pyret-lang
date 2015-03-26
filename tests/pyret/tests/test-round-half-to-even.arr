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
