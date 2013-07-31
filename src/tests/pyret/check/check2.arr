#lang pyret

eq = checkers.check-equals

fun odd(n):
  if n == 0: false
  else if n == 1: true
  else: even(n - 1)
  end
check:
  eq("odd(9)",odd(9), true)
  eq("odd(8)",odd(8), false)
end


fun even(n):
  if n == 0: true
  else if n == 1: false
  else: odd(n - 1)
  end
check:
  eq("even(9)",even(9), false)
  eq("even(8)",even(8), true)
end

