#lang pyret

eq = checkers.check-equals

fun greater_than_4(n):
  n > 4
check:
  eq(greater_than_4(5), true)
  eq(greater_than_4(3), true)
end

