#lang pyret

eq = checkers.check-equals

fun len(l):
  l.length()
check
  eq(len([1,2,3]), 3)
  eq(len([]), 0)
  eq(len([1]), 1)
end

