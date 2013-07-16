#lang pyret

fun f():
check:
  checkers.check-true("variable defined after", x)
end
x = true
