#lang pyret

fun f():

check
  checkers.check-equals({equals(not-enough-args): end}, 42)
  checkers.check-equals(0, 0)
end

