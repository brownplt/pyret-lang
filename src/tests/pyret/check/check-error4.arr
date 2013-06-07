#lang pyret

fun f():

check:
  checkers.check-equals("obj = 42",{equals(not-enough-args): end}, 42)
  checkers.check-equals("0=0",0, 0)
end

