#lang pyret

fun f():

where:
  checkers.check-equals("obj = 42",{equals(not-enough-args): end}, 42)
  checkers.check-equals("0=0",0, 0)
end
