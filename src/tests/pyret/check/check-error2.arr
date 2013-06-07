#lang pyret

fun f():
  
check:
  checkers.check-equals(0, 0)
  checkers.check-equals(0, 1)
  raise("Done checking")
  checkers.check-equals(1, 1)
end

fun h():

check:
  checkers.check-equals(0, 0)
  checkers.check-equals(0, 1)
end

