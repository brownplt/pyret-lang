#lang pyret

fun f():
  
check:
  0 is 0
  1 is 0
  0 is 1
  raise("Done checking")
  1 is 1
end

fun h():

check:
  0 is 0
  0 is 1
end

