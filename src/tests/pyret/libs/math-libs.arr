#lang pyret

check:
  1.exp().log() is 1
  (2.3).floor() is 2
  (2.3).ceiling() is 3
  pi.cos() is -1
  (pi.sin() < 0.00001) is true
  0.sin() is 0
  0.cos() is 1
end
