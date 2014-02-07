#lang pyret
check:
  string-append("a", "b") is "ab"
  string-append("a", 5) raises "expected String"
  string-append(5, 5) raises "expected String"

  string-length("a") is 1
  string-length("abcd") is 4
  string-length(4) raises "expected String"

  sq(4) is 16
  sq(9) is 81
  sq(-7) is 49

  sqrt(9) is 3
  sqrt(16) is 4
  sqrt(-1) raises "negative"

  expt(2, 4) is 16
  expt(3, 3) is 27
  expt(4, .5) is 2
end

