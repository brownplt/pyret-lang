#lang pyret

import test as T

fun greater_than_4(n):
  n > 4
check
  T.assert-true(greater_than_4(5))
  T.assert-true(greater_than_4(3))
end

T.assert-passing-results(1)

