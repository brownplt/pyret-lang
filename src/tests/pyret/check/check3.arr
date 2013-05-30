#lang pyret

import test as T

fun expt(x, n):
  fun mult(x, y):
    x * y
  check
    T.assert-equals(mult(3,4), 12)
    for list.map(i from list.range(0, 10)):
      T.assert-equals(mult(0,i), 0)
    end
  end

  for list.fold(acc from 1, i from list.range(0, n)):
    mult(acc, x)
  end
check
  T.assert-equals(expt(5, 0), 1)
  T.assert-equals(expt(0, 5), 0)
  T.assert-equals(expt(2, 4), 16)
end

# should run
# 1 +
# 56 +
# 45
# = 102 passing tests
T.assert-passing-results(102)

