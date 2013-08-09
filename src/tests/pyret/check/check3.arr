#lang pyret

eq = checkers.check-equals

fun expt(x, n):
  fun mult(x, y):
    x * y
  check:
    eq("mult(3,4)=12",mult(3,4), 12)
    for list.map(i from list.range(0, 10)):
      eq("mult(0,"+tostring(i)+")=0", mult(0,i), 0)
    end
  end

  for list.fold(acc from 1, i from list.range(0, n)):
    mult(acc, x)
  end
check:
  expt(5, 0) is 1
  expt(0, 5) is 0
  expt(2, 4) is 16
end

# should run
# 12 +
# 12 +
# 12
# = 36 passing tests

