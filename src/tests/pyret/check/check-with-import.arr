#lang pyret

import "check1.arr" as check1

fun f():
where:
  checkers.check-equals("tough one",1,1)
end

# Should only run 1 test (no tests from check1.arr should run)
