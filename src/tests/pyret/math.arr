#lang pyret

import "test.arr" as T
import math as M
provide { run-tests: run-tests } end

fun run-tests():

  fun f(i):
    cond:
    | i.lessthan(1000) => 
      val = M.sample(M.uniform-dist(0, 100))
      val.greaterthan(0).and(val.lessthan(100)).and(f(i.plus(1)))
    | else => true
    end
  end
  T.check-equals("Test uniform", \(f(0)), true)

end
