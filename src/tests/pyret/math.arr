#lang pyret

import test as T
import math as M
provide { run-tests: run-tests } end

fun run-tests():

  fun f():
    for list.fold(acc from true, _ from list.range(0, 1000)):
      val = M.sample(M.uniform-dist(0, 100))
      acc.and(val > 0).and(val < 100)
    end
  end
    
  T.check-equals("Test uniform", f, true)

end
