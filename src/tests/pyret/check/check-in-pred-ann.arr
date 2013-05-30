#lang pyret

import test as T

var x :: Number(fun(n):
    fun f():
      4  
    check
      T.assert-equals(f(), 4)
    end
    true
  end) = 5

T.assert-passed-results(1)

