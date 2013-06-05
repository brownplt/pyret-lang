#lang pyret

var x :: Number(fun(n):
    fun f():
      4  
    check
      checkers.check-equals(f(), 4)
    end
    true
  end) = 5

