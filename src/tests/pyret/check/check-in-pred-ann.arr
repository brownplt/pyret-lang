#lang pyret

var x :: Number(fun(n):
    fun f():
      4
    where:
      checkers.check-equals("f()=4",f(), 4)
    end
    true
  end) = 5
