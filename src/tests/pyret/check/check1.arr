#lang pyret

import test as T

eq = checkers.check-equals

fun len(l):
  l.length()
check
  print(len([1,2,3]))
  print(list.List([1,2,3]))
  print(list.List(len([1,2,3])))
  eq(len([1,2,3]), 3)
  eq(len([]), 0)
  eq(len([1]), 1)
  T.check-exn("number to length is an error",
              fun: len(0) end,
              fun(e): true end)
end

