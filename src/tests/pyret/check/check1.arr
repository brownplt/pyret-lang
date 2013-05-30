#lang pyret

import test as T

fun length(l):
  l.length()
check
  T.assert-equals(length([1,2,3]), 3)
  T.assert-equals(length([]), 0)
  T.assert-equals(length([1]), 1)
  T.check-exn("number to length is an error",
              fun: length(0) end,
              fun(e): true end)
end

