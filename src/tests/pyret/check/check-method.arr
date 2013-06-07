#lang pyret

eq = checkers.check-equals

fun len(l):
  m = method(self, l): l.length() end
  {m:m}.m(l)
check:
  eq("len([1,2,3])=3",len([1,2,3]), 3)
  eq("len([])=0",len([]), 0)
  eq("len([1])=1",len([1]), 1)
end



