#lang pyret

check:
  p = mk-placeholder()
  p.get() raises "Tried to get value"
  p.set(5)
  p.get() is 5
end

check:
  p = mk-placeholder()
  p.guard(fun(n :: Number): n end)
  p.set("not-a-num") raises "expected Number"
end

check:
  p = mk-placeholder()
  p.guard(fun(n :: Number): n end)
  p.guard(fun(s :: String): s end)
  p.set("not-a-num") raises "expected Number"
  p.set(5) raises "expected String"
end

check:
  p = mk-placeholder()
  p.set(42)
  p.guard(fun(n :: Number): n end) raises "Tried to add guard on an already-initialized"
  p.set(10) raises "Tried to set value in already-initialized"
end

