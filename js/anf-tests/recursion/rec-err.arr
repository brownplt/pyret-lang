#lang pyret
var f = 0
var g = 0
nt = fun(b): if b: false else: true;;
f := fun(x):
  if x < 1:
    f(1, 2)
  else:
    nt(g(x - 1))
  end
end
g := fun(x):
  if x < 1:
    true
  else:
    nt(f(x - 1))
  end
end
test-print(f(1000))

