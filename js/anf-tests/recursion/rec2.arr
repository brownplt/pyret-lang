var f = 0
var g = 0
nt = fun(b): if b: false else: true;;
f := fun(x):
  if x < 1:
    true
  else:
    nt(g(x - 1))
  end
end
g := fun(x):
  if x < 2:
    true
  else:
    nt(f(x - 1))
  end
end
test-print(f(100001))
test-print(f(200000))
test-print(g(300000))
test-print(g(400001))

