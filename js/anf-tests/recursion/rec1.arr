var f = 0
f := fun(x):
  if x < 1:
    1
  else:
    x + f(x - 1)
  end
end
test-print(f(1000))
