var x = 10
f = fun():
  test-print(x)
  x := 3
  test-print(x)
end
test-print(x)
f()
test-print(x)
g = fun():
  test-print(x)
  x := 5
  test-print(x)
end
test-print(x)
g()
test-print(x)
f()
test-print(x)
g()
test-print(x)

