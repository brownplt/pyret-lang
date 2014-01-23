var x = 1
fun f(y):
  fun g():
    x := y
  end
  x := 10
  g
end
test-print(x)
g1 = f(15)
test-print(x)
g1()
test-print(x)
g2 = f(20)
test-print(x)
g1()
test-print(x)
g2()
test-print(x)
g1()
test-print(x)
