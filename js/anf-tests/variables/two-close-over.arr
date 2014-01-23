var x = 0
fun f(): x := x + 1 end
fun g(): x := x * 10 end
fun getx(): x end
f()
test-print(x)
test-print(getx())
g()
test-print(x)
test-print(getx())
g()
test-print(x)
test-print(getx())
f()
test-print(x)
test-print(getx())
x := 5
test-print(x)
test-print(getx())
f()
test-print(x)
test-print(getx())
g()
test-print(x)
test-print(getx())