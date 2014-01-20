var x = 0
fun f():
  x := 10
  false
end
test-print(true and f())
test-print(x)
test-print(false and f())
test-print(x)

