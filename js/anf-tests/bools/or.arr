var x = 0
fun f():
  x := 10
  false
end
test-print(true or f())
test-print(x)
test-print(false or f())
test-print(x)

