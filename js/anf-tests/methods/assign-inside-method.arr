var x = 10
o = {
  m(self):
    x := x + 1
    x
  end
}
test-print(o.m())
test-print(x)
test-print(o.m())
test-print(x)

test-print(o:m(x, x))
test-print(x)
test-print(o:m(x, x))
test-print(x)