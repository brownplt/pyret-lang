b1 = brander()
b2 = brander()
o = b1.brand({})
o2 = o.{x:5}
test-middle = b1.test(o2)
o3 = b2.brand(o2.{x:10})
b2.test(o3) and test-middle and (not b1.test(o3))
