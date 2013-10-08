b1 = brander()
b2 = brander()
o = b1.brand(b2.brand({}))
b1.test(o) and b2.test(o)
