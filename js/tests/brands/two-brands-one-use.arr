b1 = brander()
b2 = brander()
o = b1.brand({})
(not b2.test(o)) and b1.test(o)