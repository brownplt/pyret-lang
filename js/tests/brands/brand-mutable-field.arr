b = brander()
o = b.brand({ mutable x: 5 })
o!{x : 10}
b.test(o)
