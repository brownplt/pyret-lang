b = brander()
f = b.brand(fun(x): x end)
f(true) and b.test(f)
