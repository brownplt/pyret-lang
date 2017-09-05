r = range(0, 10000)
r.map(lam(i): i + 1 end)
# test the cost of doing a lot of method calls, since .map() is recursive method calling
