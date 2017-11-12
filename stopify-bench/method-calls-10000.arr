import file("benchmark-base.arr") as B
r = range(0, 1000)
fun main():
  r.map(lam(i): i + 1 end)
end
B.benchmark(lam(): main() end, 50)
# test the cost of doing a lot of method calls, since .map() is recursive method calling
