import file("./benchmark-base.arr") as B

fun main():
  range(0,1000)
end
# test allocation of a lot of links

B.benchmark(lam(): main() end, 1000)
