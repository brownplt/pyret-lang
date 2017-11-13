import string-dict as SD
import file("./benchmark-base.arr") as B

m = [SD.mutable-string-dict:]
fun main():
  for map(i from range(0, 10000)):
    m.set-now(num-to-string(i), true)
  end
end
# test growing mutable-string-dict

B.benchmark(lam(): main() end, 500)
