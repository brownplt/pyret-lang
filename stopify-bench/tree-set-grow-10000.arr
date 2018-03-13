import sets as S
import file("benchmark-base.arr") as B
fun main():
  for fold(s from [S.tree-set:], i from range(0, 10000)):
    s.add(i)
  end
end
B.benchmark(lam(): main() end, 1)
# test growing tree-set
