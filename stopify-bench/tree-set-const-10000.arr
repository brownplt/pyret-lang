import sets as S
import file("benchmark-base.arr") as B
fun main():
  for fold(s from [S.tree-set:], _ from range(0, 10000)):
    s.add(0)
  end
end
B.benchmark(lam(): main() end, 1)
# test repeatedly adding the same element to a tree-set
