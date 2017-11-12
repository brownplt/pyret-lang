import sets as S
import file("benchmark-base.arr") as B
fun main():
  for fold(s from [S.tree-set:], _ from range(0, 1000)):
    s.add(0)
  end
end
B.benchmark(lam(): main() end, 50)
# test repeatedly adding the same element to a tree-set
