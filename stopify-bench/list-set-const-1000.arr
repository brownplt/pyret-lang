import file("benchmark-base.arr") as B
import sets as S
fun main():
  for fold(s from [S.list-set:], _ from range(0, 1000)):
    s.add(0)
  end
end

B.benchmark(lam(): main() end, 500)
# test repeatedly adding same element to a list-set
