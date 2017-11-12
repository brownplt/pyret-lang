import sets as S
import file("benchmark-base.arr") as B
fun main():
  for fold(s from [S.list-set:], i from range(0, 500)):
    s.add(i)
  end
end
B.benchmark(lam(): main() end, 50)
# test growing list-set
