import sets as S
import file("benchmark-base.arr") as B
fun main():
  for fold(s from [S.list-set:], i from range(0, 1000)):
    s.add(i)
  end
end
B.benchmark-base(lam(): main() end, 1)
# test growing list-set
