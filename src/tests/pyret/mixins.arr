#lang pyret

var times-eq-ran = 0
fun Eq():
  times-eq-ran := times-eq-ran + 1
  b = brander()
  {
    extend: fun(obj):
        obj.{eq(self, other): b.test(self) and b.test(other) end}
      end,
    brand: fun(obj): b.brand(obj) end
  }
end
fun Show(value):
  { extend: fun(obj): obj.{show: value} end, brand: fun(obj): obj end }
end   

    
data Test deriving Eq, Show("of hands"):
  | case1
  | case2(x)
end

check:
  case1.eq(case1) is true

  c21 = case2(1)
  c22 = case2(2)

  c21.eq(c21) is true
  c21.eq(c22) is false
  c22.eq(c21) is false
  c22.eq(c22) is true

  case1.show is "of hands"
  c21.show is "of hands"
  c22.show is "of hands"

  times-eq-ran is 3
end
