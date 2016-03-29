import "tests/data1.arr" as C
import "tests/data2.arr" as D

check:
  C.foo(1) == D.foo(1) is false
  C.is-foo(D.foo(1)) is false
end
