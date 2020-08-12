import file("provide-arrow-using-datatype.arr") as D

result = D.f().x
check:
  result is 10
end
