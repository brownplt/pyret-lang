import file("provide-datatype-as-type-alias.arr") as M

d :: M.D = M.d1
check:
  d is M.d1
end
