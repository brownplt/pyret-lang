import file("provide-type-as-simple.arr") as P

include from P: type B end

check:
  x :: B = {1; 3}
  x is {1; 3}
end


