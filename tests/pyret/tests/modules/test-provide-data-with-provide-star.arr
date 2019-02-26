import file("./provide-data-with-provide-star.arr") as P

x = cases(P.D) P.d1:
  | d1 => 5
  | d2 => 10
end

result = P.d1.method-name()

check:
  x is 5
  result is "has a method"
end

