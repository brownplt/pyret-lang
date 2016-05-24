import parse-pyret as P
import pprint as PP

check "parse and print":
   x = P.surface-parse("{1; 2}", "test")
   x.tosource().pretty(80) is [list: "{ 1; 2 }"]
end


check "basic tuple access":
   x = {1; 3; 10}
   y = x.{2}
   x.{0} is 1
   x.{1} is 3
   x.{2} is 10
   x.{3} raises "too large"
   y.{0} raises "Tuple"
   x.{-1} raises "Index"
end


check "print tuple":
  x = {13; 1 + 4; 41; 1}
  torepr(x) is "{ 13; 5; 41; 1 }"
end

check "tuple equals":
 x = {1; 3; 5; 2}
 y = {1; 3; 5; 2}
 x is y
end

check "parse and print tuple-get":
   x = P.surface-parse("tup.{2}", "test")
   x.tosource().pretty(80) is [list: "tup.{2}"]
end
