import parse-pyret as P
import pprint as PP

check "parse and print":
   x = P.surface-parse("{1; 2}", "test")
   x.tosource().pretty(80) is [list: "{ 1; 2 }"]
end


check "desugar to array":
   x = {1; 3; 10}
   x.v1 is 1
   x.v2 is 3
   x.v3 is 10
end


check "print tuple":
  x = {13; 1 + 4; 41; 1}
  torepr(x) is "{ 13; 5; 41; 1 }"
end
