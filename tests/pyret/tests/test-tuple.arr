import parse-pyret as P
import pprint as PP

check "parse and print":
   x = P.surface-parse("{1; 2}", "test")
   x.tosource().pretty(80) is [list: "{ 1; 2 }"]
end


check "desugar to array":
   x = {1; 3; 10}
   raw-array-get(x, 0) is 1
   raw-array-get(x, 1) is 3
   raw-array-get(x, 2) is 10
end
