import parse-pyret as P
import pprint as PP

check "parse and print":
   x = P.surface-parse("{1; 2}", "test")
   x.tosource().pretty(80) is [list: "{ 1; 2 }"]
end
