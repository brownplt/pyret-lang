import file as F
import ast as A
import parse-pyret as P

print(A.dummy-loc)
print(F.input-file("test.txt").read-file())
print(P.surface-parse("x = 10", "test"))

