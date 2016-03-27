import file as F
import ast as A
import parse-pyret as P
import builtin-modules as B
import cmdline-lib as C
import checker as CH
import format as FMT
import string-dict as SD

print(A.dummy-loc)
print(F.input-file("test.txt").read-file())
print(P.surface-parse("x = 10", "test"))

l = B.builtin-raw-locator("src/js/troveA/builtin-modules")

print(string-substring(l.get-raw-compiled(), 0, 30))

print(C.command-line-arguments())

print(CH.render-check-results([list:]))

print(FMT.format("~a ~a", [list: {x:5}, true]))

print([SD.string-dict: "a", 5])
