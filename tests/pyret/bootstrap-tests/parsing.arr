import parse-pyret as P
import parse-errors as PE
import error as E
import srcloc as S

check:
  p = P.parse-dialect("Bootstrap", _, "test")
  pe = PE.detect-parse-errors("Bootstrap", _, "test")
  l = S.srcloc("file", _, _, _, _, _, _)

  errs1 = pe("fun f() 5 end")
  print(errs1)
  errs1 is [E.fun-missing-colon(l(1, 8, 8, 1, 8, 8))]

  errs2 = pe("fun f() end")
  print(errs2)
  errs2 is [
      E.fun-missing-colon(l(1, 8, 8, 1, 8, 8))
#      E.fun-empty-block(l(1, 8, 8, 1, 8, 8))
    ]

  errs3 = pe(
"fun f(x)
  5

fun g():
  10
end
15")
  print(errs3)
  errs3 is [
      E.fun-missing-colon(l(1, 9, 9, 1, 9, 9)),
      E.fun-missing-end(l(2, 1, 12, 2, 1, 12))
    ]

  errs4 = pe(
"fun f(x)
  print(x)
  1

fun g():
  10
end
15")
  print(errs4)
end

