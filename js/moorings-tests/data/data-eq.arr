#lang pyret

  data D1:
    | single
    | noargs()
    | hasx(x :: Number)
    | rec(a :: D1, b :: D2)
  end

  data D2:
    | single2
    | override with: eq(self, other): false end
    | hasy(y)
    | rec2(a :: D2, b :: D1)
  end

  test-print(single.eq(single))
  test-print(single.eq(noargs()))
  test-print(identical(single, single))
  test-print(identical(single, noargs()))

  test-print(hasx(5).eq(hasx(5)))
  x1 = hasx(5)
  test-print(identical(x1, x1))

  r1 = rec(single, single2)
  test-print(identical(r1, r1))
  test-print(identical(r1.a, r1.a))
  test-print(identical(r1.b, r1.b))
  r2 = rec(single, single2)
  test-print(identical(r1, r2))
  test-print(identical(r1.a, r2.a))
  test-print(identical(r1.b, r2.b))

  inner-rec = rec2(single2, single)
  long-rec1 = rec(rec(single, inner-rec), single2)
  long-rec2 = rec(rec(single, inner-rec), single2)

  test-print(identical(long-rec1, long-rec1))
  test-print(identical(long-rec1, long-rec2))

  test-print(identical(long-rec1.a, long-rec2.a))
  test-print(identical(long-rec1.b, long-rec2.b))
  test-print(identical(long-rec1.a.b, long-rec2.a.b))

  # can't override this; mixin wins for now
  test-print(override.eq(override))
