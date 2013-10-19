#lang pyret

check:
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

  single.eq(single) is true
  single.eq(noargs()) is false
  identical(single, single) is true
  identical(single, noargs()) is false

  hasx(5).eq(hasx(5)) is false
  x1 = hasx(5)
  identical(x1, x1) is true

  r1 = rec(single, single2)
  identical(r1, r1) is true
  identical(r1.a, r1.a) is true
  identical(r1.b, r1.b) is true
  r2 = rec(single, single2)
  identical(r1, r2) is false
  identical(r1.a, r2.a) is true
  identical(r1.b, r2.b) is true

  inner-rec = rec2(single2, single)
  long-rec1 = rec(rec(single, inner-rec), single2)
  long-rec2 = rec(rec(single, inner-rec), single2)

  identical(long-rec1, long-rec1) is true
  identical(long-rec1, long-rec2) is false

  identical(long-rec1.a, long-rec2.a) is false
  identical(long-rec1.b, long-rec2.b) is true
  identical(long-rec1.a.b, long-rec2.a.b) is true

  # can't override this; mixin wins for now
  override.eq(override) is true
  
end

