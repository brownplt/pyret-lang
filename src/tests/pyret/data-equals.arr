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
    | override with: _equals(self, other): true end
    | hasy(y)
    | rec2(a :: D2, b :: D1)
  end

  single is single
  noargs() is noargs()
  (noargs() == single) is false
  (noargs() == single2) is false
  (single2 == single) is false
  (single == single2) is false

  hasx(5) is hasx(5)
  (hasx(6) == hasx(5)) is false
  (hasx(6) == hasy(6)) is false
  (hasx(6) == hasy(5)) is false

  rec(single, single2) is rec(single, single2)
  rec(single, hasy(5)) is rec(single, hasy(5))
  (rec(single, single2) == rec(noargs(), single2)) is false
  rec2(hasy(4), single) is rec2(hasy(4), single)

  rec(rec(single, rec2(single2, single)), single2) is
    rec(rec(single, rec2(single2, single)), single2)

  (rec(rec(single, rec2(hasy(4), single)), single2) ==
    rec(rec(single, rec2(single2, single)), single2)) is false
  
  hasy({x: single}) is hasy({x: single})
  (hasy({x: single}) == hasy({x: single2})) is false
  hasy({x: rec2(override, single)}) is hasy({x: rec2(hasy(4), single)})
  (hasy({x: rec2(override, hasx(4))}) == hasy({x: rec2(hasy(4), single)})) is false

  hasy([]) is hasy([])
  hasy([{x: rec(single, hasy(["foo"]))}]) is
    hasy([{x: rec(single, hasy(["foo"]))}])
  (hasy([{x: rec(single, hasy(["foo"]))}]) ==
    hasy([{x: rec(single, hasy(["bar"]))}])) is false

  override is single
  override is noargs()
  override is hasx(5)
  override is hasy(5)

end

