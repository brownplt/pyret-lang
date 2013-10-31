#lang pyret

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

  test-print(single == single)
  test-print(noargs() == noargs())
  test-print((noargs() == single))
  test-print((noargs() == single2))
  test-print((single2 == single))
  test-print((single == single2))

  test-print(hasx(5) == hasx(5))
  test-print((hasx(6) == hasx(5)))
  test-print((hasx(6) == hasy(6)))
  test-print((hasx(6) == hasy(5)))

  test-print(rec(single, single2) == rec(single, single2))
  test-print(rec(single, hasy(5)) == rec(single, hasy(5)))
  test-print((rec(single, single2) == rec(noargs(), single2)))
  test-print(rec2(hasy(4), single) == rec2(hasy(4), single))

  test-print(rec(rec(single, rec2(single2, single)), single2) == rec(rec(single, rec2(single2, single)), single2))

  test-print((rec(rec(single, rec2(hasy(4), single)), single2) == rec(rec(single, rec2(single2, single)), single2)))
  
  test-print(hasy({x: single}) == hasy({x: single}))
  test-print((hasy({x: single}) == hasy({x: single2})))
  test-print(hasy({x: rec2(override, single)}) == hasy({x: rec2(hasy(4), single)}))
  test-print((hasy({x: rec2(override, hasx(4))}) == hasy({x: rec2(hasy(4), single)})) == false)

  test-print(hasy([]) == hasy([]))
  test-print(hasy([{x: rec(single, hasy(["foo"]))}]) == hasy([{x: rec(single, hasy(["foo"]))}]))
  test-print((hasy([{x: rec(single, hasy(["foo"]))}]) == hasy([{x: rec(single, hasy(["bar"]))}])))

  test-print(override == single)
  test-print(override == noargs())
  test-print(override == hasx(5))
  test-print(override == hasy(5))
