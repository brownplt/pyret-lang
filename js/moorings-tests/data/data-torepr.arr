  data MyRepr:
    | d1
    | d4()
    | d2(a :: MyRepr, b :: String)
  end

  test-print(torepr(d2(d1, "foo")))
  test-print(torepr(d2(d2(d1, "bar"), "foo")))
  test-print(torepr(d1))

  data MyOtherRepr:
    | d3(x :: MyRepr, y :: list.List)
  end

  test-print(torepr(d3(d1, ["1"])))
  test-print(torepr(d3(   d2( d4(), "2"),[ "1"])))