data Loc deriving builtins.Eq:
     | loc(name :: String, latitude :: Number, longitude :: Number, 
       neighbors :: list.List, distances :: list.List)
end

  graph:
  tl1 = loc("a", 0, 0, [tl2, tl4, tl5], [10, 8, 21])
  tl2 = loc("b", 8, 6, [tl1, tl3, tl4, tl5], [10, 13, 6, 17])
  tl3 = loc("c", 13, 18, [tl2], [13])
  tl4 = loc("d", 8, 0, [tl1, tl2], [8, 6])
  tl5 = loc("e", 0, 21, [tl1, tl2], [21, 17])
  end
  test-print(tl1 == tl1)
  test-print(tl1.neighbors.first == tl2)
  test-print(torepr(tl1) == 'loc("a", 0, 0, [cyclic-field, cyclic-field, cyclic-field], [10, 8, 21])')
  test-print(String(tostring(tl1)) == true)
  test-print(String(torepr(tl2)) == true)
  test-print(String(tostring(tl2)) == true)