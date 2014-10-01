check:
  map2(_(_),
    [list: lam(x): 2 * x end, lam(x): x end],
    [list: 2, 3]) is [list: 4, 3]
  (_ + _)(2) raises "arity"
end
