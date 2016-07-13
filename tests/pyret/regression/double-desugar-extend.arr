maker = {
  make: lam(_): {x:5} end,
  make0: lam(): {x:5} end,
  make1: lam(a): {x:5} end,
  make2: lam(a, b): {x:5} end,
  make3: lam(a, b, c): {x:5} end,
  make4: lam(a, b, c ,d): {x:5} end,
  make5: lam(a, b, c, d, e): {x:5} end,
}
check:
  [maker:].{x:10} is {x:10}
end

