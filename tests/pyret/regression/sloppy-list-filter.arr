
check: 
  [list: -1, 0, 1, 2].filter(lam(x): x end) 
    raises "Boolean"
  [list: "hello", "", "world", "a"].filter(lam(x): x end) 
    raises "Boolean"
end
