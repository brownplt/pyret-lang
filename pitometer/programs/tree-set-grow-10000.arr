import sets as S
for fold(s from [S.tree-set:], i from range(0, 10000)):
  s.add(i)
end
# test growing tree-set
