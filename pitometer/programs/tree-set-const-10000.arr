import sets as S
z = for fold(s from [S.tree-set:], _ from range(0, 10000)):
  s.add(0)
end
# test repeatedly adding the same element to a tree-set
