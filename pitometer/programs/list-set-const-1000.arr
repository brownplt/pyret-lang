import sets as S
z = for fold(s from [S.list-set:], _ from range(0, 1000)):
  s.add(0)
end
# test repeatedly adding same element to a list-set
