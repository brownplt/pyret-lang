#lang pyret

fun check-random(n :: Number) -> Bool:
  nums = for map(elt from range(0,n)):
    random(n*n)
  end
  expect = for fold(s from [], elt from nums):
    if s.member(elt): s else: link(elt, s) end
  end.sort()
  (sets.list-set(nums).to-list() == expect)
    and (sets.tree-set(nums).to-list() == expect)
end

check:
  Set(set([1, 2])) is true
  Set(set([])) is true
  set([1, 2, 3]).member(2) is true
  set([1, 2, 3]).member(4) is false
  set([]).add(1) is set([1])
  set([1]).add(1) is set([1])
  set([1, 2, 3]).add(2) is set([1, 2, 3])
  set([1, 2, 3]).add(1.5) is set([1, 2, 3, 1.5])
  set([1, 2]).remove(18) is set([1, 2])
  set([1, 2]).remove(2) is set([1])
  set([3, 1, 2]).to-list() is [1, 2, 3]
  [2, 1, 3].to-set() is set([3, 2, 1])
  set([1, 2]).union(set([2, 3])) is set([1, 2, 3])
  (set([1, 2.1, 3]) <> set([1, 2.2, 3])) is true
  set([1, 2, 4]) is set([2, 1, 4])

  # oracle tests
  for each(n from range(1,21)):
    check-random(n*5) is true
  end
end
