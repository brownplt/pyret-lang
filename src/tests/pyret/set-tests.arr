#lang pyret

s = set.list-to-set

check:
  set.Set(s([1, 2])) is true
  set.Set(set.empty-set) is true
  s([1, 2, 3]).member(2) is true
  s([1, 2, 3]).member(4) is false
  s([]).add(1) is s([1])
  s([1]).add(1) is s([1])
  s([1, 2, 3]).add(2) is s([1, 2, 3])
  s([1, 2, 3]).add(1.5) is s([1, 2, 3, 1.5])
  s([1, 2]).remove(18) is s([1, 2])
  s([1, 2]).remove(2) is s([1])
  s([3, 1, 2]).to-list() is [1, 2, 3]
  s([1, 2]).union(s([2, 3])) is s([1, 2, 3])
  (s([1, 2.1, 3]) <> s([1, 2.2, 3])) is true
  s([1, 2, 4]) is s([2, 1, 4])
end
