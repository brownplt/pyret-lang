#lang pyret

fun check-random-adds(n :: Number, set-constructor) -> Bool:
  nums = for map(elt from range(0,n)):
    random(n*n)
  end
  expect = for fold(s from [], elt from nums):
    if s.member(elt): s else: link(elt, s) end
  end.sort()
  set-constructor(nums).to-list().sort() == expect
end

fun check-random-removes(n :: Number, set-constructor) -> Bool:
  nums = for map(elt from range(0,n)):
    random(2*n)
  end
  orig = range(0,n)
  expect = for filter(elt from orig):
    not nums.member(elt)
  end
  for fold(s from set-constructor(orig), rem-elt from nums):
    s.remove(rem-elt)
  end.to-list().sort() == expect
end

check:
  constructors = [set, list-set, tree-set]

  # run tests for each variant
  for each(s from constructors):
    Set(s([1, 2])) is true
    Set(s([])) is true
    s([1, 2, 3]).member(2) is true
    s([1, 2, 3]).member(4) is false
    s([]).add(1) is s([1])
    s([1]).add(1) is s([1])
    s([1, 2, 3]).add(2) is s([1, 2, 3])
    s([1, 2, 3]).add(1.5) is s([1, 2, 3, 1.5])
    s([1, 2]).remove(18) is s([1, 2])
    s([1, 2]).remove(2) is s([1])
    s([3, 1, 2]).to-list().sort() is [1, 2, 3]
    s([1, 2]).union(s([2, 3])) is s([1, 2, 3])
    s([1, 2]).union(s([4])) is s([1, 2, 4])
    s([1, 2]).intersect(s([2, 3])) is s([2])
    s([1, 2]).intersect(s([4])) is s([])
    s([1, 2]).difference(s([2, 3])) is s([1])
    s([1, 2]).difference(s([4])) is s([1, 2])
    s([1, 2]).symmetric_difference(s([1, 2])) is s([])
    s([1, 2]).symmetric_difference(s([2, 3])) is s([1, 3])
    s([1, 2]).symmetric_difference(s([3, 4])) is s([1, 2, 3, 4])
    (s([1, 2.1, 3]) <> s([1, 2.2, 3])) is true
    s([1, 2, 4]) is s([2, 1, 4])

    # oracle tests
    for each(n from range(1,21)):
      check-random-adds(n*5, s) is true
      check-random-removes(n*5, s) is true
    end
  end
end
