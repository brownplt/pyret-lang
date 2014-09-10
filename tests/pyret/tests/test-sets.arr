import pick as P

check "member":
  [tree-set: 1, 2, 3].member(2) is true
  [tree-set: 1, 2, 3].member(4) is false
end

check "add":
  [tree-set: ].add(1) is [tree-set: 1]
  [tree-set: 1].add(1) is [tree-set: 1]
  [tree-set: 1, 2, 3].add(2) is [tree-set: 1, 2, 3]
  [tree-set: 1, 2, 3].add(1.5) is [tree-set: 1, 2, 3, 1.5]
end

check "remove":
  [tree-set: 1, 2].remove(18) is [tree-set: 1, 2]
  [tree-set: 1, 2].remove(2) is [tree-set: 1]
end

check "to-list":
  [tree-set: 3, 1, 2].to-list() is [list: 1, 2, 3]
  [tree-set: "x", "f"].to-list() is [tree-set: "f", "x"].to-list()
  [tree-set: "x", "x"].to-list() is [tree-set: "x"].to-list()
end

fun raw-build-array(n, make-elt):
  arr = raw-array-of(0, n)
  for raw-array-fold(_ from 0, _ from arr, ix from 0):
    raw-array-set(arr, ix, make-elt(n))
  end
end

fun make-arraynge(n):
  arr = raw-array-of(0, n)
  for raw-array-fold(_ from 0, _ from arr, ix from 0):
    raw-array-set(arr, ix, ix)
  end
end

fun check-random-adds(n :: Number, set-constructor) -> Boolean:
  nums = raw-build-array(n, lam(n2): random(n2 * n2) end)
  expect = for raw-array-fold(s from [list: ], elt from nums, _ from 0):
    if s.member(elt): s else: link(elt, s) end
  end.sort()
  set-constructor.make(nums).to-list().sort() == expect
end

fun check-random-removes(n :: Number, set-constructor) -> Boolean:
  nums = raw-build-array(n, lam(n2): random(2 * n2) end)
  orig = make-arraynge(n)
  nums-list = raw-array-to-list(nums)
  expect = for raw-array-fold(lst from empty, elt from orig, _ from 0):
    if not(nums-list.member(elt)):
      link(elt, lst)
    else:
      lst
    end
  end.reverse()
  result = for fold(s from set-constructor.make(orig), rem-elt from nums-list):
    s.remove(rem-elt)
  end.to-list().sort()
  result == expect
end

check:
  fun canonicalize(s):
    s.to-list().sort()
  end
  c = canonicalize
  fun test-constructor(s):
# SKIP(wating for predicates/annotations)
#    Set(s([list: 1, 2])) is true
#    Set(s([list: ])) is true
    [s: 1, 2, 3].member(2) is true
    [s: 1, 2, 3].member(4) is false
    [s: ].add(1) is [s: 1]
    [s: 1].add(1) is [s: 1]
    [s: 1, 2, 3].add(2) is [s: 1, 2, 3]
    [s: 1, 2, 3].add(1.5) is [s: 1, 2, 3, 1.5]
    [s: 1, 2].remove(18) is [s: 1, 2]
    [s: 1, 2].remove(2) is [s: 1]
    [s: 3, 1, 2].to-list().sort() is [list: 1, 2, 3]
    [s: 1, 2].union([s: 2, 3]) is [s: 1, 2, 3]
    [s: 1, 2].union([s: 4]) is [s: 1, 2, 4]
    [s: 1, 2].intersect([s: 2, 3]) is [s: 2]
    [s: 1, 2].intersect([s: 4]) is [s: ]
    [s: 1, 2].difference([s: 2, 3]) is [s: 1]
    [s: 1, 2].difference([s: 4]) is [s: 1, 2]
    [s: 1, 2].symmetric_difference([s: 1, 2]) is [s: ]
    c([s: 1, 2].symmetric_difference([s: 2, 3])) is c([s: 1, 3])
    c([s: 1, 2].symmetric_difference([s: 3, 4])) is c([s: 1, 2, 3, 4])
    ([s: 1, 2.1, 3] <> [s: 1, 2.2, 3]) is true
    c([s: 1, 2, 4]) is c([s: 2, 1, 4])

    for each(n from range(1,21)):
      check-random-adds(n * 5, s) is true
      check-random-removes(n * 5, s) is true
    end
  end

#  test-constructor(set)
  test-constructor(list-set)
  test-constructor(tree-set)
end

check "Different constructors should work well together":
  fun canonicalize(s):
    s.to-list().sort()
  end
  c = canonicalize
  fun test-constructor(s-a, s-b):
    [s-a: 1, 2].union([s-b: 2, 3]) is [s-a: 1, 2, 3]
    [s-a: 1, 2].union([s-b: 4]) is [s-a: 1, 2, 4]
    [s-a: 1, 2].intersect([s-b: 2, 3]) is [s-a: 2]
    [s-a: 1, 2].intersect([s-b: 4]) is [s-a: ]
    [s-a: 1, 2].difference([s-b: 2, 3]) is [s-a: 1]
    [s-a: 1, 2].difference([s-b: 4]) is [s-a: 1, 2]
    [s-a: 1, 2].symmetric_difference([s-b: 1, 2]) is [s-a: ]
    c([s-a: 1, 2].symmetric_difference([s-b: 2, 3])) is c([s-a: 1, 3])
    c([s-a: 1, 2].symmetric_difference([s-b: 3, 4])) is c([s-a: 1, 2, 3, 4])
    ([s-a: 1, 2.1, 3] <> [s-b: 1, 2.2, 3]) is true
    c([s-a: 1, 2, 4]) is c([s-b: 2, 1, 4])
  end

  test-constructor(list-set, tree-set)
  test-constructor(tree-set, list-set)
end

check "pick on list sets doesn't repeat order":
  s = [list-set: 1, 2, 3]
  var found-diff = false
  # This will fail every 2^100 times it is run, given that JS has a decent RNG
  # and given the current sets implementation
  for each(i from range(0, 100)):
    when not(s.pick().elt == s.pick().elt):
      found-diff := true
    end
  end
  found-diff is true
end

check "sets pick visits all elemeents":

  fun pick-sum(s):
    cases(P.Pick) s.pick():
      | pick-none => 0
      | pick-some(elt, rest) => elt + pick-sum(rest)
    end
  end

  pick-sum([list-set: 1, 2, 3, 4]) is 10
  pick-sum([tree-set: 1, 2, 3, 4]) is 10
  pick-sum([list-set:]) is 0
  pick-sum([tree-set:]) is 0

end
