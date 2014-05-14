
check "member":
  set([list: 1, 2, 3]).member(2) is true
  set([list: 1, 2, 3]).member(4) is false
end

check "add":
  set([list: ]).add(1) is set([list: 1])
  set([list: 1]).add(1) is set([list: 1])
  set([list: 1, 2, 3]).add(2) is set([list: 1, 2, 3])
  set([list: 1, 2, 3]).add(1.5) is set([list: 1, 2, 3, 1.5])
end

check "remove":
  set([list: 1, 2]).remove(18) is set([list: 1, 2])
  set([list: 1, 2]).remove(2) is set([list: 1])
end

check "to-list":
  set([list: 3, 1, 2]).to-list() is [list: 1, 2, 3]
  set([list: "x", "f"]).to-list() is set([list: "f", "x"]).to-list()
  set([list: "x", "x"]).to-list() is set([list: "x"]).to-list()
end

fun check-random-adds(n :: Number, set-constructor) -> Bool:
  nums = for map(elt from range(0, n)):
    random(n * n)
  end
  expect = for fold(s from [list: ], elt from nums):
    if s.member(elt): s else: link(elt, s) end
  end.sort()
  set-constructor(nums).to-list().sort() == expect
end

fun check-random-removes(n :: Number, set-constructor) -> Bool:
  nums = for map(elt from range(0, n)):
    random(2 * n)
  end
  orig = range(0, n)
  expect = for filter(elt from orig):
    not(nums.member(elt))
  end
  for fold(s from set-constructor(orig), rem-elt from nums):
    s.remove(rem-elt)
  end.to-list().sort() == expect
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
    s([list: 1, 2, 3]).member(2) is true
    s([list: 1, 2, 3]).member(4) is false
    s([list: ]).add(1) is s([list: 1])
    s([list: 1]).add(1) is s([list: 1])
    s([list: 1, 2, 3]).add(2) is s([list: 1, 2, 3])
    s([list: 1, 2, 3]).add(1.5) is s([list: 1, 2, 3, 1.5])
    s([list: 1, 2]).remove(18) is s([list: 1, 2])
    s([list: 1, 2]).remove(2) is s([list: 1])
    s([list: 3, 1, 2]).to-list().sort() is [list: 1, 2, 3]
    s([list: 1, 2]).union(s([list: 2, 3])) is s([list: 1, 2, 3])
    s([list: 1, 2]).union(s([list: 4])) is s([list: 1, 2, 4])
    s([list: 1, 2]).intersect(s([list: 2, 3])) is s([list: 2])
    s([list: 1, 2]).intersect(s([list: 4])) is s([list: ])
    s([list: 1, 2]).difference(s([list: 2, 3])) is s([list: 1])
    s([list: 1, 2]).difference(s([list: 4])) is s([list: 1, 2])
    s([list: 1, 2]).symmetric_difference(s([list: 1, 2])) is s([list: ])
    c(s([list: 1, 2]).symmetric_difference(s([list: 2, 3]))) is c(s([list: 1, 3]))
    c(s([list: 1, 2]).symmetric_difference(s([list: 3, 4]))) is c(s([list: 1, 2, 3, 4]))
    (s([list: 1, 2.1, 3]) <> s([list: 1, 2.2, 3])) is true
    c(s([list: 1, 2, 4])) is c(s([list: 2, 1, 4]))

    for each(n from range(1,21)):
      check-random-adds(n * 5, s) is true
      check-random-removes(n * 5, s) is true
    end
  end

  test-constructor(set)
  test-constructor(list-set)
  test-constructor(tree-set)
end
