### Looks shipshape

import pick as P
include from P: data Pick end
include lists
import sets as S 
include from S:
  * hiding (fold, all, any),
  data Set
end
include raw-array
import global as G 
import number as N
include from N: random end
include from G: not, raise, to-repr end
import equality as E

type SetMaker = { make :: (RawArray<Number> -> Set<Number>) }

check "regression for small constructors":
  [set: 1, 2, 1].member(2) is true

  [set: 1, 2, 3, 2].member(3) is true
  [set: 1, 2, 3, 3].member(3) is true
  [set: 1, 2, 3, 2].member(2) is true
  [set: 1, 2, 3, 4].member(3) is true
  [set: 1, 2, 3, 2].member(5) is false

  [set: 1, 2, 1, 3].member(2) is true
  [set: 1, 2, 1, 3, 4].member(2) is true
end

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

fun make-arraynge(n :: Number) -> RawArray<Number>:
  arr = raw-array-of(0, n)
  for raw-array-fold(_ from 0, _ from arr, ix from 0):
    raw-array-set(arr, ix, ix)
  end
end

fun check-random-adds(n :: Number, set-constructor :: SetMaker) -> Boolean:
  nums = raw-array-build(lam(n2): random(n2 * n2) end, n)
  expect = for raw-array-fold(s from [list: ], elt from nums, _ from 0):
    if s.member(elt): s else: link(elt, s) end
  end.sort()
  set-constructor.make(nums).to-list().sort() == expect
end

fun check-random-removes(n :: Number, set-constructor :: SetMaker) -> Boolean:
  nums = raw-array-build(lam(n2): random(2 * n2) end, n)
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
  fun canonicalize(s :: Set<Number>) -> List<Number>:
    s.to-list().sort()
  end
  c = canonicalize
  fun test-constructor(s :: SetMaker) -> Nothing block:
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
    [s: 1, 2].symmetric-difference([s: 1, 2]) is [s: ]
    c([s: 1, 2].symmetric-difference([s: 2, 3])) is c([s: 1, 3])
    c([s: 1, 2].symmetric-difference([s: 3, 4])) is c([s: 1, 2, 3, 4])
    ([s: 1, 2.1, 3] <> [s: 1, 2.2, 3]) is true
    c([s: 1, 2, 4]) is c([s: 2, 1, 4])

    for each(n from range(1,10)) block:
      check-random-adds(n * 5, s) is true
      check-random-removes(n * 5, s) is true
      nothing
    end
  end

#  test-constructor(set)
  test-constructor(list-set)
  test-constructor(tree-set)
end

check "Different constructors should work well together":
  fun canonicalize(s :: Set<Number>) -> List<Number>:
    s.to-list().sort()
  end
  c = canonicalize
  fun test-constructor(s-a :: SetMaker, s-b :: SetMaker) block:
    [s-a: 1, 2].union([s-b: 2, 3]) is [s-a: 1, 2, 3]
    [s-a: 1, 2].union([s-b: 4]) is [s-a: 1, 2, 4]
    [s-a: 1, 2].intersect([s-b: 2, 3]) is [s-a: 2]
    [s-a: 1, 2].intersect([s-b: 4]) is [s-a: ]
    [s-a: 1, 2].difference([s-b: 2, 3]) is [s-a: 1]
    [s-a: 1, 2].difference([s-b: 4]) is [s-a: 1, 2]
    [s-a: 1, 2].symmetric-difference([s-b: 1, 2]) is [s-a: ]
    c([s-a: 1, 2].symmetric-difference([s-b: 2, 3])) is c([s-a: 1, 3])
    c([s-a: 1, 2].symmetric-difference([s-b: 3, 4])) is c([s-a: 1, 2, 3, 4])
    ([s-a: 1, 2.1, 3] <> [s-b: 1, 2.2, 3]) is true
    c([s-a: 1, 2, 4]) is c([s-b: 2, 1, 4])
    [s-a: 1, 2, 4].size() is 3
    [s-a: 1, 2, 4].size() is 3
  end

  test-constructor(list-set, tree-set)
  test-constructor(tree-set, list-set)
end

fun pick-value<A, B>(p :: Pick<A, B>) -> A:
  cases(Pick) p:
    | pick-none => raise("pick-value on pick-none")
    | pick-some(v, _) => v
  end
end

check "pick on list sets doesn't repeat order":
  s = [list-set: 1, 2, 3]
  var found-diff = false
  # This will fail every 2^100 times it is run, given that JS has a decent RNG
  # and given the current sets implementation
  for each(i from range(0, 100)):
    when not(pick-value(s.pick()) == pick-value(s.pick())):
      found-diff := true
    end
  end
  found-diff is true
end

check "sets pick visits all elemeents":

  fun pick-sum(s :: Set<Number>) -> Number:
    cases(P.Pick) s.pick():
      | pick-none => 0
      | pick-some(elt :: Number, rest) => elt + pick-sum(rest)
    end
  end

  pick-sum([list-set: 1, 2, 3, 4]) is 10
  pick-sum([tree-set: 1, 2, 3, 4]) is 10
  pick-sum([list-set:]) is 0
  pick-sum([tree-set:]) is 0

end

check "to-repr":
  to-repr([set: 1, 2, 3]) is "[list-set: 3, 2, 1]"
  to-repr([tree-set: 1, 2]) is "[tree-set: 1, 2]"
  to-repr([tree-set: 1, 2, 2]) is "[tree-set: 1, 2]"
  to-repr([list-set: {1;2}]) is "[list-set: {1; 2}]"
end

check "equality":
  l1 = [list-set: [list: "a"], [list: "b"]]
  l2 = [list-set: [list: "b"], [list: "a"]]
  l1 is l2

  rec-equal = lam(a :: List<String>, b :: List<String>): E.equal-always3(a, b) end
  l1._equals(l2, rec-equal) is E.Equal
end