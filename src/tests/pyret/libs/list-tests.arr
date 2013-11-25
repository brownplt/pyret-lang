#lang pyret

check:
  checkers.check-equals("", [].join-str(", "), "")
  checkers.check-equals("", [1].join-str(", "), "1")
  checkers.check-equals("", [1,2,3].join-str(", "), "1, 2, 3")

  [1,2,3].set(1, 5) is [1,5,3]

  o1 = {
    _lessthan(self, other): self.x < other.x end,
    _equals(self, other): self.x == other.x end,
    x: 5
  }
  o2 = o1.{
    _lessthan(self, other): self.x < other.x end,
    _equals(self, other): self.x == other.x end,
    x: 10
  }
  [o2, o1].sort() is [o1, o2]

  ([] + 5) raises "expected List"

  list.fold_n(fun(i, sum, elt): sum + elt;, 0, 0, [1,2,3]) is 6
  list.fold_n(fun(i, _, _): i;, 5, 0, [1]) is 5

  list.split-at(-1, []) raises ""
  list.split-at(0, []) is { prefix: [], suffix: [] }
  list.split-at(1, []) raises ""
  list.split-at(0, [1]) is { prefix: [], suffix: [1] }
  list.split-at(1, [1]) is { prefix: [1], suffix: [] }
  list.split-at(2, [1,2,3]) is { prefix: [1,2], suffix: [3] }
  list.split-at(5, [1,2,3]) raises ""
end
