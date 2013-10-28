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

  o = {m: [1,2,3]:to-set}
  o.m() raises "expected List"

  fold_n(fun(i, sum, elt): sum + elt;, 0, [1,2,3]) is 6
end
