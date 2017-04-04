#lang pyret

# An implementation of Chris Okasaki's skew-binary random access lists.

provide {
  rlist: rlist,
  rempty: ra-empty,
  rlink: rlink,
  is-rempty: is-ra-empty,
  is-rlink: is-ra-link,
  rfirst: rfirst,
  rrest: rrest,
  rindex: rget,
  rlength: rlength,
  rmap: rmap,
  reach: reach,
  rfold: rfoldl,
  rfilter: rfilter,
  rany: rany,
  rall: rall,
  rfind: rfind,
  rpartition: rpartition,
  rlist-to-list: rlist-to-list,
  list-to-rlist: list-to-rlist
} end
provide-types *

import valueskeleton as VS

data NodeTree:
  | nt-leaf(val :: Any)
  | nt-branch(val :: Any, left :: NodeTree, right :: NodeTree)
end

data RandomAccessList:
  | ra-empty with:
    method join-str(self, str): "" end
  | ra-link(size :: Number, tree :: NodeTree, next :: RandomAccessList) with:
    method join-str(self, str): rjoinr(lam(l, r): l + str + r end, self) end,
sharing:
  method first(self): rfirst(self) end,
  method rest(self): rrest(self) end,
  method length(self): rlength(self) end,
  method each(self, f): reach(f, self) end,
  method map(self, f): rmap(f, self) end,
  method filter(self, f): rfilter(f, self) end,
  method find(self, f): rfind(f, self) end,
  method partition(self, f): rpartition(f, self) end,
  method foldr(self, f): rfoldr(f, self) end,
  method foldl(self, f): rfoldl(f, self) end,
  method member(self, elt): rany(lam(e): e == elt end, self) end,
  method append(self, other): rappend(self, other) end,
  method last(self): rlast(self) end,
  method take(self, n): rtake(self, n) end,
  method drop(self, n): rdrop(self, n) end,
  method reverse(self): rreverse(self) end,
  method get(self, n): rget(self, n) end,
  method set(self, n, e): rset(self, n, e) end,
  method sort-by(self, cmp, eq): rsort(self, cmp, eq) end,
  method sort(self): rsort(self, lam(l, r): l < r end, lam(l, r): l == r end) end,

  method to-list(self): rlist-to-list(self) end,
  method _output(self): VS.vs-constr("rlist", rlist-to-list(self)) end,
  method _plus(self, other): rappend(self, other) end
end

fun rlink(val :: Any, rlist :: RandomAccessList) -> RandomAccessList:
  doc: "Constructs a random access list from a value and a list"
  fun simple-link(v, l):
    ra-link(1, nt-leaf(v), l)
  end
  cases(RandomAccessList) rlist:
    | ra-empty => simple-link(val, ra-empty)
    | ra-link(fsize, ftree, next) =>
      cases(RandomAccessList) next:
        | ra-empty => simple-link(val, rlist)
        | ra-link(ssize, stree, rest) =>
          if fsize == ssize:
            ra-link(1 + ssize + fsize,
                    nt-branch(val, ftree, stree),
                    rest)
          else:
            simple-link(val, rlist)
          end
      end
  end
where:
  rlink("foo", ra-empty) is ra-link(1, nt-leaf("foo"), ra-empty)
  rlink("foo", rlink("bar", ra-empty))
    is ra-link(1, nt-leaf("foo"), ra-link(1, nt-leaf("bar"), ra-empty))
  rlink("foo", rlink("bar", rlink("baz", ra-empty)))
    is ra-link(3, nt-branch("foo", nt-leaf("bar"), nt-leaf("baz")), ra-empty)
end

fun rfirst(rlist :: RandomAccessList) -> Any:
  doc: "Gets the first element of the random access list"
  cases(RandomAccessList) rlist:
    | ra-empty => raise("first called on empty list")
    | ra-link(_, tree, _) => tree.val
  end
where:
#  rfirst(ra-empty) raises "first"
  rfirst(rlink("foo", ra-empty)) is "foo"
  rfirst(rlink("foo", rlink("bar", rlink("baz", ra-empty)))) is "foo"
end

fun rrest(rlist :: RandomAccessList) -> RandomAccessList:
  doc: "Pops the first element off the random access list"
  cases(RandomAccessList) rlist:
    | ra-empty => raise("rest called on empty list")
    | ra-link(size, tree, next) =>
      cases(NodeTree) tree:
        | nt-leaf(_) => next
        | nt-branch(_, left, right) =>
          child-size = (size - 1) / 2
          ra-link(child-size, left, ra-link(child-size, right, next))
      end
  end
where:
#  rrest(ra-empty) raises "rest"
  rrest(rlink("foo", ra-empty)) is ra-empty
  rrest(rlink("foo", rlink("bar", ra-empty))) is rlink("bar", ra-empty)
  rrest(rlink("foo", rlink("bar", rlink("baz", ra-empty))))
    is rlink("bar", rlink("baz", ra-empty))
end

fun rget(rlist :: RandomAccessList, n :: Number) -> Any:
  doc: "Gets the element at index n in the random access list"
  fun nt-search(nt, size, ind):
    cases(NodeTree) nt:
      | nt-leaf(val) =>
        block:
          when ind <> 0:
            raise("nt-search called with invalid index")
          end
          val
        end
      | nt-branch(val, left, right) =>
        if ind == 0:
          val
        else:
          child-size = (size - 1) / 2
          right-ind = (size + 1) / 2
          if ind < right-ind:
            nt-search(left, child-size, ind - 1)
          else:
            nt-search(right, child-size, ind - right-ind)
          end
        end
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => raise("get called with out-of-bounds index")
    | ra-link(size, tree, next) =>
      if n < size:
        nt-search(tree, size, n)
      else:
        rget(next, n - size)
      end
  end
where:
#  rget(ra-empty, 0) raises "get"
  rget(rlink("foo", ra-empty), 0) is "foo"
#  rget(rlink("foo", ra-empty), 1) raises "get"
  rget(rlink("foo", rlink("bar", rlink("baz", rlink("qux", ra-empty)))), 2) is "baz"
end

fun rset(rlist :: RandomAccessList, n :: Number, new-val :: Any) -> RandomAccessList:
  doc: "Gets a new random access list with the element at index n changed to be new-val"
  fun nt-change(nt, size, ind, nv):
    cases(NodeTree) nt:
      | nt-leaf(val) =>
        block:
          when ind <> 0:
            raise("nt-search called with invalid index")
          end
          nt-leaf(nv)
        end
      | nt-branch(val, left, right) =>
        if ind == 0:
          nt-branch(nv, left, right)
        else:
          child-size = (size - 1) / 2
          right-ind = (size + 1) / 2
          if ind < right-ind:
            nt-branch(val, nt-change(left, child-size, ind - 1, nv), right)
          else:
            nt-branch(val, left, nt-change(right, child-size, ind - right-ind, nv))
          end
        end
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => raise("set called with out-of-bounds index")
    | ra-link(size, tree, next) =>
      if n < size:
        ra-link(size, nt-change(tree, size, n, new-val), next)
      else:
        ra-link(size, tree, rset(next, n - size, new-val))
      end
  end
where:
#  rset(ra-empty, 0, "foo") raises "set"
  rset(rlink("foo", ra-empty), 0, "foo2") is rlink("foo2", ra-empty)
#  rset(rlink("foo", ra-empty), 1, "foo2") raises "set"
  rset(rlink("foo", rlink("bar", rlink("baz", rlink("qux", ra-empty)))), 2, "baz2")
    is rlink("foo", rlink("bar", rlink("baz2", rlink("qux", ra-empty))))
end

fun rlength(rlist :: RandomAccessList) -> Number:
  doc: "Get the length of a random access list"
  cases(RandomAccessList) rlist:
    | ra-empty => 0
    | ra-link(size, _, next) => size + rlength(next)
  end
where:
  rlength(ra-empty) is 0
  rlength(rlink("foo", ra-empty)) is 1
  rlength(rlink("foo", rlink("bar", ra-empty))) is 2
  rlength(rlink("foo", rlink("bar", rlink("baz", rlink("qux", ra-empty))))) is 4
end

fun rmap(f, rlist :: RandomAccessList) -> RandomAccessList:
  doc: "Maps the function f over every element in the random access list"
  fun nt-map(nt):
    cases(NodeTree) nt:
      | nt-leaf(val) => nt-leaf(f(val))
      | nt-branch(val, left, right) => nt-branch(f(val), nt-map(left), nt-map(right))
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => ra-empty
    | ra-link(size, tree, next) => ra-link(size, nt-map(tree), rmap(f, next))
  end
where:
  rmap-inc = lam(rl): rmap(lam(n): n + 1 end, rl) end
  
  rmap-inc(ra-empty) is ra-empty
  rmap-inc(rlink(1, ra-empty)) is rlink(2, ra-empty)
  rmap-inc(rlink(1, rlink(2, ra-empty))) is rlink(2, rlink(3, ra-empty))
  rmap-inc(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty)))))
    is rlink(2, rlink(3, rlink(4, rlink(5, ra-empty))))
end

fun reach(f, rlist :: RandomAccessList):
  doc: "Calls f on each element in the list, and returns nothing"
  fun nt-each(nt):
    cases(NodeTree) nt:
      | nt-leaf(val) => f(val)
      | nt-branch(val, left, right) =>
        block:
          f(val)
          nt-each(left)
          nt-each(right)
        end
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => nothing
    | ra-link(_, tree, next) =>
      block:
        nt-each(tree)
        reach(f, next)
      end
  end
where:
  var x = 0
  reach-inc-x = lam(rl): reach(lam(dummy): x := x + 1 end, rl) end
  
  reach-inc-x(ra-empty)
  x is 0
  reach-inc-x(rlink(1, ra-empty))
  x is 1
  reach-inc-x(rlink(1, rlink(2, ra-empty)))
  x is 3
  reach-inc-x(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty)))))
  x is 7
end

fun rfoldl(f, base, rlist :: RandomAccessList):
  doc: "Accumulates all elements in the random access list using f"
  fun nt-fold(b, nt):
    cases(NodeTree) nt:
      | nt-leaf(val) => f(b, val)
      | nt-branch(val, left, right) => nt-fold(nt-fold(f(b, val), left), right)
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => base
    | ra-link(_, tree, next) => rfoldl(f, nt-fold(base, tree), next)
  end
where:
  rfold-sum = lam(rl): rfoldl(lam(n, m): n + m end, 0, rl) end
  
  rfold-sum(ra-empty) is 0
  rfold-sum(rlink(1, ra-empty)) is 1
  rfold-sum(rlink(1, rlink(2, ra-empty))) is 3
  rfold-sum(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty))))) is 10
end

fun rfoldr(f, base, rlist :: RandomAccessList):
  doc: "Accumulates all elements in the random acces list using f, in list order"
  fun nt-fold(r, nt):
    cases(NodeTree) nt:
      | nt-leaf(val) => f(val, r)
      | nt-branch(val, left, right) => f(val, nt-fold(nt-fold(r, right), left))
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => base
    | ra-link(_, tree, next) => nt-fold(rfoldr(f, base, next), tree)
  end
where:
  rfold-sum = lam(rl): rfoldr(lam(n, m): n + m end, 0, rl) end
  
  rfold-sum(ra-empty) is 0
  rfold-sum(rlink(1, ra-empty)) is 1
  rfold-sum(rlink(1, rlink(2, ra-empty))) is 3
  rfold-sum(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty))))) is 10
end

fun rfilter(f, rlist :: RandomAccessList) -> RandomAccessList:
  doc: "Returns a random access list containing only elements for which the given predicate is true"
  cases(RandomAccessList) rlist:
    | ra-empty => ra-empty
    | ra-link(size, tree, next) =>
      next-list = cases(NodeTree) tree:
        | nt-leaf(val) => next
        | nt-branch(val, left, right) =>
          child-size = (size - 1) / 2
          ra-link(child-size, left, ra-link(child-size, right, next))
      end
      if f(tree.val):
        rlink(tree.val, rfilter(f, next-list))
      else:
        rfilter(f, next-list)
      end
  end
where:
  rfilter-even = lam(rl): rfilter(lam(n): (num-modulo(n, 2)) == 0 end, rl) end

  rfilter-even(ra-empty) is ra-empty
  rfilter-even(rlink(1, ra-empty)) is ra-empty
  rfilter-even(rlink(1, rlink(2, ra-empty))) is rlink(2, ra-empty)
  rfilter-even(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty)))))
    is rlink(2, rlink(4, ra-empty))
end

fun rany(f, rlist :: RandomAccessList) -> Boolean:
  doc: "Returns true if the predicate f is true for any element in the list"
  fun nt-any(nt):
    cases(NodeTree) nt:
      | nt-leaf(val) => f(val)
      | nt-branch(val, left, right) => f(val) or nt-any(left) or nt-any(right)
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => false
    | ra-link(_, tree, next) => nt-any(tree) or rany(f, next)
  end
where:
  rany-even = lam(rl): rany(lam(n): (num-modulo(n, 2)) == 0 end, rl) end

  rany-even(ra-empty) is false
  rany-even(rlink(1, ra-empty)) is false
  rany-even(rlink(1, rlink(2, ra-empty))) is true
  rany-even(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty))))) is true
end

fun rall(f, rlist :: RandomAccessList) -> Boolean:
  doc: "Returns true if the predicate f is true for all elements in the list"
  fun nt-all(nt):
    cases(NodeTree) nt:
      | nt-leaf(val) => f(val)
      | nt-branch(val, left, right) => f(val) and nt-all(left) and nt-all(right)
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => true
    | ra-link(_, tree, next) => nt-all(tree) and rall(f, next)
  end
where:
  rall-even = lam(rl): rall(lam(n): (num-modulo(n, 2)) == 0 end, rl) end

  rall-even(ra-empty) is true
  rall-even(rlink(1, ra-empty)) is false
  rall-even(rlink(1, rlink(2, ra-empty))) is false
  rall-even(rlink(2, rlink(4, rlink(6, rlink(8, ra-empty))))) is true
end

fun rfind(f, rlist :: RandomAccessList) -> Option:
  doc: "Finds the first element of the list satisfying the predicate"
  fun nt-find(nt):
    if f(nt.val):
      some(nt.val)
    else:
      cases(NodeTree) nt:
        | nt-leaf(val) => none
        | nt-branch(val, left, right) =>
          cases(Option) nt-find(left):
            | none => nt-find(right)
            | some(v) => some(v)
          end
      end
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => none
    | ra-link(_, tree, next) =>
      cases(Option) nt-find(tree):
        | none => rfind(f, next)
        | some(val) => some(val)
      end
  end
where:
  rfind-even = lam(rl): rfind(lam(n): (num-modulo(n, 2)) == 0 end, rl) end

  rfind-even(ra-empty) is none
  rfind-even(rlink(1, ra-empty)) is none
  rfind-even(rlink(1, rlink(2, ra-empty))) is some(2)
  rfind-even(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty))))) is some(2)
end

fun rpartition(f, rlist :: RandomAccessList):
  doc: "Splits the list into two lists depending on the result of the predicate on each element"
  cases(RandomAccessList) rlist:
    | ra-empty => { is-true: ra-empty, is-false: ra-empty }
    | ra-link(size, tree, next) =>
      next-list = cases(NodeTree) tree:
        | nt-leaf(val) => next
        | nt-branch(val, left, right) =>
          child-size = (size - 1) / 2
          ra-link(child-size, left, ra-link(child-size, right, next))
      end
      part-result = rpartition(f, next-list)
      if f(tree.val):
        { is-true: rlink(tree.val, part-result.is-true), is-false: part-result.is-false }
      else:
        { is-true: part-result.is-true, is-false: rlink(tree.val, part-result.is-false) }
      end
  end
where:
  rpartition-even = lam(rl): rpartition(lam(n): (num-modulo(n, 2)) == 0 end, rl) end

  rpartition-even(ra-empty) is { is-true: ra-empty, is-false: ra-empty }
  rpartition-even(rlink(1, ra-empty)) is { is-true: ra-empty, is-false: rlink(1, ra-empty) }
  rpartition-even(rlink(1, rlink(2, ra-empty)))
    is { is-true: rlink(2, ra-empty), is-false: rlink(1, ra-empty) }
  rpartition-even(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty)))))
    is { is-true: rlink(2, rlink(4, ra-empty)), is-false: rlink(1, rlink(3, ra-empty)) }
end

fun rappend(rlistl :: RandomAccessList, rlistr :: RandomAccessList) -> RandomAccessList:
  doc: "Append one list to the end of the other"
  cases(RandomAccessList) rlistl:
    | ra-empty => rlistr
    | ra-link(_, _, _) => rlink(rfirst(rlistl), rappend(rrest(rlistl), rlistr))
  end
where:
  rappend(ra-empty, ra-empty) is ra-empty
  rappend(rlink(1, ra-empty), ra-empty) is rlink(1, ra-empty)
  rappend(ra-empty, rlink(1, ra-empty)) is rlink(1, ra-empty)
  rappend(rlink(1, ra-empty), rlink(2, ra-empty)) is rlink(1, rlink(2, ra-empty))
  rappend(rlink(1, rlink(2, rlink(3, ra-empty))), rlink(4, rlink(5, ra-empty)))
    is rlink(1, rlink(2, rlink(3, rlink(4, rlink(5, ra-empty)))))
end

fun rlast(rlist :: RandomAccessList):
  doc: "Get last element of a random access list"
  fun nt-last(nt):
    cases(NodeTree) nt:
      | nt-leaf(val) => val
      | nt-branch(_, _, right) => nt-last(right)
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => raise("last called on empty list")
    | ra-link(_, tree, next) =>
      if is-ra-empty(next): nt-last(tree) else: rlast(next) end
  end
where:
#  rlast(ra-empty) raises "last"
  rlast(rlink(1, ra-empty)) is 1
  rlast(rlink(1, rlink(2, ra-empty))) is 2
  rlast(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty))))) is 4
end

fun rreverse(rlist :: RandomAccessList) -> RandomAccessList:
  doc: "Reverse a random access list"
  rfoldl(lam(rl, v): rlink(v, rl) end, ra-empty, rlist)
where:
  rreverse(ra-empty) is ra-empty
  rreverse(rlink(1, ra-empty)) is rlink(1, ra-empty)
  rreverse(rlink(1, rlink(2, ra-empty))) is rlink(2, rlink(1, ra-empty))
  rreverse(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty)))))
    is rlink(4, rlink(3, rlink(2, rlink(1, ra-empty))))
end

fun rtake(rlist :: RandomAccessList, n) -> RandomAccessList:
  doc: "Takes the first n elements of the list"
  if n == 0:
    ra-empty
  else:
    cases(RandomAccessList) rlist:
      | ra-empty => raise("take hit end of list")
      | ra-link(_, _, _) => rlink(rfirst(rlist), rtake(rrest(rlist), n - 1))
    end
  end
where:
  rtake(ra-empty, 0) is ra-empty
#  rtake(ra-empty, 1) raises "take"
  rtake(rlink(1, ra-empty), 0) is ra-empty
  rtake(rlink(1, ra-empty), 1) is rlink(1, ra-empty)
#  rtake(rlink(1, ra-empty), 2) raises "take"
  rtake(rlink(1, rlink(2, ra-empty)), 1) is rlink(1, ra-empty)
  rtake(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty)))), 2) is rlink(1, rlink(2, ra-empty))
end

fun rdrop(rlist :: RandomAccessList, n) -> RandomAccessList:
  doc: "Drops the first n elements from the list"
  if n == 0:
    rlist
  else:
    cases(RandomAccessList) rlist:
      | ra-empty => raise("drop hit end of list")
      | ra-link(_, _, _) => rdrop(rrest(rlist), n - 1)
    end
  end
where:
  rdrop(ra-empty, 0) is ra-empty
#  rdrop(ra-empty, 1) raises "drop"
  rdrop(rlink(1, ra-empty), 0) is rlink(1, ra-empty)
  rdrop(rlink(1, ra-empty), 1) is ra-empty
#  rdrop(rlink(1, ra-empty), 2) raises "drop"
  rdrop(rlink(1, rlink(2, ra-empty)), 1) is rlink(2, ra-empty)
  rdrop(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty)))), 2) is rlink(3, rlink(4, ra-empty))
end

fun rjoinl(joiner, rlist :: RandomAccessList):
  doc: "Joins elements of the list together using the specified joiner function"
  cases(RandomAccessList) rlist:
    | ra-empty => raise("join called on empty list")
    | ra-link(_, _, _) =>
      rfoldl(joiner, rfirst(rlist), rrest(rlist))
  end
where:
  join-sum = lam(rl): rjoinl(lam(l, r): l + r end, rl) end

#  join-sum(ra-empty) raises "join"
  join-sum(rlink(1, ra-empty)) is 1
  join-sum(rlink(1, rlink(2, ra-empty))) is 3
  join-sum(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty))))) is 10
end

fun rjoinr(joiner, rlist :: RandomAccessList):
  doc: "Joins elements of the list together using the specified joiner function, in list order"
  cases(RandomAccessList) rlist:
    | ra-empty => raise("join called on empty list")
    | ra-link(_, _, _) =>
      fst = rfirst(rlist)
      rst = rrest(rlist)
      if is-ra-empty(rst): fst else: joiner(fst, rjoinr(joiner, rst)) end
  end
where:
  join-sum = lam(rl): rjoinr(lam(l, r): l + r end, rl) end

#  join-sum(ra-empty) raises "join"
  join-sum(rlink(1, ra-empty)) is 1
  join-sum(rlink(1, rlink(2, ra-empty))) is 3
  join-sum(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty))))) is 10
end

fun rsort(rlist :: RandomAccessList, cmp, eq) -> RandomAccessList:
  doc: "Sort a list using the cmp function for less-than comparison, and the eq function for equality"
  cases(RandomAccessList) rlist:
    | ra-empty => ra-empty
    | ra-link(_, _, _) =>
      pivot = rfirst(rlist)
      part-result = for rfoldl(acc from {l: ra-empty, e: ra-empty, g: ra-empty}, el from rlist):
        if cmp(el, pivot):
          acc.{l: rlink(el, acc.l)}
        else if eq(el, pivot):
          acc.{e: rlink(el, acc.e)}
        else:
          acc.{g: rlink(el, acc.g)}
        end
      end
      rsort(part-result.l, cmp, eq) + part-result.e + rsort(part-result.g, cmp, eq)
  end
where:
  cmp = lam(l, r): l < r end
  eq = lam(l, r): l == r end
  sort = lam(rl): rsort(rl, cmp, eq) end

  sort(ra-empty) is ra-empty
  sort(rlink(3, rlink(1, rlink(2, ra-empty)))) is rlink(1, rlink(2, rlink(3, ra-empty)))
  sort(rlink(1, rlink(-7, rlink(-5, rlink(10, rlink(1, rlink(0, rlink(4, ra-empty))))))))
    is rlink(-7, rlink(-5, rlink(0, rlink(1, rlink(1, rlink(4, rlink(10, ra-empty)))))))
  
  wrapper = lam(n): { v:n,
                      method _lessthan(self, other): self.v < other.v end,
                      method _equals(self, other, shadow eq): eq(self.v, other.v) end } end
  wrap-list = rmap(wrapper, rlink(5, rlink(2, rlink(4, rlink(8, ra-empty)))))
  for rmap(el from sort(wrap-list)): el.v end
    is rlink(2, rlink(4, rlink(5, rlink(8, ra-empty))))
end

fun rlist-to-list(rlist :: RandomAccessList) -> List:
  doc: "Convert a random access list to a normal list"
  rfoldr(link, empty, rlist)
where:
  rlist-to-list(ra-empty) is [list: ]
  rlist-to-list(rlink(1, ra-empty)) is [list: 1]
  rlist-to-list(rlink(1, rlink(2, ra-empty))) is [list: 1, 2]
  rlist-to-list(rlink(1, rlink(2, rlink(3, rlink(4, ra-empty))))) is [list: 1, 2, 3, 4]
end

fun list-to-rlist(lst :: List) -> RandomAccessList:
  doc: "Convert a list to a random access list"
  lst.foldr(rlink, ra-empty)
where:
  list-to-rlist([list: ]) is ra-empty
  list-to-rlist([list: 1]) is rlink(1, ra-empty)
  list-to-rlist([list: 1, 2]) is rlink(1, rlink(2, ra-empty))
  list-to-rlist([list: 1, 2, 3, 4]) is rlink(1, rlink(2, rlink(3, rlink(4, ra-empty))))
end

rlist = {
  make: lam(args): list-to-rlist(raw-array-to-list(args)) end,
  make0: lam(): ra-empty end,
  make1: lam(a): rlink(a, ra-empty) end,
  make2: lam(a, b): rlink(a, rlink(b, ra-empty)) end,
  make3: lam(a, b, c): rlink(a, rlink(b, rlink(c, ra-empty))) end,
  make4: lam(a, b, c, d): rlink(a, rlink(b, rlink(c, rlink(d, ra-empty)))) end,
  make5: lam(a, b, c, d, e): rlink(a, rlink(b, rlink(c, rlink(d, rlink(e, ra-empty))))) end,
}

check:
  is-RandomAccessList(ra-empty) is true
  is-RandomAccessList(rlink(1, ra-empty)) is true

  ra-empty.join-str(", ") is ""
  rlink("foo", ra-empty).join-str(", ") is "foo"
  rlink("foo", rlink("bar", ra-empty)).join-str(", ") is "foo, bar"
end
