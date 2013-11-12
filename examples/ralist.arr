#lang pyret

# An implementation of Chris Okasaki's skew-binary random access lists.

provide {
  RandomAccessList: RandomAccessList,
  rempty: rempty,
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

data NodeTree:
  | nt-leaf(val :: Any)
  | nt-branch(val :: Any, left :: NodeTree, right :: NodeTree)
end

data RandomAccessList:
  | ra-empty
  | ra-link(size :: Number, tree :: NodeTree, next :: RandomAccessList)
sharing:
  first(self): rfirst(self) end,
  rest(self): rrest(self) end,
  length(self): rlength(self) end,
  each(self, f): reach(f, self) end,
  map(self, f): rmap(f, self) end,
  filter(self, f): rfilter(f, self) end,
  find(self, f): rfind(f, self) end,
  partition(self, f): rpartition(f, self) end,
  foldr(self, f): rfoldr(f, self) end,
  foldl(self, f): rfoldl(f, self) end,
  member(self, elt): rany(fun(e): e == elt end, self) end,
  append(self, other): rappend(self, other) end,
  last(self): rlast(self) end,
  take(self, n): rtake(self, n) end,
  drop(self, n): rdrop(self, n) end,
  reverse(self): rreverse(self) end,
  get(self, n): rget(self, n) end,
  set(self, n, e): rset(self, n, e) end,
  sort-by(self, cmp, eq): rsort(self, cmp, eq) end,
  sort(self): rsort(self, fun(l, r): l < r end, fun(l, r): l == r end) end,
  join-str(self, str): rjoinr(fun(l, r): l + str + r end, self) end,

  tostring(self): tostring(rlist-to-list(self)) end,
  _torepr(self): torepr(rlist-to-list(self)) end,
  _plus(self, other): rappend(self, other) end
end

# for testing convenience
rempty = ra-empty

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
  rlink("foo", rempty) is ra-link(1, nt-leaf("foo"), ra-empty)
  rlink("foo", rlink("bar", rempty))
    is ra-link(1, nt-leaf("foo"), ra-link(1, nt-leaf("bar"), ra-empty))
  rlink("foo", rlink("bar", rlink("baz", rempty)))
    is ra-link(3, nt-branch("foo", nt-leaf("bar"), nt-leaf("baz")), ra-empty)
end

fun rfirst(rlist :: RandomAccessList) -> Any:
  doc: "Gets the first element of the random access list"
  cases(RandomAccessList) rlist:
    | ra-empty => raise("first called on empty list")
    | ra-link(_, tree, _) => tree.val
  end
where:
  rfirst(rempty) raises "first"
  rfirst(rlink("foo", rempty)) is "foo"
  rfirst(rlink("foo", rlink("bar", rlink("baz", rempty)))) is "foo"
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
  rrest(rempty) raises "rest"
  rrest(rlink("foo", rempty)) is rempty
  rrest(rlink("foo", rlink("bar", rempty))) is rlink("bar", rempty)
  rrest(rlink("foo", rlink("bar", rlink("baz", rempty))))
    is rlink("bar", rlink("baz", rempty))
end

fun rget(rlist :: RandomAccessList, n :: Number) -> Any:
  doc: "Gets the element at index n in the random access list"
  fun nt-search(nt, size, ind):
    cases(NodeTree) nt:
      | nt-leaf(val) =>
        when ind <> 0:
          raise("nt-search called with invalid index")
        end
        val
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
  rget(rempty, 0) raises "get"
  rget(rlink("foo", rempty), 0) is "foo"
  rget(rlink("foo", rempty), 1) raises "get"
  rget(rlink("foo", rlink("bar", rlink("baz", rlink("qux", rempty)))), 2) is "baz"
end

fun rset(rlist :: RandomAccessList, n :: Number, new-val :: Any) -> RandomAccessList:
  doc: "Gets a new random access list with the element at index n changed to be new-val"
  fun nt-change(nt, size, ind, nv):
    cases(NodeTree) nt:
      | nt-leaf(val) =>
        when ind <> 0:
          raise("nt-search called with invalid index")
        end
        nt-leaf(nv)
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
  rset(rempty, 0, "foo") raises "set"
  rset(rlink("foo", rempty), 0, "foo2") is rlink("foo2", rempty)
  rset(rlink("foo", rempty), 1, "foo2") raises "set"
  rset(rlink("foo", rlink("bar", rlink("baz", rlink("qux", rempty)))), 2, "baz2")
    is rlink("foo", rlink("bar", rlink("baz2", rlink("qux", rempty))))
end

fun rlength(rlist :: RandomAccessList) -> Number:
  doc: "Get the length of a random access list"
  cases(RandomAccessList) rlist:
    | ra-empty => 0
    | ra-link(size, _, next) => size + rlength(next)
  end
where:
  rlength(rempty) is 0
  rlength(rlink("foo", rempty)) is 1
  rlength(rlink("foo", rlink("bar", rempty))) is 2
  rlength(rlink("foo", rlink("bar", rlink("baz", rlink("qux", rempty))))) is 4
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
  rmap-inc = fun(rl): rmap(fun(n): n + 1 end, rl) end
  
  rmap-inc(rempty) is rempty
  rmap-inc(rlink(1, rempty)) is rlink(2, rempty)
  rmap-inc(rlink(1, rlink(2, rempty))) is rlink(2, rlink(3, rempty))
  rmap-inc(rlink(1, rlink(2, rlink(3, rlink(4, rempty)))))
    is rlink(2, rlink(3, rlink(4, rlink(5, rempty))))
end

fun reach(f, rlist :: RandomAccessList):
  doc: "Calls f on each element in the list, and returns nothing"
  fun nt-each(nt):
    cases(NodeTree) nt:
      | nt-leaf(val) => f(val)
      | nt-branch(val, left, right) =>
        f(val)
        nt-each(left)
        nt-each(right)
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => nothing
    | ra-link(_, tree, next) =>
      nt-each(tree)
      reach(f, next)
  end
where:
  var x = 0
  reach-inc-x = fun(rl): reach(fun(dummy): x := x + 1 end, rl) end
  
  reach-inc-x(rempty)
  x is 0
  reach-inc-x(rlink(1, rempty))
  x is 1
  reach-inc-x(rlink(1, rlink(2, rempty)))
  x is 3
  reach-inc-x(rlink(1, rlink(2, rlink(3, rlink(4, rempty)))))
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
  rfold-sum = fun(rl): rfoldl(fun(n, m): n + m end, 0, rl) end
  
  rfold-sum(rempty) is 0
  rfold-sum(rlink(1, rempty)) is 1
  rfold-sum(rlink(1, rlink(2, rempty))) is 3
  rfold-sum(rlink(1, rlink(2, rlink(3, rlink(4, rempty))))) is 10
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
  rfold-sum = fun(rl): rfoldr(fun(n, m): n + m end, 0, rl) end
  
  rfold-sum(rempty) is 0
  rfold-sum(rlink(1, rempty)) is 1
  rfold-sum(rlink(1, rlink(2, rempty))) is 3
  rfold-sum(rlink(1, rlink(2, rlink(3, rlink(4, rempty))))) is 10
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
  rfilter-even = fun(rl): rfilter(fun(n): (n.modulo(2)) == 0 end, rl) end

  rfilter-even(rempty) is rempty
  rfilter-even(rlink(1, rempty)) is rempty
  rfilter-even(rlink(1, rlink(2, rempty))) is rlink(2, rempty)
  rfilter-even(rlink(1, rlink(2, rlink(3, rlink(4, rempty)))))
    is rlink(2, rlink(4, rempty))
end

fun rany(f, rlist :: RandomAccessList) -> Bool:
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
  rany-even = fun(rl): rany(fun(n): (n.modulo(2)) == 0 end, rl) end

  rany-even(rempty) is false
  rany-even(rlink(1, rempty)) is false
  rany-even(rlink(1, rlink(2, rempty))) is true
  rany-even(rlink(1, rlink(2, rlink(3, rlink(4, rempty))))) is true
end

fun rall(f, rlist :: RandomAccessList) -> Bool:
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
  rall-even = fun(rl): rall(fun(n): (n.modulo(2)) == 0 end, rl) end

  rall-even(rempty) is true
  rall-even(rlink(1, rempty)) is false
  rall-even(rlink(1, rlink(2, rempty))) is false
  rall-even(rlink(2, rlink(4, rlink(6, rlink(8, rempty))))) is true
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
  rfind-even = fun(rl): rfind(fun(n): (n.modulo(2)) == 0 end, rl) end

  rfind-even(rempty) is none
  rfind-even(rlink(1, rempty)) is none
  rfind-even(rlink(1, rlink(2, rempty))) is some(2)
  rfind-even(rlink(1, rlink(2, rlink(3, rlink(4, rempty))))) is some(2)
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
  rpartition-even = fun(rl): rpartition(fun(n): (n.modulo(2)) == 0 end, rl) end

  rpartition-even(rempty) is { is-true: rempty, is-false: rempty }
  rpartition-even(rlink(1, rempty)) is { is-true: rempty, is-false: rlink(1, rempty) }
  rpartition-even(rlink(1, rlink(2, rempty)))
    is { is-true: rlink(2, rempty), is-false: rlink(1, rempty) }
  rpartition-even(rlink(1, rlink(2, rlink(3, rlink(4, rempty)))))
    is { is-true: rlink(2, rlink(4, rempty)), is-false: rlink(1, rlink(3, rempty)) }
end

fun rappend(rlistl :: RandomAccessList, rlistr :: RandomAccessList) -> RandomAccessList:
  doc: "Append one list to the end of the other"
  cases(RandomAccessList) rlistl:
    | ra-empty => rlistr
    | ra-link(_, _, _) => rlink(rfirst(rlistl), rappend(rrest(rlistl), rlistr))
  end
where:
  rappend(rempty, rempty) is rempty
  rappend(rlink(1, rempty), rempty) is rlink(1, rempty)
  rappend(rempty, rlink(1, rempty)) is rlink(1, rempty)
  rappend(rlink(1, rempty), rlink(2, rempty)) is rlink(1, rlink(2, rempty))
  rappend(rlink(1, rlink(2, rlink(3, rempty))), rlink(4, rlink(5, rempty)))
    is rlink(1, rlink(2, rlink(3, rlink(4, rlink(5, rempty)))))
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
  rlast(rempty) raises "last"
  rlast(rlink(1, rempty)) is 1
  rlast(rlink(1, rlink(2, rempty))) is 2
  rlast(rlink(1, rlink(2, rlink(3, rlink(4, rempty))))) is 4
end

fun rreverse(rlist :: RandomAccessList) -> RandomAccessList:
  doc: "Reverse a random access list"
  rfoldl(fun(rl, v): rlink(v, rl) end, ra-empty, rlist)
where:
  rreverse(rempty) is rempty
  rreverse(rlink(1, rempty)) is rlink(1, rempty)
  rreverse(rlink(1, rlink(2, rempty))) is rlink(2, rlink(1, rempty))
  rreverse(rlink(1, rlink(2, rlink(3, rlink(4, rempty)))))
    is rlink(4, rlink(3, rlink(2, rlink(1, rempty))))
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
  rtake(rempty, 0) is rempty
  rtake(rempty, 1) raises "take"
  rtake(rlink(1, rempty), 0) is rempty
  rtake(rlink(1, rempty), 1) is rlink(1, rempty)
  rtake(rlink(1, rempty), 2) raises "take"
  rtake(rlink(1, rlink(2, rempty)), 1) is rlink(1, rempty)
  rtake(rlink(1, rlink(2, rlink(3, rlink(4, rempty)))), 2) is rlink(1, rlink(2, rempty))
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
  rdrop(rempty, 0) is rempty
  rdrop(rempty, 1) raises "drop"
  rdrop(rlink(1, rempty), 0) is rlink(1, rempty)
  rdrop(rlink(1, rempty), 1) is rempty
  rdrop(rlink(1, rempty), 2) raises "drop"
  rdrop(rlink(1, rlink(2, rempty)), 1) is rlink(2, rempty)
  rdrop(rlink(1, rlink(2, rlink(3, rlink(4, rempty)))), 2) is rlink(3, rlink(4, rempty))
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
  join-sum = fun(rl): rjoinr(fun(l, r): l + r end, rl) end

  join-sum(rempty) raises "join"
  join-sum(rlink(1, rempty)) is 1
  join-sum(rlink(1, rlink(2, rempty))) is 3
  join-sum(rlink(1, rlink(2, rlink(3, rlink(4, rempty))))) is 10
end

fun rsort(rlist :: RandomAccessList, cmp, eq) -> RandomAccessList:
  doc: "Sort a list using the cmp function for less-than comparison, and the eq function for equality"
  cases(RandomAccessList) rlist:
    | ra-empty => ra-empty
    | ra-link(_, _, _) =>
      pivot = rfirst(rlist)
      part-result = for rfoldl(acc from {l: rempty, e: rempty, g: rempty}, el from rlist):
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
  cmp = fun(l, r): l < r end
  eq = fun(l, r): l == r end
  sort = fun(rl): rsort(rl, cmp, eq) end

  sort(rempty) is rempty
  sort(rlink(3, rlink(1, rlink(2, rempty)))) is rlink(1, rlink(2, rlink(3, rempty)))
  sort(rlink(1, rlink(-7, rlink(-5, rlink(10, rlink(1, rlink(0, rlink(4, rempty))))))))
    is rlink(-7, rlink(-5, rlink(0, rlink(1, rlink(1, rlink(4, rlink(10, rempty)))))))
  
  wrapper = fun(n): { v:n,
                      _lessthan(self, other): self.v < other.v end,
                      _equals(self, other): self.v == other.v end } end
  wrap-list = rmap(wrapper, rlink(5, rlink(2, rlink(4, rlink(8, rempty)))))
  for rmap(el from sort(wrap-list)): el.v end
    is rlink(2, rlink(4, rlink(5, rlink(8, rempty))))
end

fun rlist-to-list(rlist :: RandomAccessList) -> List:
  doc: "Convert a random access list to a normal list"
  rfoldr(link, empty, rlist)
where:
  rlist-to-list(rempty) is []
  rlist-to-list(rlink(1, rempty)) is [1]
  rlist-to-list(rlink(1, rlink(2, rempty))) is [1, 2]
  rlist-to-list(rlink(1, rlink(2, rlink(3, rlink(4, rempty))))) is [1, 2, 3, 4]
end

fun list-to-rlist(lst :: List) -> RandomAccessList:
  doc: "Convert a list to a random access list"
  lst.foldr(rlink, ra-empty)
where:
  list-to-rlist([]) is rempty
  list-to-rlist([1]) is rlink(1, rempty)
  list-to-rlist([1, 2]) is rlink(1, rlink(2, rempty))
  list-to-rlist([1, 2, 3, 4]) is rlink(1, rlink(2, rlink(3, rlink(4, rempty))))
end

check:
  RandomAccessList(rempty) is true
  RandomAccessList(rlink(1, rempty)) is true
end
