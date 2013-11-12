#lang pyret

provide {
  RandomAccessList: RandomAccessList,
  rempty: rempty,
  rlink: rlink,
  is-rempty: is-ra-empty,
  is-rlink: is-ra-link,
  rfirst: rfirst,
  rrest: rrest,
  rget: rget,
  rset: rset,
  rlength: rlength,
  rmap: rmap,
  reach: reach,
  rfold: rfold,
  rfilter: rfilter,
  rany: rany,
  rall: rall,
  rfind: rfind,
  rpartition: rpartition,
  rlist-to-list: rlist-to-list,
  list-to-rlist: list-to-rlist
} end

data NodeTree:
  | nt-leaf
  | nt-branch(val :: Any, left :: NodeTree, right :: NodeTree)
end

data RandomAccessList:
  | ra-empty
  | ra-link(size :: Number, tree :: NodeTree, next :: RandomAccessList)
end

# for testing convenience
rempty = ra-empty

fun rlink(val :: Any, rlist :: RandomAccessList) -> RandomAccessList:
  doc: "Constructs a random access list from a value and a list"
  fun simple-link(v, l):
    ra-link(1, nt-branch(v, nt-leaf, nt-leaf), l)
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
  rlink("foo", rempty) is ra-link(1, nt-branch("foo", nt-leaf, nt-leaf), ra-empty)
  rlink("foo", rlink("bar", rempty))
    is ra-link(1, nt-branch("foo", nt-leaf, nt-leaf),
               ra-link(1, nt-branch("bar", nt-leaf, nt-leaf), ra-empty))
  rlink("foo", rlink("bar", rlink("baz", rempty)))
    is ra-link(3,
               nt-branch("foo", nt-branch("bar", nt-leaf, nt-leaf),
                                nt-branch("baz", nt-leaf, nt-leaf)),
               ra-empty)
end

fun rfirst(rlist :: RandomAccessList) -> Any:
  doc: "Gets the first element of the random access list"
  cases(RandomAccessList) rlist:
    | ra-empty => raise("first called on empty list")
    | ra-link(size, tree, _) =>
      cases(NodeTree) tree:
        | nt-leaf => raise("Empty node tree on list!")
        | nt-branch(val, _, _) => val
      end
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
        | nt-leaf => raise("Empty node tree on list!")
        | nt-branch(val, left, right) =>
          if size == 1:
            next
          else:
            child-size = (size - 1) / 2
            ra-link(child-size, left, ra-link(child-size, right, next))
          end
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
      | nt-leaf => raise("Shouldn't be reaching leaf nodes!")
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
      | nt-leaf => raise("Shouldn't be reaching leaf nodes!")
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
      | nt-leaf => nt-leaf
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
      | nt-leaf => nothing
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

fun rfold(f, base, rlist :: RandomAccessList):
  doc: "Accumulates all elements in the random access list using f"
  fun nt-fold(b, nt):
    cases(NodeTree) nt:
      | nt-leaf => b
      | nt-branch(val, left, right) => nt-fold(nt-fold(f(val, b), left), right)
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => base
    | ra-link(_, tree, next) => rfold(f, nt-fold(base, tree), next)
  end
where:
  rfold-sum = fun(rl): rfold(fun(n, m): n + m end, 0, rl) end
  
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
      cases(NodeTree) tree:
        | nt-leaf => raise("Empty tree encountered in filter!")
        | nt-branch(val, left, right) =>
          next-list = if size == 1:
            next
          else:
            child-size = (size - 1) / 2
            ra-link(child-size, left, ra-link(child-size, right, next))
          end
          if f(val):
            rlink(val, rfilter(f, next-list))
          else:
            rfilter(f, next-list)
          end
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
      | nt-leaf => false
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
      | nt-leaf => true
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
    cases(NodeTree) nt:
      | nt-leaf => none
      | nt-branch(val, left, right) =>
        if f(val):
          some(val)
        else: 
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
      cases(NodeTree) tree:
        | nt-leaf => raise("Empty tree encountered in partition!")
        | nt-branch(val, left, right) =>
          next-list = if size == 1:
            next
          else:
            child-size = (size - 1) / 2
            ra-link(child-size, left, ra-link(child-size, right, next))
          end
          part-result = rpartition(f, next-list)
          if f(val):
            { is-true: rlink(val, part-result.is-true), is-false: part-result.is-false }
          else:
            { is-true: part-result.is-true, is-false: rlink(val, part-result.is-false) }
          end
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

fun rlist-to-list(rlist :: RandomAccessList) -> List:
  doc: "Convert a random access list to a normal list"
  fun nt-to-list(nt):
    cases(NodeTree) nt:
      | nt-leaf => []
      | nt-branch(val, left, right) => list.link(val, nt-to-list(left)) + nt-to-list(right)
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => []
    | ra-link(_, tree, next) => nt-to-list(tree) + rlist-to-list(next)
  end
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
