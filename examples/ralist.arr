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
  rfold: rfold,
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
  fun nt-map(fn, nt):
    cases(NodeTree) nt:
      | nt-leaf => nt-leaf
      | nt-branch(val, left, right) => nt-branch(fn(val), nt-map(fn, left), nt-map(fn, right))
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => ra-empty
    | ra-link(size, tree, next) => ra-link(size, nt-map(f, tree), rmap(f, next))
  end
where:
  rmap-inc = fun(rl): rmap(fun(n): n + 1 end, rl) end
  
  rmap-inc(rempty) is rempty
  rmap-inc(rlink(1, rempty)) is rlink(2, rempty)
  rmap-inc(rlink(1, rlink(2, rempty))) is rlink(2, rlink(3, rempty))
  rmap-inc(rlink(1, rlink(2, rlink(3, rlink(4, rempty)))))
    is rlink(2, rlink(3, rlink(4, rlink(5, rempty))))
end

fun rfold(f, base, rlist :: RandomAccessList):
  doc: "Accumulates all elements in the random access list using f"
  fun nt-fold(fn, b, nt):
    cases(NodeTree) nt:
      | nt-leaf => b
      | nt-branch(val, left, right) => nt-fold(fn, nt-fold(fn, fn(val, b), left), right)
    end
  end
  cases(RandomAccessList) rlist:
    | ra-empty => base
    | ra-link(_, tree, next) => rfold(f, nt-fold(f, base, tree), next)
  end
where:
  rfold-sum = fun(rl): rfold(fun(n, m): n + m end, 0, rl) end
  
  rfold-sum(rempty) is 0
  rfold-sum(rlink(1, rempty)) is 1
  rfold-sum(rlink(1, rlink(2, rempty))) is 3
  rfold-sum(rlink(1, rlink(2, rlink(3, rlink(4, rempty))))) is 10
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
