#lang pyret/library

provide {
  RandomAccessList: RandomAccessList,
  rempty: ra-empty,
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

import "../pyret-lib/moorings.rkt" as M

list = M.list
error = M.error
builtins = M.builtins
checkers = M.checkers
option = M.option
List = list.List

data NodeTree:
  | nt-leaf
  | nt-branch(val :: Any, left :: NodeTree, right :: NodeTree)
end

data RandomAccessList:
  | ra-empty
  | ra-link(size :: Number, tree :: NodeTree, next :: RandomAccessList)
end

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
#where:
#  rlink("foo", rempty) is ra-link(1, nt-branch("foo", nt-leaf, nt-leaf), ra-empty)
#  rlink("foo", rlink("bar", rempty))
#    is ra-link(1, nt-branch("foo", nt-leaf, nt-leaf),
#               ra-link(1, nt-branch("bar", nt-leaf, nt-leaf), ra-empty))
#  rlink("foo", rlink("bar", rlink("baz", rempty)))
#    is ra-link(3,
#               nt-branch("foo", nt-branch("bar", nt-leaf, nt-leaf),
#                                nt-branch("baz", nt-leaf, nt-leaf)),
#               ra-empty)
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
end

fun rlength(rlist :: RandomAccessList) -> Number:
  doc: "Get the length of a random access list"
  cases(RandomAccessList) rlist:
    | ra-empty => 0
    | ra-link(size, _, next) => size + rlength(next)
  end
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
end

fun list-to-rlist(lst :: List) -> RandomAccessList:
  doc: "Convert a list to a random access list"
  lst.foldr(rlink, ra-empty)
end
