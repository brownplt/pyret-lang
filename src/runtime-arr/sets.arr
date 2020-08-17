#lang pyret/library

provide {
  set: list-set,
  list-set: list-set,
  tree-set: tree-set,
  empty-set: empty-list-set,
  empty-list-set: empty-list-set,
  empty-tree-set: empty-tree-set,
  list-to-set: list-to-list-set,
  list-to-list-set: list-to-list-set,
  list-to-tree-set: list-to-tree-set,
  fold: set-fold,
  all: set-all,
  any: set-any
} end
provide-types *

import global as G
include pick
include lists
import equality as equality
import raw-array as RA
# import valueskeleton as VS

include from RA:
    raw-array-fold
end

include from G:
  not,
  num-max,
  num-abs,
  raise,
  num-floor,
  num-ceiling,
  _lessthan
end

# SETS

# NOTE(alex): Sets are NOT parameterized by a type variable
#   b/c Pyret has no way to constrain type arguments to comparables
# To get around the lack of a guarenteed '<' operator, punt the
#   details to the "_lessthan" function which may throw a dynamic error
#
#   _lessthan :: (Any, Any -> Boolean)
#
# TODO(alex): Now that we rely on the the "_lessthan" function, can we parameterize
#   Sets? Otherwise, all type annotations will need to be in terms of Any
#
data AVLTree:
  | leaf
  | branch(value :: Any, h :: Number, left :: AVLTree, right :: AVLTree)
sharing:

  method height(self) -> Number:
    doc: "Returns the depth of the tree"
    cases(AVLTree) self:
      | leaf => 0
      | branch(_, _, _, _) => self.h
    end
  end,
  method contains(self, val :: Any) -> Boolean:
    doc: "Returns true of the tree contains val, otherwise returns false"
    cases(AVLTree) self:
      | leaf => false
      | branch(value, _, left, right) =>
        if (val == value):
          true
        else if _lessthan(val, value):
          left.contains(val)
        else:
          right.contains(val)
        end
    end
  end,
  method insert(self, val :: Any) -> AVLTree:
    doc: "Returns a new tree containing val but otherwise equal"
    cases(AVLTree) self:
      | leaf => mkbranch(val, leaf, leaf)
      | branch(value, _, left, right) =>
        if val == value:
          mkbranch(val, left, right)
        else if _lessthan(val, value):
          rebalance(mkbranch(value, left.insert(val), right))
        else:
          rebalance(mkbranch(value, left, right.insert(val)))
        end
    end
  end,
  method remove(self, val :: Any) -> AVLTree:
    doc: "Returns a new tree without val but otherwise equal"
    cases(AVLTree) self:
      | leaf => leaf
      | branch(value, _, left, right) =>
        if val == value:
          remove-root(self)
        else if _lessthan(val, value):
          rebalance(mkbranch(value, left.remove(val), right))
        else:
          rebalance(mkbranch(value, left, right.remove(val)))
        end
    end
  end,
  method preorder(self) -> List<Any>:
    doc: "Returns a list of all elements from a left-to-right preorder traversal"
    fun knil(l, x): link(x, l) end # needed because argument order of link is backwards to fold
    # TODO(alex): Why did self.fold-revpreorder() give the postorder?
    self.fold-revpostorder(knil, empty) # reversed because knil is reversed
  end,
  method inorder(self) -> List<Any>:
    doc: "Returns a list of all elements from a left-to-right inorder traversal"
    fun knil(l, x): link(x, l) end
    self.fold-revinorder(knil, empty)
  end,
  method postorder(self) -> List<Any>:
    doc: "Returns a list of all elements from a left-to-right postorder traversal"
    fun knil(l, x): link(x, l) end
    # TODO(alex): Why did self.fold-revpostorder() give the preorder?
    self.fold-revpreorder(knil, empty)
  end,
  method revpreorder(self) -> List<Any>:
    doc: "Returns a list of all elements from a right-to-left preorder traversal"
    fun knil(l, x): link(x, l) end
    self.fold-preorder(knil, empty)
  end,
  method revinorder(self) -> List<Any>:
    doc: "Returns a list of all elements from a right-to-leftinorder traversal"
    fun knil(l, x): link(x, l) end
    self.fold-inorder(knil, empty)
  end,
  method revpostorder(self) -> List<Any>:
    doc: "Returns a list of all elements from a roght-to-left postorder traversal"
    fun knil(l, x): link(x, l) end
    self.fold-postorder(knil, empty)
  end,
  method fold-preorder<a>(self, f :: (a, Any -> a), base :: a) -> a:
    doc: ```Folds the elements contained in the tree into a single value with f.
          analogous to folding a list, in a preorder traversal```
    cases(AVLTree) self:
      | leaf => base
      | branch(value, _, left, right) =>
        right.fold-preorder(f, left.fold-preorder(f, f(base, value)))
    end
  end,
  method fold-inorder<a>(self, f :: (a, Any -> a), base :: a) -> a:
    doc: ```Folds the elements contained in the tree into a single value with f.
          analogous to folding a list, in an inorder traversal```
    cases(AVLTree) self:
      | leaf => base
      | branch(value, _, left, right) =>
        right.fold-inorder(f, f(left.fold-inorder(f, base), value))
    end
  end,
  method fold-postorder<a>(self, f :: (a, Any -> a), base :: a) -> a:
    doc: ```Folds the elements contained in the tree into a single value with f.
          analogous to folding a list, in a postorder traversal```
    cases(AVLTree) self:
      | leaf => base
      | branch(value, _, left, right) =>
        f(right.fold-postorder(f, left.fold-postorder(f, base)), value)
    end
  end,
  method fold-revpreorder<a>(self, f :: (a, Any -> a), base :: a) -> a:
    doc: ```Folds the elements contained in the tree into a single value with f.
          analogous to folding a list, in a right-to-left preorder traversal```
    cases(AVLTree) self:
      | leaf => base
      | branch(value, _, left, right) =>
        left.fold-revpreorder(f, right.fold-revpreorder(f, f(base, value)))
    end
  end,
  method fold-revinorder<a>(self, f :: (a, Any -> a), base :: a) -> a:
    doc: ```Folds the elements contained in the tree into a single value with f.
          analogous to folding a list, in a right-to-left inorder traversal```
    cases(AVLTree) self:
      | leaf => base
      | branch(value, _, left, right) =>
        left.fold-revinorder(f, f(right.fold-revinorder(f, base), value))
    end
  end,
  method fold-revpostorder<a>(self, f :: (a, Any -> a), base :: a) -> a:
    doc: ```Folds the elements contained in the tree into a single value with f.
          analogous to folding a list, in a right-to-left postorder traversal```
    cases(AVLTree) self:
      | leaf => base
      | branch(value, _, left, right) =>
        f(left.fold-revpostorder(f, right.fold-revpostorder(f, base)), value)
    end
  end,
  method count(self):
    cases(AVLTree) self:
      | leaf => 0
      | branch(value, _, left, right) =>
        1 + left.count() + right.count()
    end
  end,
  method all(self, f :: (Any -> Boolean)) -> Boolean:
    cases(AVLTree) self:
      | leaf => true
      | branch(value, _, left, right) =>
        f(value) and right.all(f) and left.all(f)
    end
  end,
  method any(self, f :: (Any -> Boolean)) -> Boolean:
    cases(AVLTree) self:
      | leaf => false
      | branch(value, _, left, right) =>
        f(value) or right.all(f) or left.all(f)
    end
  end,
  method to-list(self) -> List<Any>:
    doc: "Returns a list of all elements from a inorder traversal"
    self.inorder()
  end,
  method _equals(self, other, eq):
    if not(is-AVLTree(other)):
      equality.NotEqual("Non-AVLTree", self, other)
    else:
      eq(self.inorder(), other.inorder())
    end
  end
end

fun tree-fold<a>(f :: (a, Any -> a), base :: a, tree :: AVLTree) -> a:
  tree.fold-preorder(f, base)
end

fun tree-all(f :: (Any -> Boolean), tree :: AVLTree) -> Boolean:
  tree.all(f)
end

fun tree-any(f :: (Any -> Boolean), tree :: AVLTree) -> Boolean:
  tree.any(f)
end

fun mkbranch(val :: Any, left :: AVLTree, right :: AVLTree):
  branch(val, num-max(left.height(), right.height()) + 1, left, right)
end

fun rebalance(tree :: AVLTree) -> AVLTree:
  fun left-left(t :: AVLTree) -> AVLTree:
    { value; _; left; right } = tree-get(t)
    { left-value; _; left-left-subtree; left-right-subtree } = tree-get(left)
    mkbranch(left-value, left-left-subtree, mkbranch(value, left-right-subtree, right))
  end

  fun right-right(t :: AVLTree) -> AVLTree:
    { value; _; left; right } = tree-get(t)
    { right-value; _; right-left-subtree; right-right-subtree } = tree-get(right)
    mkbranch(right-value, mkbranch(value, left, right-left-subtree), right-right-subtree)
  end
  fun left-right(t :: AVLTree) -> AVLTree:
    { value; _; left; right } = tree-get(t)
    { left-value; _; left-left-subtree; left-right-subtree } = tree-get(left)
    { left-right-value; _; left-right-left-subtree; left-right-right-subtree } = tree-get(left-right-subtree)
    mkbranch(left-right-value,
      mkbranch(left-value, left-left-subtree, left-right-left-subtree),
      mkbranch(value, left-right-right-subtree, right))
  end
  fun right-left(t :: AVLTree) -> AVLTree:
    { value; _; left; right } = tree-get(t)
    { right-value; _; right-left-subtree; right-right-subtree } = tree-get(right)
    { right-left-value; _; right-left-left-subtree; right-left-right-subtree } = tree-get(right-left-subtree)
    mkbranch(right-left-value,
      mkbranch(value, left, right-left-left-subtree),
      mkbranch(right-value, right-left-right-subtree, right-right-subtree))
  end

  cases(AVLTree) tree:
    | leaf => leaf
    | branch(value, height, left, right) =>
      lh = left.height()
      rh = right.height()
      if num-abs(lh - rh) <= 1:
        tree
      else if (lh - rh) == 2:
        { _; left-height; left-left-subtree; left-right-subtree } = tree-get-left(tree)
        if left-left-subtree.height() >= left-right-subtree.height():
          left-left(tree)
        else:
          left-right(tree)
        end
      else if (rh - lh) == 2:
        { _; right-height; right-left-subtree; right-right-subtree } = tree-get-right(tree)
        if right-right-subtree.height() >= right-left-subtree.height():
          right-right(tree)
        else:
          right-left(tree)
        end
      else:
        raise("AVL tree invariant has been broken!")
      end
  end
end

fun tree-get(tree :: AVLTree) -> { Any; Number; AVLTree; AVLTree }:
  cases(AVLTree) tree:
    | leaf => raise("Parent was a leaf")
    | branch(v, h, l, r) => { v; h; l; r}
  end
end


fun tree-get-left(tree :: AVLTree) -> { Any; Number; AVLTree; AVLTree }:
  cases(AVLTree) tree:
    | leaf => raise("Parent was a leaf")
    | branch(_, _, left, _) =>
      cases(AVLTree) left:
        | leaf => raise("Left subtree was a leaf")
        | branch(v, h, l, r) => { v; h; l ;r }
      end
  end
end

fun tree-get-right(tree :: AVLTree) -> { Any; Number; AVLTree; AVLTree }:
  cases(AVLTree) tree:
    | leaf => raise("Parent was a leaf")
    | branch(_, _, _, right) =>
      cases(AVLTree) right:
        | leaf => raise("Right subtree was a leaf")
        | branch(v, h, l, r) => { v; h; l ;r }
      end
  end
end

fun remove-root(tree :: AVLTree):
  cases(AVLTree) tree:
    | leaf => raise("Trying to remove the root of a leaf")
    | branch(_, _, left, right) =>
      if is-leaf(left):
        if is-leaf(right):
          leaf
        else:
          right
        end
      else:
        if is-leaf(right):
          left
        else:
          swap-next-lowest(tree)
        end
      end
  end
end

fun swap-next-lowest(tree :: AVLTree) -> AVLTree:
  fun greatest(t :: AVLTree) -> AVLTree:
    cases(AVLTree) t:
      | leaf => raise("Went too far in traversal step")
      | branch(_, _, _, right) => if is-leaf(right): t else: greatest(right) end
    end
  end
  fun remove-greatest-and-rebalance(t :: AVLTree) -> AVLTree:
    cases(AVLTree) t:
      | leaf => raise("Went too far in removal step")
      | branch(val, _, left, right) =>
        if is-leaf(right):
          left
        else:
          rebalance(mkbranch(val, left, remove-greatest-and-rebalance(right)))
        end
    end
  end

  { _; _; left; right } = tree-get(tree)
  { new-value; _; _; _ } = tree-get(greatest(left))
  rebalance(mkbranch(new-value,
      remove-greatest-and-rebalance(left),
      right))
end


check:
  tree1 =
    branch(4, 666, branch(2, 666, branch(1, 666, leaf, leaf), branch(3, 666, leaf, leaf)),
      branch(6, 666, branch(5, 666, leaf, leaf), leaf))
  tree1.inorder() is   [list: 1, 2, 3, 4, 5, 6]
  tree1.preorder() is  [list: 4, 2, 1, 3, 6, 5]
  tree1.postorder() is [list: 1, 3, 2, 5, 6, 4]
  tree1.revinorder() is   [list: 6, 5, 4, 3, 2, 1]
  tree1.revpreorder() is  [list: 5, 6, 3, 1, 2, 4]
  tree1.revpostorder() is [list: 4, 6, 5, 2, 3, 1]
end


data Set:
  | list-set(elems :: List<Any>)

    # TODO(alex): valueskeleton
    # method _output(self): VS.vs-collection("list-set", self.to-list().map(VS.vs-value)) end,

  | tree-set(elems :: AVLTree)

sharing:
  # Note(alex): Many methods are implemented as "sharing" b/c "with" methods cannot see other "with" methods
  #   Known restriction of the typechecker (see type-checker.arr:1226)


  method pick(self):
    cases(Set) self:
      | list-set(elems) =>
        lst = elems
        cases(List) lst:
          | empty => pick-none
          | link(f, r) =>
            cases(List) r:
              | empty => pick-some(f, list-set(empty))
              | link(f2, r2) =>
                # TODO(alex): implement rng
                # get-first = random(2)
                get-first = raise("sets TODO: random")
                if get-first == 0:
                  pick-some(f, list-set(r))
                else:
                  pick-some(f2, list-set(link(f, r2)))
                end
            end
        end
      | tree-set(elems) =>
        cases(AVLTree) elems:
          | leaf => pick-none
          | branch(v, _, _, _) =>
            pick-some(v, tree-set(elems.remove(v)))
        end
    end
  end,

  # TODO(alex): valueskeleton
  # method _output(self): VS.vs-collection("tree-set", self.to-list().map(VS.vs-value)) end,

  method fold<a>(self, f :: (a, Any -> a), base :: a) -> a:
    cases(Set) self:
      | list-set(elems) => fold(f, base, elems)
      | tree-set(elems) => tree-fold(f, base, elems)
    end
  end,

  method member(self, elem :: Any) -> Boolean:
    doc: 'Check to see if an element is in a set.'

    cases(Set) self:
      | list-set(elems) => elems.member(elem)
      | tree-set(elems) => elems.contains(elem)
    end
  end,

  method add(self, elem :: Any) -> Set:
    doc: "Add an element to the set if it is not already present."
    cases(Set) self:
      | list-set(elems) =>
        if (elems.member(elem)):
          self
        else:
          list-set(link(elem, elems))
        end
      | tree-set(elems) => tree-set(elems.insert(elem))
    end
  end,

  method remove(self, elem :: Any) -> Set:
    doc: "Remove an element from the set if it is present."
    cases(Set) self:
      | list-set(elems) => list-set(elems.remove(elem))
      | tree-set(elems) => tree-set(elems.remove(elem))
    end
  end,

  method to-list(self) -> List<Any>:
    doc: 'Convert a set into a list of elements.'
    cases(Set) self:
      | list-set(elems) => elems
      | tree-set(elems) => elems.inorder()
    end
  end,

  method union(self, other :: Set) -> Set:
    doc: 'Compute the union of this set and another set.'
    cases(Set) self:
      | list-set(elems) =>
        other.fold(lam(u :: Set, elem :: Any):
          u.add(elem)
        end, list-set(elems))
      | tree-set(elems) => tree-set-union(elems, other)
    end
  end,

  method intersect(self, other :: Set) -> Set:
    doc: 'Compute the intersection of this set and another set.'

    cases(Set) self:
      | list-set(elems) =>
        new-elems = for fold(shadow elems from elems, elem from elems):
          if other.member(elem):
            elems
          else:
            elems.remove(elem)
          end
        end
        list-set(new-elems)
      | tree-set(elems) => tree-set-intersect(elems, other)
    end
  end,

  method overlaps(self :: Set, other :: Set) -> Boolean:
    doc: 'Determines if the intersection of this set and another set is non-empty.'
    self.any(other.member)
  end,

  method difference(self :: Set, other :: Set) -> Set:
    doc: 'Compute the difference of this set and another set.'
    cases(Set) self:
      | list-set(elems) =>
        new-elems = for fold(shadow elems from elems, elem from elems):
          if other.member(elem):
            elems.remove(elem)
          else:
            elems
          end
        end
      list-set(new-elems)
      | tree-set(elems) => tree-set-difference(elems, other)
    end

  end,

  method size(self :: Set) -> Number:
    cases(Set) self:
      | list-set(elems) => elems.length()
      | tree-set(elems) => elems.count()
    end
  end,

  method is-empty(self):
    cases(Set) self:
      | list-set(elems) => is-empty(elems)
      | tree-set(elems) => is-leaf(elems)
    end
  end,

  method all(self, f) -> Boolean:
    cases(Set) self:
      | list-set(elems) => elems.all(f)
      | tree-set(elems) => elems.all(f)
    end
  end,

  method any(self, f) -> Boolean:
    cases(Set) self:
      | list-set(elems) => elems.any(f)
      | tree-set(elems) => elems.any(f)
    end
  end,

  method symmetric-difference(self :: Set, other :: Set) -> Set:
    doc: 'Compute the symmetric difference of this set and another set.'
    self.union(other).difference(self.intersect(other))
  end,

  method _equals(self, other, eq):
    if not(is-Set(other)):
      equality.NotEqual("Non-Set", self, other)
    else:
      self-list :: List<Any> = self.to-list()
      other-list :: List<Any> = other.to-list()
      if not(other-list.length() == self-list.length()):
        equality.NotEqual("set size", self, other)
      else:
        for fold(result from equality.Equal, elt from self-list):
          result-for-elt = member-with(other-list, elt, eq)
          equality.equal-and(result, result-for-elt)
        end
      end
    end
  end
end

fun set-to-sorted-elems(s :: Set) -> List<Any>:
  cases(Set) s:
    | list-set(elems) => elems.sort()
    | tree-set(elems) => elems.inorder()
  end
end

fun elems-to-balanced-avl(elems):
  doc: ```
       Constructs a balanced (but not full) binary search tree from the given sorted list of items.
       Note: algorithm is O(elems.length()), using a mutable pointer into the element list to ensure
       that each item gets processed once, in order, as the tree is being constructed.
       ```
  var head = elems
  len = elems.length()
  fun helper(l):
    if l <= 0 block: leaf
    else if is-empty(head): leaf
    else:
      left = helper(num-floor(l / 2))
      item = head.first
      head := head.rest
      branch(item, left.height() + 1, left, helper(num-ceiling((l / 2) - 1)))
    end
  end
  helper(len)
where:
  elems-to-balanced-avl(empty) is leaf
  elems-to-balanced-avl([list: 1, 2, 3, 4, 5]) is
  branch(3, 3, branch(2, 2, branch(1, 1, leaf, leaf), leaf),
    branch(5, 2, branch(4, 1, leaf, leaf), leaf))
  elems-to-balanced-avl([list: 1, 2, 3, 4, 5, 6]) is
  branch(4, 3, branch(2, 2, branch(1, 1, leaf, leaf), branch(3, 1, leaf, leaf)),
    branch(6, 2, branch(5, 1, leaf, leaf), leaf))
end

fun merge-no-dups(l1, l2):
  doc: "Returns a merged list of the values in both input sorted lists, discarding duplicates"
  if is-empty(l1): l2
  else if is-empty(l2): l1
  else if l1.first < l2.first:  link(l1.first, merge-no-dups(l1.rest, l2))
  else if l1.first == l2.first: merge-no-dups(l1.rest, l2)
  else:                         link(l2.first, merge-no-dups(l1, l2.rest))
  end
where:
  merge-no-dups([list: 1, 3, 5, 6], [list: 1, 2, 4, 5]) is [list: 1, 2, 3, 4, 5, 6]
end

fun tree-set-union(s1, s2) -> Set:
  s1-elems = set-to-sorted-elems(s1)
  s2-elems = set-to-sorted-elems(s2)
  new-elems = merge-no-dups(s1-elems, s2-elems)
  tree-set(elems-to-balanced-avl(new-elems))
end

fun merge-only-dups(l1, l2):
  doc: "Returns a list of the duplicate values appearing in both input sorted lists"
  if is-empty(l1) or is-empty(l2): empty
  else if l1.first < l2.first:  merge-only-dups(l1.rest, l2)
  else if l1.first == l2.first: link(l1.first, merge-only-dups(l1.rest, l2.rest))
  else:                         merge-only-dups(l1, l2.rest)
  end
where:
  merge-only-dups([list: 1, 3, 5, 6], [list: 1, 2, 4, 5]) is [list: 1, 5]
end

fun tree-set-intersect(s1, s2) -> Set:
  s1-elems = set-to-sorted-elems(s1)
  s2-elems = set-to-sorted-elems(s2)
  new-elems = merge-only-dups(s1-elems, s2-elems)
  tree-set(elems-to-balanced-avl(new-elems))
end

fun merge-drop-l2(l1, l2):
  doc: "Returns the items in the first sorted list that are not in the second sorted list"
  if is-empty(l1) or is-empty(l2): l1
  else if l1.first == l2.first: merge-drop-l2(l1.rest, l2.rest)
  else if l1.first < l2.first:  link(l1.first, merge-drop-l2(l1.rest, l2))
  else:                         merge-drop-l2(l1, l2.rest)
  end
where:
  merge-drop-l2([list: 1, 3, 5, 6], [list: 1, 2, 4, 5]) is [list: 3, 6]
end

fun tree-set-difference(s1, s2) -> Set:
  s1-elems = set-to-sorted-elems(s1)
  s2-elems = set-to-sorted-elems(s2)
  new-elems = merge-drop-l2(s1-elems, s2-elems)
  tree-set(elems-to-balanced-avl(new-elems))
end

fun set-all(f, s :: Set) -> Boolean:
  s.all(f)
end

fun set-any(f, s :: Set) -> Boolean:
  s.any(f)
end

fun set-fold(f, base, s :: Set):
  s.fold(f, base)
end

fun list-to-set(lst :: List<Any>, base-set :: Set) -> Set:
  doc: "Convert a list into a set."
  for fold(s :: Set from base-set, elem from lst):
    s.add(elem)
  end
end

fun list-to-list-set(lst :: List<Any>) -> Set:
  doc: "Convert a list into a list-based set."
  list-to-set(lst, list-set(empty))
end

fun list-to-tree-set(lst :: List<Any>) -> Set:
  doc: "Convert a list into a tree-based set."
  list-to-set(lst, tree-set(leaf))
end

fun list-to-tree(lst :: List<Any>) -> AVLTree:
  for fold(tree from leaf, elt from lst):
    tree.insert(elt)
  end
end

fun arr-to-list-set(arr :: RawArray<Any>) -> Set:
  raw-array-fold(
    lam(acc :: Set, elem :: Any):
      acc.add(elem)
    end,
    list-set(empty),
    arr
  )
end

fun arr-to-tree-set(arr :: RawArray<Any>) -> Set:
  tree = raw-array-fold(
    lam(acc :: AVLTree, elem :: Any):
      acc.insert(elem)
    end,
    leaf,
    arr
  )
  tree-set(tree)
end

empty-list-set = list-set(empty)
empty-tree-set = tree-set(leaf)

fun makeSet2(a, b):
  if a == b: link(a, empty)
  else: link(a, link(b, empty))
  end
end
fun makeSet3(a, b, c):
  if      a == b: makeSet2(b, c)
  else if a == c: makeSet2(a, b)
  else:           link(a, makeSet2(b, c))
  end
end
fun makeSet4(a, b, c, d):
  if      a == b: makeSet3(b, c, d)
  else if a == c: makeSet3(a, b, d)
  else if a == d: makeSet3(a, b, c)
  else:           link(a, makeSet3(b, c, d))
  end
end
fun makeSet5(a, b, c, d, e):
  if      a == b: makeSet4(b, c, d, e)
  else if a == c: makeSet4(a, b, d, e)
  else if a == d: makeSet4(a, b, c, e)
  else if a == e: makeSet4(a, b, c, d)
  else:           link(a, makeSet4(b, c, d, e))
  end
end

shadow list-set = {
  make: arr-to-list-set,
  make0: lam() -> Set: empty-list-set end,
  make1: lam<x>(a :: x) -> Set: list-set(link(a, empty)) end,
  make2: lam<x>(a :: x, b :: x) -> Set: list-set(makeSet2(a, b)) end,
  make3: lam<x>(a :: x, b :: x, c :: x) -> Set: list-set(makeSet3(a, b, c)) end,
  make4: lam<x>(a :: x, b :: x, c :: x, d :: x) -> Set: list-set(makeSet4(a, b, c, d)) end,
  make5: lam<x>(a :: x, b :: x, c :: x, d :: x, e :: x) -> Set: list-set(makeSet5(a, b, c, d, e)) end
}

shadow tree-set = {
  make: arr-to-tree-set,
  make0: lam() -> Set: empty-tree-set end,
  make1: lam<x>(a :: x) -> Set: empty-tree-set.add(a) end,
  make2: lam<x>(a :: x, b :: x) -> Set: empty-tree-set.add(a).add(b) end,
  make3: lam<x>(a :: x, b :: x, c :: x) -> Set: empty-tree-set.add(a).add(b).add(c) end,
  make4: lam<x>(a :: x, b :: x, c :: x, d :: x) -> Set: empty-tree-set.add(a).add(b).add(c).add(d) end,
  make5: lam<x>(a :: x, b :: x, c :: x, d :: x, e :: x) -> Set: empty-tree-set.add(a).add(b).add(c).add(d).add(e) end
}

empty-set = empty-list-set
shadow set = list-set
shadow list-to-set = list-to-list-set
shadow fold = set-fold
shadow all = set-all
shadow any = set-any
