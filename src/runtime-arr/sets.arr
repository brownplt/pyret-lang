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

import primitive-types as _
import runtime-global as G
include pick
include lists
import equality as equality
import raw-array as RA
import number as N
include from N: random end
import valueskeleton as VS
include from VS: data ValueSkeleton end

include from RA:
  raw-array-fold, raw-array-map, raw-array-from-list
end

include from N:
  num-max,
  num-abs,
  num-floor,
  num-ceiling,
end

include from G:
  not,
  raise,
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
data AVLTree<a>:
  | leaf
  | branch(value :: a, h :: Number, left :: AVLTree<a>, right :: AVLTree<a>)
sharing:

  method height(self) -> Number:
    doc: "Returns the depth of the tree"
    cases(AVLTree) self:
      | leaf => 0
      | branch(_, _, _, _) => self.h
    end
  end,
  method contains(self, val :: a) -> Boolean:
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
  method insert(self, val :: a) -> AVLTree<a>:
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
  method remove(self, val :: a) -> AVLTree<a>:
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
  method preorder(self) -> List<a>:
    doc: "Returns a list of all elements from a left-to-right preorder traversal"
    fun knil(l, x): link(x, l) end # needed because argument order of link is backwards to fold
    # TODO(alex): Why did self.fold-revpreorder() give the postorder?
    self.fold-revpostorder(knil, empty) # reversed because knil is reversed
  end,
  method inorder(self) -> List<a>:
    doc: "Returns a list of all elements from a left-to-right inorder traversal"
    fun knil(l, x): link(x, l) end
    self.fold-revinorder(knil, empty)
  end,
  method postorder(self) -> List<a>:
    doc: "Returns a list of all elements from a left-to-right postorder traversal"
    fun knil(l, x): link(x, l) end
    # TODO(alex): Why did self.fold-revpostorder() give the preorder?
    self.fold-revpreorder(knil, empty)
  end,
  method revpreorder(self) -> List<a>:
    doc: "Returns a list of all elements from a right-to-left preorder traversal"
    fun knil(l, x): link(x, l) end
    self.fold-preorder(knil, empty)
  end,
  method revinorder(self) -> List<a>:
    doc: "Returns a list of all elements from a right-to-leftinorder traversal"
    fun knil(l, x): link(x, l) end
    self.fold-inorder(knil, empty)
  end,
  method revpostorder(self) -> List<a>:
    doc: "Returns a list of all elements from a roght-to-left postorder traversal"
    fun knil(l, x): link(x, l) end
    self.fold-postorder(knil, empty)
  end,
  method fold-preorder<b>(self, f :: (b, a -> b), base :: b) -> b:
    doc: ```Folds the elements contained in the tree into a single value with f.
          analogous to folding a list, in a preorder traversal```
    cases(AVLTree) self:
      | leaf => base
      | branch(value, _, left, right) =>
        right.fold-preorder(f, left.fold-preorder(f, f(base, value)))
    end
  end,
  method fold-inorder<b>(self, f :: (b, a -> b), base :: b) -> b:
    doc: ```Folds the elements contained in the tree into a single value with f.
          analogous to folding a list, in an inorder traversal```
    cases(AVLTree) self:
      | leaf => base
      | branch(value, _, left, right) =>
        right.fold-inorder(f, f(left.fold-inorder(f, base), value))
    end
  end,
  method fold-postorder<b>(self, f :: (b, a -> b), base :: b) -> b:
    doc: ```Folds the elements contained in the tree into a single value with f.
          analogous to folding a list, in a postorder traversal```
    cases(AVLTree) self:
      | leaf => base
      | branch(value, _, left, right) =>
        f(right.fold-postorder(f, left.fold-postorder(f, base)), value)
    end
  end,
  method fold-revpreorder<b>(self, f :: (b, a -> b), base :: b) -> b:
    doc: ```Folds the elements contained in the tree into a single value with f.
          analogous to folding a list, in a right-to-left preorder traversal```
    cases(AVLTree) self:
      | leaf => base
      | branch(value, _, left, right) =>
        left.fold-revpreorder(f, right.fold-revpreorder(f, f(base, value)))
    end
  end,
  method fold-revinorder<b>(self, f :: (b, a -> b), base :: b) -> b:
    doc: ```Folds the elements contained in the tree into a single value with f.
          analogous to folding a list, in a right-to-left inorder traversal```
    cases(AVLTree) self:
      | leaf => base
      | branch(value, _, left, right) =>
        left.fold-revinorder(f, f(right.fold-revinorder(f, base), value))
    end
  end,
  method fold-revpostorder<b>(self, f :: (b, a -> b), base :: b) -> b:
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
  method all(self, f :: (a -> Boolean)) -> Boolean:
    cases(AVLTree) self:
      | leaf => true
      | branch(value, _, left, right) =>
        f(value) and right.all(f) and left.all(f)
    end
  end,
  method any(self, f :: (a -> Boolean)) -> Boolean:
    cases(AVLTree) self:
      | leaf => false
      | branch(value, _, left, right) =>
        f(value) or right.all(f) or left.all(f)
    end
  end,
  method to-list(self) -> List<a>:
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

fun tree-fold<a, b>(f :: (b, a -> b), base :: b, tree :: AVLTree<a>) -> b:
  tree.fold-preorder(f, base)
end

fun tree-all<a>(f :: (a -> Boolean), tree :: AVLTree<a>) -> Boolean:
  tree.all(f)
end

fun tree-any<a>(f :: (a -> Boolean), tree :: AVLTree<a>) -> Boolean:
  tree.any(f)
end

fun mkbranch<a>(val :: a, left :: AVLTree<a>, right :: AVLTree<a>) -> AVLTree<a>:
  branch(val, num-max(left.height(), right.height()) + 1, left, right)
end

fun rebalance<a>(tree :: AVLTree<a>) -> AVLTree<a>:
  fun left-left(t :: AVLTree<a>) -> AVLTree<a>:
    { value; _; left; right } = tree-get(t)
    { left-value; _; left-left-subtree; left-right-subtree } = tree-get(left)
    mkbranch(left-value, left-left-subtree, mkbranch(value, left-right-subtree, right))
  end

  fun right-right(t :: AVLTree<a>) -> AVLTree<a>:
    { value; _; left; right } = tree-get(t)
    { right-value; _; right-left-subtree; right-right-subtree } = tree-get(right)
    mkbranch(right-value, mkbranch(value, left, right-left-subtree), right-right-subtree)
  end
  fun left-right(t :: AVLTree<a>) -> AVLTree<a>:
    { value; _; left; right } = tree-get(t)
    { left-value; _; left-left-subtree; left-right-subtree } = tree-get(left)
    { left-right-value; _; left-right-left-subtree; left-right-right-subtree } = tree-get(left-right-subtree)
    mkbranch(left-right-value,
      mkbranch(left-value, left-left-subtree, left-right-left-subtree),
      mkbranch(value, left-right-right-subtree, right))
  end
  fun right-left(t :: AVLTree<a>) -> AVLTree<a>:
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

fun tree-get<a>(tree :: AVLTree<a>) -> { a; Number; AVLTree<a>; AVLTree<a> }:
  cases(AVLTree) tree:
    | leaf => raise("Parent was a leaf")
    | branch(v, h, l, r) => { v; h; l; r}
  end
end


fun tree-get-left<a>(tree :: AVLTree<a>) -> { a; Number; AVLTree<a>; AVLTree<a> }:
  cases(AVLTree) tree:
    | leaf => raise("Parent was a leaf")
    | branch(_, _, left, _) =>
      cases(AVLTree) left:
        | leaf => raise("Left subtree was a leaf")
        | branch(v, h, l, r) => { v; h; l ;r }
      end
  end
end

fun tree-get-right<a>(tree :: AVLTree<a>) -> { a; Number; AVLTree<a>; AVLTree<a> }:
  cases(AVLTree) tree:
    | leaf => raise("Parent was a leaf")
    | branch(_, _, _, right) =>
      cases(AVLTree) right:
        | leaf => raise("Right subtree was a leaf")
        | branch(v, h, l, r) => { v; h; l ;r }
      end
  end
end

fun remove-root<a>(tree :: AVLTree<a>) -> AVLTree<a>:
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

fun swap-next-lowest<a>(tree :: AVLTree<a>) -> AVLTree<a>:
  fun greatest(t :: AVLTree<a>) -> AVLTree<a>:
    cases(AVLTree) t:
      | leaf => raise("Went too far in traversal step")
      | branch(_, _, _, right) => if is-leaf(right): t else: greatest(right) end
    end
  end
  fun remove-greatest-and-rebalance(t :: AVLTree<a>) -> AVLTree<a>:
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


data Set<a>:
  | list-set(elems :: List<a>)
  | tree-set(elems :: AVLTree<a>)
sharing:
  # Note(alex): Many methods are implemented as "sharing" b/c "with" methods cannot see other "with" methods
  #   Known restriction of the typechecker (see type-checker.arr:1226)


  method pick(self) -> Pick<a, Set<a>>:
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
                get-first = random(2)
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

  method _output(self, output :: (Any -> ValueSkeleton)):
    name = cases(Set) self:
      | list-set(_) => "list-set"
      | tree-set(_) => "tree-set"
    end
    VS.vs-collection(name, raw-array-map(output, raw-array-from-list(self.to-list())))
  end,

  method fold<b>(self, f :: (b, a -> b), base :: b) -> b:
    cases(Set) self:
      | list-set(elems) => fold(f, base, elems)
      | tree-set(elems) => tree-fold(f, base, elems)
    end
  end,

  method member(self, elem :: a) -> Boolean:
    doc: 'Check to see if an element is in a set.'

    cases(Set) self:
      | list-set(elems) => elems.member(elem)
      | tree-set(elems) => elems.contains(elem)
    end
  end,

  method add(self, elem :: a) -> Set<a>:
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

  method remove(self, elem :: a) -> Set<a>:
    doc: "Remove an element from the set if it is present."
    cases(Set) self:
      | list-set(elems) => list-set(elems.remove(elem))
      | tree-set(elems) => tree-set(elems.remove(elem))
    end
  end,

  method to-list(self) -> List<a>:
    doc: 'Convert a set into a list of elements.'
    cases(Set) self:
      | list-set(elems) => elems
      | tree-set(elems) => elems.inorder()
    end
  end,

  method union(self, other :: Set<a>) -> Set<a>:
    doc: 'Compute the union of this set and another set.'
    cases(Set) self:
      | list-set(elems) =>
        other.fold(lam(u :: Set<a>, elem :: a):
          u.add(elem)
        end, list-set(elems))
      | tree-set(_) => tree-set-union(self, other)
    end
  end,

  method intersect(self, other :: Set<a>) -> Set<a>:
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
      | tree-set(_) => tree-set-intersect(self, other)
    end
  end,

  method overlaps(self :: Set<a>, other :: Set<a>) -> Boolean:
    doc: 'Determines if the intersection of this set and another set is non-empty.'
    self.any(other.member)
  end,

  method difference(self :: Set<a>, other :: Set<a>) -> Set<a>:
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
      | tree-set(_) => tree-set-difference(self, other)
    end

  end,

  method size(self :: Set<a>) -> Number:
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

  method symmetric-difference(self :: Set<a>, other :: Set<a>) -> Set<a>:
    doc: 'Compute the symmetric difference of this set and another set.'
    self.union(other).difference(self.intersect(other))
  end,

  method _equals(self, other, eq):
    if not(is-Set(other)):
      equality.NotEqual("Non-Set", self, other)
    else:
      self-list :: List<a> = self.to-list()
      other-list :: List<a> = other.to-list()
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

fun set-to-sorted-elems<a>(s :: Set<a>) -> List<a>:
  cases(Set) s:
    | list-set(elems) => elems.sort()
    | tree-set(elems) => elems.inorder()
  end
end

fun elems-to-balanced-avl<a>(elems :: List<a>) -> AVLTree<a> block:
  doc: ```
       Constructs a balanced (but not full) binary search tree from the given sorted list of items.
       Note: algorithm is O(elems.length()), using a mutable pointer into the element list to ensure
       that each item gets processed once, in order, as the tree is being constructed.
       ```

  len = elems.length()
  # TODO(alex): This implementation results in a type checking error for `head := rest`
  #   Found t-data-refinement(x) but expected t-ref(t-data-refinement(x))
  #
  # var head = empty
  # head := elems
  # fun helper(l :: Number) -> AVLTree<a>:
  #   if l <= 0 block: leaf
  #   else:
  #     cases(List) head:
  #       | link(first :: a, rest :: List<a>) =>
  #         block:
  #           left = helper(num-floor(l / 2))
  #           item = first
  #           head := rest
  #           branch(item, left.height() + 1, left, helper(num-ceiling((l / 2) - 1)))
  #         end

  #       | empty => leaf
  #     end
  #   end
  # end

  # TODO(alex): Swap back to normal helper() once type checking works
  #   v2 probably has worse performance characteristics unoptimized
  fun helperv2(l :: Number, head :: List<a>) -> { List<a>; AVLTree<a> }:
    if l <= 0 block: { head; leaf }
    else:
      cases(List) head:
        | link(first :: a, rest :: List<a>) =>
          { left-head-out; left } = helperv2(num-floor(l / 2), head)
          { right-head-in; item } = cases(List) left-head-out:
            | link(newf, inner-rest) => { inner-rest; newf }
            | empty => raise("unreachable")
          end

          { right-head-out; right } = helperv2(num-ceiling((l / 2) - 1), right-head-in)
          { right-head-out; branch(item, left.height() + 1, left, right) }
        | empty => { head; leaf }
      end
    end
  end

  { _; result } = helperv2(len, elems)
  result
  # helper(len)
where:
  elems-to-balanced-avl([list: 1]) is
  branch(1, 1, leaf, leaf)
  elems-to-balanced-avl([list: 1, 2]) is
  branch(2, 2, branch(1, 1, leaf, leaf), leaf)
  elems-to-balanced-avl([list: 1, 2, 3]) is
  branch(2, 2, branch(1, 1, leaf, leaf), branch(3, 1, leaf, leaf))

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

fun tree-set-union<a>(s1 :: Set<a>, s2 :: Set<a>) -> Set<a>:
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

fun tree-set-intersect<a>(s1 :: Set<a>, s2 :: Set<a>) -> Set<a>:
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

fun tree-set-difference<a>(s1 :: Set<a>, s2 :: Set<a>) -> Set<a>:
  s1-elems = set-to-sorted-elems(s1)
  s2-elems = set-to-sorted-elems(s2)
  new-elems = merge-drop-l2(s1-elems, s2-elems)
  tree-set(elems-to-balanced-avl(new-elems))
end

fun set-all<a>(f, s :: Set<a>) -> Boolean:
  s.all(f)
end

fun set-any<a>(f, s :: Set<a>) -> Boolean:
  s.any(f)
end

fun set-fold<a, b>(f :: (b, a -> b), base :: b, s :: Set<a>) -> b:
  s.fold(f, base)
end

fun list-to-set<a>(lst :: List<a>, base-set :: Set<a>) -> Set<a>:
  doc: "Convert a list into a set."
  for fold(s :: Set<a> from base-set, elem :: a from lst):
    s.add(elem)
  end
end

fun list-to-list-set<a>(lst :: List<a>) -> Set<a>:
  doc: "Convert a list into a list-based set."
  list-to-set(lst, list-set(empty))
end

fun list-to-tree-set<a>(lst :: List<a>) -> Set<a>:
  doc: "Convert a list into a tree-based set."
  list-to-set(lst, tree-set(leaf))
end

fun list-to-tree<a>(lst :: List<a>) -> AVLTree<a>:
  for fold(tree from leaf, elt from lst):
    tree.insert(elt)
  end
end

fun arr-to-list-set<a>(arr :: RawArray<a>) -> Set<a>:
  raw-array-fold(
    lam(acc :: Set<a>, elem :: a, idx :: Number):
      acc.add(elem)
    end,
    list-set(empty),
    arr,
    0
  )
end

fun arr-to-tree-set<a>(arr :: RawArray<a>) -> Set<a>:
  tree = raw-array-fold(
    lam(acc :: AVLTree<a>, elem :: a, idx :: Number):
      acc.insert(elem)
    end,
    leaf,
    arr,
    0
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
  make0: lam<x>() -> Set<x>: empty-list-set end,
  make1: lam<x>(a :: x) -> Set<x>: list-set(link(a, empty)) end,
  make2: lam<x>(a :: x, b :: x) -> Set<x>: list-set(makeSet2(a, b)) end,
  make3: lam<x>(a :: x, b :: x, c :: x) -> Set<x>: list-set(makeSet3(a, b, c)) end,
  make4: lam<x>(a :: x, b :: x, c :: x, d :: x) -> Set<x>: list-set(makeSet4(a, b, c, d)) end,
  make5: lam<x>(a :: x, b :: x, c :: x, d :: x, e :: x) -> Set<x>: list-set(makeSet5(a, b, c, d, e)) end
}

shadow tree-set = {
  make: arr-to-tree-set,
  make0: lam<x>() -> Set<x>: empty-tree-set end,
  make1: lam<x>(a :: x) -> Set<x>: empty-tree-set.add(a) end,
  make2: lam<x>(a :: x, b :: x) -> Set<x>: empty-tree-set.add(a).add(b) end,
  make3: lam<x>(a :: x, b :: x, c :: x) -> Set<x>: empty-tree-set.add(a).add(b).add(c) end,
  make4: lam<x>(a :: x, b :: x, c :: x, d :: x) -> Set<x>: empty-tree-set.add(a).add(b).add(c).add(d) end,
  make5: lam<x>(a :: x, b :: x, c :: x, d :: x, e :: x) -> Set<x>: empty-tree-set.add(a).add(b).add(c).add(d).add(e) end
}

empty-set = empty-list-set
shadow set = list-set
shadow list-to-set = list-to-list-set
shadow fold = set-fold
shadow all = set-all
shadow any = set-any
