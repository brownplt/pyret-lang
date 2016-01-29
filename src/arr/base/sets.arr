#lang pyret/library

provide {
  set: list-set-maker,
  list-set: list-set-maker,
  tree-set: tree-set-maker,
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

import pick as pick
import lists as lists
import error as error
import option as option
import arrays as arrays
import equality as equality
import valueskeleton as VS

type List = lists.List
List = lists.List
empty = lists.empty
link = lists.link
is-empty = lists.is-empty
fold = lists.fold

type Pick = pick.Pick
pick-none = pick.pick-none
pick-some = pick.pick-some

# SETS

data AVLTree:
  | leaf with:
    height(self) -> Number: 0 end,
    contains(self, val :: Any) -> Boolean: false end,
    insert(self, val :: Any) -> AVLTree: mkbranch(val, leaf, leaf) end,
    remove(self, val :: Any) -> AVLTree: leaf end,
    preorder(self) -> lists.List: empty end,
    inorder(self) -> lists.List: empty end,
    postorder(self) -> lists.List: empty end,
    revpreorder(self) -> lists.List: empty end,
    revinorder(self) -> lists.List: empty end,
    revpostorder(self) -> lists.List: empty end,
    fold-preorder(self, f, base): base end,
    fold-inorder(self, f, base): base end,
    fold-postorder(self, f, base): base end,
    fold-revpreorder(self, f, base): base end,
    fold-revinorder(self, f, base): base end,
    fold-revpostorder(self, f, base): base end,
    count(self): 0 end,
    all(self, f): true end,
    any(self, f): false end

  | branch(value :: Any, h :: Number, left :: AVLTree, right :: AVLTree) with:
    height(self) -> Number:
      doc: "Returns the depth of the tree"
      self.h
    end,
    contains(self, val :: Any) -> Boolean:
      doc: "Returns true of the tree contains val, otherwise returns false"
      if val == self.value: true
      else if val < self.value: self.left.contains(val)
      else: self.right.contains(val)
      end
    end,
    insert(self, val :: Any) -> AVLTree:
      doc: "Returns a new tree containing val but otherwise equal"
      if val == self.value: mkbranch(val, self.left, self.right)
      else if val < self.value:
        rebalance(mkbranch(self.value, self.left.insert(val), self.right))
      else:
        rebalance(mkbranch(self.value, self.left, self.right.insert(val)))
      end
    end,
    remove(self, val :: Any) -> AVLTree:
      doc: "Returns a new tree without val but otherwise equal"
      if val == self.value: remove-root(self)
      else if val < self.value:
        rebalance(mkbranch(self.value, self.left.remove(val), self.right))
      else:
        rebalance(mkbranch(self.value, self.left, self.right.remove(val)))
      end
    end,
    preorder(self) -> lists.List:
      doc: "Returns a list of all elements from a left-to-right preorder traversal"
      fun knil(l, x): link(x, l) end # needed because argument order of link is backwards to fold
      self.fold-revpreorder(knil, empty) # reversed because knil is reversed
    end,
    inorder(self) -> lists.List:
      doc: "Returns a list of all elements from a left-to-right inorder traversal"
      fun knil(l, x): link(x, l) end
      self.fold-revinorder(knil, empty)
    end,
    postorder(self) -> lists.List:
      doc: "Returns a list of all elements from a left-to-right postorder traversal"
      fun knil(l, x): link(x, l) end
      self.fold-revpostorder(knil, empty)
    end,
    revpreorder(self) -> lists.List:
      doc: "Returns a list of all elements from a right-to-left preorder traversal"
      fun knil(l, x): link(x, l) end
      self.fold-preorder(knil, empty)
    end,
    revinorder(self) -> lists.List:
      doc: "Returns a list of all elements from a right-to-leftinorder traversal"
      fun knil(l, x): link(x, l) end
      self.fold-inorder(knil, empty)
    end,
    revpostorder(self) -> lists.List:
      doc: "Returns a list of all elements from a roght-to-left postorder traversal"
      fun knil(l, x): link(x, l) end
      self.fold-postorder(knil, empty)
    end,
    fold-preorder(self, f, base):
      doc: ```Folds the elements contained in the tree into a single value with f.
            analogous to folding a list, in a preorder traversal```
      self.right.fold-preorder(f, self.left.fold-preorder(f, f(base, self.value)))
    end,
    fold-inorder(self, f, base):
      doc: ```Folds the elements contained in the tree into a single value with f.
            analogous to folding a list, in an inorder traversal```
      self.right.fold-inorder(f, f(self.left.fold-inorder(f, base), self.value))
    end,
    fold-postorder(self, f, base):
      doc: ```Folds the elements contained in the tree into a single value with f.
            analogous to folding a list, in a postorder traversal```
      f(self.right.fold-postorder(f, self.left.fold-postorder(f, base)), self.value)
    end,
    fold-revpreorder(self, f, base):
      doc: ```Folds the elements contained in the tree into a single value with f.
            analogous to folding a list, in a right-to-left preorder traversal```
      self.left.fold-revpreorder(f, self.right.fold-revpreorder(f, f(base, self.value)))
    end,
    fold-revinorder(self, f, base):
      doc: ```Folds the elements contained in the tree into a single value with f.
            analogous to folding a list, in a right-to-left inorder traversal```
      self.left.fold-revinorder(f, f(self.right.fold-revinorder(f, base), self.value))
    end,
    fold-revpostorder(self, f, base):
      doc: ```Folds the elements contained in the tree into a single value with f.
            analogous to folding a list, in a right-to-left postorder traversal```
      f(self.left.fold-revpostorder(f, self.right.fold-revpostorder(f, base)), self.value)
    end,
    count(self): 1 + self.left.count() + self.right.count() end,
    all(self, f):
      f(self.value) and self.right.all(f) and self.left.all(f)
    end,
    any(self, f):
      f(self.value) or self.right.all(f) or self.left.all(f)
    end
sharing:
  to-list(self) -> lists.List:
    doc: "Returns a list of all elements from a inorder traversal"
    self.inorder()
  end,
  _equals(self, other, eq):
    if not(AVLTree(other)):
      equality.NotEqual("Non-AVLTree", self, other)
    else:
      eq(self.inorder(), other.inorder())
    end
  end
end

fun tree-fold(f, base, tree): tree.fold-preorder(f, base) end
fun tree-all(f, tree): tree.all(f) end
fun tree-any(f, tree): tree.any(f) end

fun mkbranch(val :: Any, left :: AVLTree, right :: AVLTree):
  branch(val, num-max(left.height(), right.height()) + 1, left, right)
end

fun rebalance(tree :: AVLTree):
  fun left-left(t):
    mkbranch(t.left.value, t.left.left, mkbranch(t.value, t.left.right, t.right))
  end
  fun right-right(t):
    mkbranch(t.right.value, mkbranch(t.value, t.left, t.right.left), t.right.right)
  end
  fun left-right(t):
    mkbranch(t.left.right.value,
      mkbranch(t.left.value, t.left.left, t.left.right.left),
      mkbranch(t.value, t.left.right.right, t.right))
  end
  fun right-left(t):
    mkbranch(t.right.left.value,
      mkbranch(t.value, t.left, t.right.left.left),
      mkbranch(t.right.value, t.right.left.right, t.right.right))
  end
  lh = tree.left.height()
  rh = tree.right.height()
  if num-abs(lh - rh) <= 1:
    tree
  else if (lh - rh) == 2:
    if tree.left.left.height() >= tree.left.right.height():
      left-left(tree)
    else:
      left-right(tree)
    end
  else if (rh - lh) == 2:
    if tree.right.right.height() >= tree.right.left.height():
      right-right(tree)
    else:
      right-left(tree)
    end
  else:
    raise("AVL tree invariant has been broken!")
  end
end

fun remove-root(tree :: AVLTree):
  if is-leaf(tree.left):
    if is-leaf(tree.right):
      leaf
    else:
      tree.right
    end
  else:
    if is-leaf(tree.right):
      tree.left
    else:
      swap-next-lowest(tree)
    end
  end
end

fun swap-next-lowest(tree :: AVLTree):
  fun greatest(t):
    cases(AVLTree) t:
      | leaf => raise("Went too far in traversal step")
      | branch(_, _, _, right) => if is-leaf(right): t else: greatest(right) end
    end
  end
  fun remove-greatest-and-rebalance(t):
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
  rebalance(mkbranch(greatest(tree.left).value,
      remove-greatest-and-rebalance(tree.left),
      tree.right))
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
  | list-set(elems :: lists.List) with:
    pick(self):
      lst = self.elems
      cases(List) lst:
        | empty => pick-none
        | link(f, r) =>
          cases(List) r:
            | empty => pick-some(f, list-set(empty))
            | link(f2, r2) =>
              get-first = random(2)
              if get-first == 0:
                pick-some(f, list-set(r))
              else:
                pick-some(f2, list-set(link(f, r2)))
              end
          end
      end
    end,
    _tostring(self, shadow tostring):
      "[set: " +
      self.elems.foldl(lam(elem, acc):
          if acc == "": tostring(elem)
          else: tostring(elem) + ", " + acc
          end
        end, "") +
      "]"
    end,
    _torepr(self, shadow torepr):
      "[list-set: " +
      self.elems.foldl(lam(elem, acc):
          if acc == "": torepr(elem)
          else: torepr(elem) + ", " + acc
          end
        end, "") +
      "]"
    end,
    _output(self): VS.vs-collection("list-set", self.to-list().map(VS.vs-value)) end,

    fold(self, f :: (Any, Any -> Any), base :: Any):
      fold(f, base, self.elems)
    end,

    member(self, elem :: Any) -> Boolean:
      doc: 'Check to see if an element is in a set.'
      self.elems.member(elem)
    end,

    add(self, elem :: Any) -> Set:
      doc: "Add an element to the set if it is not already present."
      if (self.elems.member(elem)):
        self
      else:
        list-set(link(elem, self.elems))
      end
    end,

    remove(self, elem :: Any) -> Set:
      doc: "Remove an element from the set if it is present."
      list-set(self.elems.remove(elem))
    end,

    to-list(self) -> lists.List:
      doc: 'Convert a set into a list of elements.'
      self.elems
    end,

    union(self :: Set, other :: Set) -> Set:
      doc: 'Compute the union of this set and another set.'
      other.fold(lam(u, elem):
        u.add(elem)
      end, self)
    end,

    intersect(self :: Set, other :: Set) -> Set:
      doc: 'Compute the intersection of this set and another set.'
      new-elems = for fold(elems from self.elems, elem from self.elems):
        if other.member(elem):
          elems
        else:
          elems.remove(elem)
        end
      end
      list-set(new-elems)
    end,

    overlaps(self :: Set, other :: Set) -> Boolean:
      doc: 'Determines if the intersection of this set and another set is non-empty.'
      self.any(other.member)
    end,
    
    difference(self :: Set, other :: Set) -> Set:
      doc: 'Compute the difference of this set and another set.'
      new-elems = for fold(elems from self.elems, elem from self.elems):
        if other.member(elem):
          elems.remove(elem)
        else:
          elems
        end
      end
      list-set(new-elems)
    end,

    size(self :: Set) -> Number:
      self.elems.length()
    end,

    is-empty(self): is-empty(self.elems) end,

    all(self, f) -> Boolean:
      self.elems.all(f)
    end,

    any(self, f) -> Boolean:
      self.elems.any(f)
    end
    
  | tree-set(elems :: AVLTree) with:
    pick(self):
      t = self.elems
      cases(AVLTree) t:
        | leaf => pick-none
        | branch(v, _, _, _) =>
          pick-some(v, tree-set(t.remove(v)))
      end
    end,
    _tostring(self, shadow tostring):
      "[tree-set: " +
      self.elems.fold(lam(acc, elem):
          if acc == "": tostring(elem)
          else: acc + ", " + tostring(elem)
          end
        end, "") +
      "]"
    end,
    _torepr(self, shadow torepr):
      "[tree-set: " +
      self.elems.fold(lam(acc, elem):
          if acc == "": torepr(elem)
          else: acc + ", " + torepr(elem)
          end
        end, "") +
      "]"
    end,
    _output(self): VS.vs-collection("tree-set", self.to-list().map(VS.vs-value)) end,

    fold(self, f :: (Any -> Any), base :: Any):
      tree-fold(f, base, self.elems)
    end,

    member(self, elem :: Any) -> Boolean:
      doc: 'Check to see if an element is in a set.'
      self.elems.contains(elem)
    end,

    add(self, elem :: Any) -> Set:
      doc: "Add an element to the set if it is not already present."
      tree-set(self.elems.insert(elem))
    end,

    remove(self, elem :: Any) -> Set:
      doc: "Remove an element from the set if it is present."
      tree-set(self.elems.remove(elem))
    end,

    to-list(self) -> lists.List:
      doc: 'Convert a set into a list of elements.'
      self.elems.inorder()
    end,

    union(self, other):
      doc: 'Compute the union of this set and another set.'
      tree-set-union(self, other)
    end,

    intersect(self, other):
      doc: 'Compute the intersection of this set and another set.'
      tree-set-intersect(self, other)
    end,

    overlaps(self :: Set, other :: Set) -> Boolean:
      doc: 'Determines if the intersection of this set and another set is non-empty.'
      self.any(other.member)
    end,

    difference(self :: Set, other :: Set) -> Set:
      doc: 'Compute the difference of this set and another set.'
      tree-set-difference(self, other)
    end,

    size(self :: Set) -> Number:
      self.elems.count()
    end,

    is-empty(self): is-leaf(self.elems) end,

    all(self, f) -> Boolean:
      self.elems.all(f)
    end,

    any(self, f) -> Boolean:
      self.elems.any(f)
    end
    
sharing:

  symmetric-difference(self :: Set, other :: Set) -> Set:
    doc: 'Compute the symmetric difference of this set and another set.'
    self.union(other).difference(self.intersect(other))
  end,

  _equals(self, other, eq):
    if not(Set(other)):
      equality.NotEqual("Non-Set", self, other)
    else:
      self-list = self.to-list()
      other-list = other.to-list()
      if not(other-list.length() == self-list.length()):
        equality.NotEqual("set size", self, other)
      else:
        for fold(result from equality.Equal, elt from self-list):
          result-for-elt = lists.member-with(other-list, elt, eq)
          equality.equal-and(result, result-for-elt)
        end
      end
    end
  end
end

fun set-to-sorted-elems(s):
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
    if l <= 0: leaf
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

fun list-to-set(lst :: lists.List, base-set :: Set) -> Set:
  doc: "Convert a list into a set."
  for lists.fold(s from base-set, elem from lst):
    s.add(elem)
  end
end

fun list-to-list-set(lst :: lists.List) -> Set:
  doc: "Convert a list into a list-based set."
  list-to-set(lst, list-set(empty))
end

fun list-to-tree-set(lst :: lists.List) -> Set:
  doc: "Convert a list into a tree-based set."
  list-to-set(lst, tree-set(leaf))
end

fun list-to-tree(lst :: lists.List):
  for lists.fold(tree from leaf, elt from lst):
    tree.insert(elt)
  end
end

fun arr-to-list-set(arr :: RawArray) -> Set:
  for raw-array-fold(ls from list-set(empty), elt from arr, _ from 0):
    ls.add(elt)
  end
end

fun arr-to-tree-set(arr :: RawArray) -> Set:
  tree = for raw-array-fold(t from leaf, elt from arr, _ from 0):
    t.insert(elt)
  end
  tree-set(tree)
end

empty-list-set = list-set(empty)
empty-tree-set = tree-set(leaf)


list-set-maker = {
  make: arr-to-list-set,
  make0: lam(): empty-list-set end,
  make1: lam(a): list-set(link(a, empty)) end,
  make2: lam(a, b): list-set(link(a, link(b, empty))) end,
  make3: lam(a, b, c): list-set(link(a, link(b, link(c, empty)))) end,
  make4: lam(a, b, c, d): list-set(link(a, link(b, link(c, link(d, empty))))) end,
  make5: lam(a, b, c, d, e): list-set(link(a, link(b, link(c, link(d, link(e, empty)))))) end
}

tree-set-maker = {
  make: arr-to-tree-set,
  make0: lam(): empty-tree-set end,
  make1: lam(a): empty-tree-set.add(a) end,
  make2: lam(a, b): empty-tree-set.add(a).add(b) end,
  make3: lam(a, b, c): empty-tree-set.add(a).add(b).add(c) end,
  make4: lam(a, b, c, d): empty-tree-set.add(a).add(b).add(c).add(d) end,
  make5: lam(a, b, c, d, e): empty-tree-set.add(a).add(b).add(c).add(d).add(e) end
}
