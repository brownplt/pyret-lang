#lang pyret/library

provide {
  set: {
    make: arr-to-list-set
  },
  list-set: {
    make: arr-to-list-set
  },
  tree-set: {
    make: arr-to-tree-set
  },
  empty-set: list-set(empty),
  empty-list-set: list-set(empty),
  empty-tree-set: tree-set(leaf),
  list-to-set: list-to-list-set,
  list-to-list-set: list-to-list-set,
  list-to-tree-set: list-to-tree-set
} end
provide-types *

import pick as pick
import lists as lists
import error as error
import option as option
import arrays as arrays
import equality as equality

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
    fold(self, f, base): base end,
    count(self): 0 end
    
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
      doc: "Returns a list of all elements from a preorder traversal"
      link(self.value, self.left.preorder() + self.right.preorder())
    end,
    inorder(self) -> lists.List:
      doc: "Returns a list of all elements from a inorder traversal"
      self.left.inorder() + link(self.value, self.right.inorder())
    end,
    postorder(self) -> lists.List:
      doc: "Returns a list of all elements from a postorder traversal"
      self.left.postorder() + self.right.postorder() + link(self.value, empty)
    end,
    fold(self, f, base):
      doc: ```Folds the elements contained in the tree into a single value with f.
            analogous to folding a list```
      self.right.fold(f, self.left.fold(f, f(base, self.value)))
    end,
    count(self): 1 + self.left.count() + self.right.count() end
sharing:
  to-list(self) -> lists.List:
    doc: "Returns a list of all elements from a inorder traversal"
    self.inorder()
  end,
  _equals(self, other, eq):
    if not(AVLTree(other)):
      equality.NotEqual("Non-AVLTree")
    else:
      eq(self.inorder(), other.inorder())
    end
  end
end

fun tree-fold(f, base, tree): tree.fold(f, base) end


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
    tostring(self, shadow tostring):
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
      list-set(self.elems.filter(lam(x): x <> elem end))
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
      for fold(u from self, elem from self.elems):
        if other.member(elem):
          u
        else:
          u.remove(elem)
        end
      end
    end,
    
    difference(self :: Set, other :: Set) -> Set:
      doc: 'Compute the difference of this set and another set.'
      for fold(u from self, elem from self.elems):
        if other.member(elem):
          u.remove(elem)
        else:
          u
        end
      end
    end,
    
    size(self :: Set) -> Number:
      self.elems.length() 
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
    tostring(self, shadow tostring):
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
      new-elems =
        other.fold(lam(elems, elem):
          elems.insert(elem)
        end, self.elems)
      tree-set(new-elems)
    end,
    
    intersect(self, other):
      new-elems =
        for tree-fold(elems from self.elems, elem from self.elems):
          if other.member(elem):
            elems
          else: 
            elems.remove(elem)
          end
        end
      tree-set(new-elems)
    end,
    
    difference(self :: Set, other :: Set) -> Set:
      doc: 'Compute the difference of this set and another set.'
      new-elems = other.fold(lam(elems, elem):
        if self.member(elem):
          elems.remove(elem)
        else:
          elems
        end
      end, self.elems)
      tree-set(new-elems)
    end,

    size(self :: Set) -> Number:
      self.elems.count() 
    end

sharing:
  
  symmetric_difference(self :: Set, other :: Set) -> Set:
    doc: 'Compute the symmetric difference of this set and another set.'
    self.union(other).difference(self.intersect(other))
  end,
  
  _equals(self, other, eq):
    if not(Set(other)):
      equality.NotEqual("Non-Set")
    else:
      self-list = self.to-list()
      other-list = other.to-list()
      if not(other-list.length() == self-list.length()):
        equality.NotEqual("set size")
      else:
        for fold(result from equality.Equal, elt from self-list):
          result-for-elt = lists.member-with(other-list, elt, eq)
          equality.equal-and(result, result-for-elt)
        end
      end
    end
  end
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


