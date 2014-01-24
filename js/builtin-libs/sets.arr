#lang pyret/library

provide {
  Set: Set,
  set: list-to-tree-set,
  list-set: list-to-list-set,
  tree-set: list-to-tree-set,
} end

import list as list
import error as error
import option as option

List = list.List

# SETS

data AVLTree:
  | leaf with:
      height(self) -> Number: 0 end,
      contains(self, val :: Any) -> Bool: false end,
      insert(self, val :: Any) -> AVLTree: mkbranch(val, leaf, leaf) end,
      remove(self, val :: Any) -> AVLTree: leaf end,
      preorder(self) -> List: [] end,
      inorder(self) -> List: [] end,
      postorder(self) -> List: [] end
      
  | branch(value :: Any, h :: Number, left :: AVLTree, right :: AVLTree) with:
      height(self) -> Number: self.h end,
      contains(self, val :: Any) -> Bool:
        if val == self.value: true
        else if val < self.value: self.left.contains(val)
        else: self.right.contains(val)
        end
      end,
      insert(self, val :: Any) -> AVLTree:
        if val == self.value: mkbranch(val, self.left, self.right)
        else if val < self.value:
          rebalance(mkbranch(self.value, self.left.insert(val), self.right))
        else:
          rebalance(mkbranch(self.value, self.left, self.right.insert(val)))
        end
      end,
      remove(self, val :: Any) -> AVLTree:
        if val == self.value: remove-root(self)
        else if val < self.value:
          rebalance(mkbranch(self.value, self.left.remove(val), self.right))
        else:
          rebalance(mkbranch(self.value, self.left, self.right.remove(val)))
        end
      end,
      preorder(self) -> List: list.link(self.value, self.left.preorder() + self.right.preorder()) end,
      inorder(self) -> List: self.left.inorder() + list.link(self.value, self.right.inorder()) end,
      postorder(self) -> List: self.left.postorder() + self.right.postorder() + [self.value] end
sharing:
  to-list(self) -> List: self.inorder() end,
  _equals(self, other):
    AVLTree(other) and (self.inorder() == other.inorder())
  end
end

fun mkbranch(val :: Any, left :: AVLTree, right :: AVLTree):
  branch(val, left.height().max(right.height()) + 1, left, right)
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
  if (lh - rh).abs() <= 1:
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
  | list-set(elems :: List) with:

      member(self, elem :: Any) -> Bool:
        doc: 'Check to see if an element is in a set.'
        self.elems.member(elem)
      #where:
      #  sets.set([1, 2, 3]).member(2) is true
      #  sets.set([1, 2, 3]).member(4) is false
      end,

      add(self, elem :: Any) -> Set:
        doc: "Add an element to the set if it is not already present."
        if (self.elems.member(elem)):
          self
        else:
          list-set(list.link(elem, self.elems))
        end
      #where:
      #  sets.set([]).add(1) is sets.set([1])
      #  sets.set([1]).add(1) is sets.set([1])
      #  sets.set([1, 2, 3]).add(2) is sets.set([1, 2, 3])
      #  sets.set([1, 2, 3]).add(1.5) is sets.set([1, 2, 3, 1.5])
      end,

      remove(self, elem :: Any) -> Set:
        doc: "Remove an element from the set if it is present."
        list-set(self.elems.filter(fun (x): x <> elem end))
      #where:
      #  sets.set([1, 2]).remove(18) is sets.set([1, 2])
      #  sets.set([1, 2]).remove(2) is sets.set([1])
      end,

      to-list(self) -> List:
        doc: 'Convert a set into a list of elements.'
        self.elems
      #where:
      #  sets.set([3, 1, 2]).to-list() is [1, 2, 3]
      end

  | tree-set(elems :: AVLTree) with:

      member(self, elem :: Any) -> Bool:
        doc: 'Check to see if an element is in a set.'
        self.elems.contains(elem)
      #where:
      #  sets.tree-set([1, 2, 3]).member(2) is true
      #  sets.tree-set([1, 2, 3]).member(4) is false
      end,

      add(self, elem :: Any) -> Set:
        doc: "Add an element to the set if it is not already present."
        tree-set(self.elems.insert(elem))
      #where:
      #  sets.tree-set([]).add(1) is sets.tree-set([1])
      #  sets.tree-set([1]).add(1) is sets.tree-set([1])
      #  sets.tree-set([1, 2, 3]).add(2) is sets.tree-set([1, 2, 3])
      #  sets.tree-set([1, 2, 3]).add(1.5) is sets.tree-set([1, 2, 3, 1.5])
      end,

      remove(self, elem :: Any) -> Set:
        doc: "Remove an element from the set if it is present."
        tree-set(self.elems.remove(elem))
      #where:
      #  sets.tree-set([1, 2]).remove(18) is sets.tree-set([1, 2])
      #  sets.tree-set([1, 2]).remove(2) is sets.tree-set([1])
      end,

      to-list(self) -> List:
        doc: 'Convert a set into a list of elements.'
        self.elems.preorder()
      #where:
      #  sets.tree-set([3, 1, 2]).to-list() is [1, 2, 3]
      end

sharing:
  union(self :: Set, other :: Set) -> Set:
    doc: 'Compute the union of this set and another set.'
    for list.fold(u from self, elem from other.to-list()):
      u.add(elem)
    end
  end,

  intersect(self :: Set, other :: Set) -> Set:
    doc: 'Compute the intersection of this set and another set.'
    for list.fold(u from self, elem from self.to-list()):
      if other.member(elem):
        u
      else:
        u.remove(elem)
      end
    end
  end,

  difference(self :: Set, other :: Set) -> Set:
    doc: 'Compute the difference of this set and another set.'
    for list.fold(u from self, elem from self.to-list()):
      if other.member(elem):
        u.remove(elem)
      else:
        u
      end
    end
  end,

  symmetric_difference(self :: Set, other :: Set) -> Set:
    doc: 'Compute the symmetric difference of this set and another set.'
    self.union(other).difference(self.intersect(other))
  end,
    
  _equals(self, other):
    Set(other) and (self.to-list().sort() == other.to-list().sort())
  end

end

fun list-to-set(lst :: List, base-set :: Set) -> Set:
  doc: "Convert a list into a set."
  for list.fold(s from base-set, elem from lst):
    s.add(elem)
  end
end

fun list-to-list-set(lst :: List) -> Set:
  doc: "Convert a list into a list-based set."
  list-to-set(lst, list-set([]))
end

fun list-to-tree-set(lst :: List) -> Set:
  doc: "Convert a list into a tree-based set."
  list-to-set(lst, tree-set(leaf))
end

fun list-to-tree(lst :: List):
  for list.fold(tree from leaf, elt from lst):
    tree.insert(elt)
  end
end

