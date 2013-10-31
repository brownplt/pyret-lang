#lang pyret

# SETS

provide {
  Set: Set,
  set: list-to-tree-set,
  list-set: list-to-list-set,
  tree-set: list-to-tree-set
} end


data Set:
  | list-set(elems :: List) with:

      member(self, elem :: Any) -> Bool:
        doc: 'Check to see if an element is in a set.'
        self.elems.member(elem)
      where:
        sets.set([1, 2, 3]).member(2) is true
        sets.set([1, 2, 3]).member(4) is false
      end,

      add(self, elem :: Any) -> Set:
        doc: "Add an element to the set if it is not already present."
        if (self.elems.member(elem)):
          self
        else:
          list-set(link(elem, self.elems))
        end
      where:
        sets.set([]).add(1) is sets.set([1])
        sets.set([1]).add(1) is sets.set([1])
        sets.set([1, 2, 3]).add(2) is sets.set([1, 2, 3])
        sets.set([1, 2, 3]).add(1.5) is sets.set([1, 2, 3, 1.5])
      end,

      remove(self, elem :: Any) -> Set:
        doc: "Remove an element from the set if it is present."
        list-set(self.elems.filter(fun (x): x <> elem end))
      where:
        sets.set([1, 2]).remove(18) is sets.set([1, 2])
        sets.set([1, 2]).remove(2) is sets.set([1])
      end,

      to-list(self) -> List:
        doc: 'Convert a set into a sorted list of elements.'
        self.elems.sort()
      where:
        sets.set([3, 1, 2]).to-list() is [1, 2, 3]
      end

  | tree-set(elems :: AVLTree) with:

      member(self, elem :: Any) -> Bool:
        doc: 'Check to see if an element is in a set.'
        self.elems.contains(elem)
      where:
        sets.tree-set([1, 2, 3]).member(2) is true
        sets.tree-set([1, 2, 3]).member(4) is false
      end,

      add(self, elem :: Any) -> Set:
        doc: "Add an element to the set if it is not already present."
        tree-set(self.elems.insert(elem))
      where:
        sets.tree-set([]).add(1) is sets.tree-set([1])
        sets.tree-set([1]).add(1) is sets.tree-set([1])
        sets.tree-set([1, 2, 3]).add(2) is sets.tree-set([1, 2, 3])
        sets.tree-set([1, 2, 3]).add(1.5) is sets.tree-set([1, 2, 3, 1.5])
      end,

      remove(self, elem :: Any) -> Set:
        doc: "Remove an element from the set if it is present."
        tree-set(self.elems.remove(elem))
      where:
        sets.tree-set([1, 2]).remove(18) is sets.tree-set([1, 2])
        sets.tree-set([1, 2]).remove(2) is sets.tree-set([1])
      end,

      to-list(self) -> List:
        doc: 'Convert a set into a sorted list of elements.'
        self.elems.to-list()
      where:
        sets.tree-set([3, 1, 2]).to-list() is [1, 2, 3]
      end

sharing:
  union(self :: Set, other :: Set) -> Set:
    doc: 'Compute the union of this set and another set.'
    for fold(u from self, elem from other.to-list()):
      u.add(elem)
    end
  end,

  intersect(self :: Set, other :: Set) -> Set:
    doc: 'Compute the intersection of this set and another set.'
    for fold(u from self, elem from self.to-list()):
      if other.member(elem):
        u
      else:
        u.remove(elem)
      end
    end
  end,

  difference(self :: Set, other :: Set) -> Set:
    doc: 'Compute the difference of this set and another set.'
    for fold(u from self, elem from self.to-list()):
      if other.member(elem):
        u.remove(elem)
      else:
        u
      end
    end
  end,
    
  _equals(self, other):
    Set(other) and (self.to-list() == other.to-list())
  end

end

fun list-to-set(lst :: List, base-set :: Set) -> Set:
  doc: "Convert a list into a set."
  for fold(s from base-set, elem from lst):
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
  for fold(tree from leaf, elt from lst):
    tree.insert(elt)
  end
end

