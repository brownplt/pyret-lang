provide *
provide-types *

# See: https://en.wikipedia.org/wiki/Pairing_heap
data PairingHeap<A>:
  | ph-empty with:
    method find-min(self):
      none
    end,
    method merge(self, other):
      other
    end,
    method insert(self, elem):
      ph-node(elem, [list:])
    end,
    method delete-min(self):
      raise("Cannot delete minimum of empty heap")
    end,
    method size(self):
      0
    end
  | ph-node(elem :: A, subheaps :: List<PairingHeap<A>>) with:
    method find-min(self):
      some(self.elem)
    end,
    method merge(self, other):
      cases(PairingHeap) other:
        | ph-empty => self
        | ph-node(other-elem, other-subheaps) =>
          if self.elem < other.elem:
            ph-node(self.elem, link(other, self.subheaps))
          else:
            ph-node(other-elem, link(self, other-subheaps))
          end
      end
    end,
    method insert(self, elem):
      ph-node(elem, [list:]).merge(self)
    end,
    method delete-min(self):
      fun merge-pairs(l):
        cases(List) l:
          | empty => ph-empty
          | link(fst, rst) =>
            cases(List) rst:
              | empty => fst
              | link(rst-fst, rst-rst) =>
                fst.merge(rst-fst).merge(merge-pairs(rst-rst))
            end
        end
      end
      merge-pairs(self.subheaps)
    end,
    method size(self):
      map(_.size(), self.subheaps).foldl(_ + _, 1)
    end
end

data MutablePairingHeap<A>:
  | mut-ph(ref contents :: PairingHeap<A>)
    with:
    method find-min(self):
      self!contents.find-min()
    end,
    method merge(self, other):
      if is-MutablePairingHeap(other):
        self!{contents : self!contents.merge(other!contents)}
      else if is-PairingHeap(other):
        self!{contents : self!contents.merge(other)}
      else:
        raise("Cannot merge with non-heap")
      end
    end,
    method insert(self, elem):
      self!{contents : self!contents.insert(elem)}
    end,
    method delete-min(self):
      self!{contents : self!contents.delete-min()}
    end,
    method size(self):
      self!contents.size()
    end
end

make-mutable-pairing-heap = lam(): mut-ph(ph-empty) end

data MutableStack<A>:
    # INVARIANT: contents.length() == _size
  | mut-stack(ref contents :: List<A>, ref _size :: Number)
    with:
    method size(self):
      self!_size
    end,
    method push(self, elt) block:
      self!{_size : self!_size + 1}
      self!{contents : link(elt, self!contents)}
    end,
    method pop(self):
      if self!_size == 0 block:
        none
      else:
        self!{_size : self!_size - 1}
        ret = self!contents.first
        self!{contents : self!contents.rest}
        some(ret)
      end
    end,
    method to-list(self):
      self!contents
    end
end

make-mutable-stack = lam(): mut-stack([list:], 0) end
