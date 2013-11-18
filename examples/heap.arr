#lang pyret

# An implementation of binomial heaps.

provide {
  BinomialHeap: BinomialHeap,
  merge: merge,
  insert: insert,
  peek: peek,
  remove-min: remove-min,
  heap-to-list: heap-to-list
} end

data BinomialTree:
  | bt-node(val :: Any, children :: List<BinomialTree>)
end

data TaggedTree:
  | tt(order :: Number, tree :: BinomialTree)
end

data BinomialHeap:
  | bh-empty
  | bh-link(order :: Number, tree :: BinomialTree, next :: BinomialHeap)
sharing:
  merge(self, other): merge(self, other) end,
  insert(self, val): insert(self, val) end,
  peek(self): peek(self) end,
  remove-min(self): remove-min(self) end,

  to-list(self): heap-to-list(self) end,
  tostring(self): "heap {" + tostring(heap-to-list(self)) + "}" end,
  torepr(self): "heap {" + torepr(heap-to-list(self)) + "}" end,
  _equals(self, other): heap-to-list(self) == heap-to-list(other) end,
  _plus(self, other): merge(self, other) end
end

fun merge(lbh :: BinomialHeap, rbh :: BinomialHeap) -> BinomialHeap:
  doc: "Merge two binomial heaps"
  fun merge-same-size(l, r):
    if l.val < r.val:
      bt-node(l.val, link(r, l.children))
    else:
      bt-node(r.val, link(l, r.children))
    end
  end
  fun merge-without-carry(l, r):
    cases(BinomialHeap) l:
      | bh-empty => r
      | bh-link(lorder, ltree, lnext) =>
        cases(BinomialHeap) r:
          | bh-empty => l
          | bh-link(rorder, rtree, rnext) =>
            if lorder < rorder:
              bh-link(lorder, ltree, merge-without-carry(lnext, r))
            else if lorder > rorder:
              bh-link(rorder, rtree, merge-without-carry(l, rnext))
            else:
              merge-with-carry(lnext, rnext, tt(lorder + 1, merge-same-size(ltree, rtree)))
            end
        end
    end
  end
  fun merge-with-carry(l, r, c):
    cases(BinomialHeap) l:
      | bh-empty => merge-without-carry(bh-link(c.order, c.tree, bh-empty), r)
      | bh-link(lorder, ltree, lnext) =>
        cases(BinomialHeap) r:
          | bh-empty => merge-without-carry(l, bh-link(c.order, c.tree, bh-empty))
          | bh-link(rorder, rtree, rnext) =>
            when (c.order > lorder) or (c.order > rorder):
              raise("Carry order too high in merge!")
            end
            if lorder < rorder:
              if c.order < lorder:
                bh-link(c.order, c.tree, bh-link(lorder, ltree, merge-without-carry(lnext, r)))
              else:
                merge-with-carry(lnext, r, tt(lorder + 1, merge-same-size(c.tree, ltree)))
              end
            else if lorder > rorder:
              if c.order < rorder:
                bh-link(c.order, c.tree, bh-link(rorder, rtree, merge-without-carry(l, rnext)))
              else:
                merge-with-carry(l, rnext, tt(rorder + 1, merge-same-size(c.tree, rtree)))
              end
            else:
              bh-link(c.order, c.tree,
                      merge-with-carry(lnext, rnext, tt(lorder + 1, merge-same-size(ltree, rtree))))
            end
        end
    end
  end
  merge-without-carry(lbh, rbh)
where:
  merge(bh-empty, bh-empty) is bh-empty
  merge(bh-empty, bh-link(0, bt-node(10, []), bh-empty))
    is bh-link(0, bt-node(10, []), bh-empty)
  merge(bh-link(0, bt-node(10, []), bh-empty), bh-empty)
    is bh-link(0, bt-node(10, []), bh-empty)
  merge(bh-link(0, bt-node(10, []), bh-empty),
        bh-link(0, bt-node(20, []), bh-empty))
    is bh-link(1, bt-node(10, [bt-node(20, [])]), bh-empty)
  merge(bh-link(0, bt-node(20, []), bh-empty),
        bh-link(0, bt-node(10, []), bh-empty))
    is bh-link(1, bt-node(10, [bt-node(20, [])]), bh-empty)
  merge(bh-link(0, bt-node(10, []), bh-empty),
        bh-link(1, bt-node(20, [bt-node(30, [])]), bh-empty))
    is bh-link(0, bt-node(10, []),
               bh-link(1, bt-node(20, [bt-node(30, [])]), bh-empty))
  merge(bh-link(1, bt-node(10, [bt-node(40, [])]), bh-empty),
        bh-link(1, bt-node(20, [bt-node(30, [])]), bh-empty))
    is bh-link(2, bt-node(10, [bt-node(20, [bt-node(30, [])]),
                               bt-node(40, [])]), bh-empty)
end

fun insert(bh :: BinomialHeap, val) -> BinomialHeap:
  merge(bh, bh-link(0, bt-node(val, []), bh-empty))
where:
  bh-empty^insert(10) is bh-link(0, bt-node(10, []), bh-empty)
  bh-empty^insert(10)^insert(20)^insert(15)
    is bh-link(0, bt-node(15, []),
               bh-link(1, bt-node(10, [bt-node(20, [])]), bh-empty))
  bh-empty^insert(10)^insert(20)^insert(15)
          ^insert(40)^insert(30)^insert(25)
    is bh-link(1, bt-node(25, [bt-node(30, [])]),
               bh-link(2, bt-node(10, [bt-node(15, [bt-node(40, [])]),
                                       bt-node(20, [])]),
                       bh-empty))
end

fun peek(bh :: BinomialHeap):
  fun peek-tree(h, min):
    cases(BinomialHeap) h:
      | bh-empty => min
      | bh-link(_, tree, next) =>
        peek-tree(next, if tree.val < min: tree.val else: min end)
    end
  end
  cases(BinomialHeap) bh:
    | bh-empty => raise("peek on empty heap")
    | bh-link(_, tree, next) => peek-tree(next, tree.val)
  end
where:
  bh-empty^peek() raises "peek"
  bh-empty^insert(10)^peek() is 10
  bh-empty^insert(10)^insert(20)^insert(15)^peek() is 10
  bh-empty^insert(10)^insert(20)^insert(15)
          ^insert(40)^insert(30)^insert(25)^peek()
    is 10
  bh-empty^insert(10)^insert(20)^insert(15)
          ^insert(40)^insert(30)^insert(25)
          ^insert(-5)^insert(45)^insert(0)^peek()
    is -5
end

fun remove-min(bh :: BinomialHeap) -> BinomialHeap:
  fun find-tree(h, min-h):
    cases(BinomialHeap) h:
      | bh-empty => min-h
      | bh-link(_, tree, next) =>
        find-tree(next, if tree.val < min-h.tree.val: h else: min-h end)
    end
  end
  fun remove-tree(h, target-ord):
    cases(BinomialHeap) h:
      | bh-empty => bh-empty
      | bh-link(order, tree, next) =>
        if target-ord == order:
          next
        else:
          bh-link(order, tree, remove-tree(next, target-ord))
        end
    end
  end
  cases(BinomialHeap) bh:
    | bh-empty => raise("remove-min on empty heap")
    | bh-link(_, _, _) =>
      least-link = find-tree(bh, bh)
      if least-link.order == 0:
        least-link.next
      else:
        without-least = remove-tree(bh, least-link.order)
        new-heap = for fold2(h from bh-empty,
                             t from least-link.tree.children,
                             o from range-by(least-link.order - 1, -1, -1)):
          bh-link(o, t, h)
        end
        merge(new-heap, without-least)
      end
  end
where:
  bh-empty^remove-min() raises "remove-min"
  bh-empty^insert(10)^remove-min()^peek() raises "peek"
  bh-empty^insert(10)^insert(20)^insert(15)^remove-min()^peek() is 15
  bh-empty^insert(10)^insert(20)^insert(15)
          ^insert(40)^insert(30)^insert(25)
          ^remove-min()^peek()
    is 15
  bh-empty^insert(10)^insert(20)^insert(15)
          ^insert(40)^insert(30)^insert(25)
          ^insert(-5)^insert(45)^insert(0)
          ^remove-min()^peek()
    is 0
  bh-empty^insert(10)^insert(20)^insert(15)
          ^remove-min()^remove-min()
          ^insert(40)^insert(30)^insert(25)
          ^remove-min()^peek()
    is 25
  bh-empty^insert(10)^insert(20)^insert(15)
          ^remove-min()
          ^insert(40)^insert(30)^insert(25)
          ^remove-min()^remove-min()
          ^insert(-5)^insert(45)^insert(0)
          ^remove-min()^remove-min()^peek()
    is 25
end

fun heap-to-list(bh :: BinomialHeap) -> List:
  cases(BinomialHeap) bh:
    | bh-empty => empty
    | bh-link(_, _, _) => link(peek(bh), heap-to-list(remove-min(bh)))
  end
where:
  bh-empty^heap-to-list() is []
  bh-empty^insert(10)^heap-to-list() is [10]
  bh-empty^insert(10)^insert(20)^insert(15)^heap-to-list()
    is [10, 15, 20]
  bh-empty^insert(10)^remove-min()^heap-to-list() is []
  bh-empty^insert(10)^insert(20)^insert(15)^remove-min()^heap-to-list()
    is [15, 20]
  bh-empty^insert(10)^insert(20)^insert(15)
          ^insert(40)^insert(30)^insert(25)
          ^remove-min()^heap-to-list()
    is [15, 20, 25, 30, 40]
  bh-empty^insert(10)^insert(20)^insert(15)
          ^insert(40)^insert(30)^insert(25)
          ^insert(-5)^insert(45)^insert(0)
          ^remove-min()^heap-to-list()
    is [0, 10, 15, 20, 25, 30, 40, 45]
  bh-empty^insert(10)^insert(20)^insert(15)
          ^remove-min()^remove-min()
          ^insert(40)^insert(30)^insert(25)
          ^remove-min()^heap-to-list()
    is [25, 30, 40]
  bh-empty^insert(10)^insert(20)^insert(15)
          ^remove-min()
          ^insert(40)^insert(30)^insert(25)
          ^remove-min()^remove-min()
          ^insert(-5)^insert(45)^insert(0)
          ^remove-min()^remove-min()^heap-to-list()
    is [25, 30, 40, 45]
end
