#lang pyret

# An implementation of binomial heaps.

provide {
  BinomialHeap: BinomialHeap,
  heap: list-to-heap,
  hempty: bh-empty,
  merge: merge,
  insert: insert,
  peek: peek,
  remove-min: remove-min,
  heap-to-list: heap-to-list
} end

data BinomialTree<a>:
  | bt-node(val :: a, children :: List<BinomialTree<a>>)
end

data TaggedTree<a>:
  | tt(order :: Number, tree :: BinomialTree<a>)
end

data BinomialHeap<a>:
  | bh-empty
  | bh-link(order :: Number, tree :: BinomialTree<a>, next :: BinomialHeap<a>)
sharing:
  merge(self, other): merge(self, other) end,
  insert(self, val): insert(self, val) end,
  peek(self): peek(self) end,
  remove-min(self): remove-min(self) end,

  to-list(self): heap-to-list(self) end,
  tostring(self): "heap(" + tostring(heap-to-list(self)) + ")" end,
  _torepr(self, shadow torepr): "heap(" + torepr(heap-to-list(self)) + ")" end,
  _equals(self, other, eq): eq(heap-to-list(self), heap-to-list(other)) end,
  _plus(self, other): merge(self, other) end
end

fun<a> merge(lbh :: BinomialHeap<a>, rbh :: BinomialHeap<a>) -> BinomialHeap<a>:
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
  merge(bh-empty, bh-link(0, bt-node(10, [list: ]), bh-empty))
    is bh-link(0, bt-node(10, [list: ]), bh-empty)
  merge(bh-link(0, bt-node(10, [list: ]), bh-empty), bh-empty)
    is bh-link(0, bt-node(10, [list: ]), bh-empty)
  merge(bh-link(0, bt-node(10, [list: ]), bh-empty),
        bh-link(0, bt-node(20, [list: ]), bh-empty))
    is bh-link(1, bt-node(10, [list: bt-node(20, [list: ])]), bh-empty)
  merge(bh-link(0, bt-node(20, [list: ]), bh-empty),
        bh-link(0, bt-node(10, [list: ]), bh-empty))
    is bh-link(1, bt-node(10, [list: bt-node(20, [list: ])]), bh-empty)
  merge(bh-link(0, bt-node(10, [list: ]), bh-empty),
        bh-link(1, bt-node(20, [list: bt-node(30, [list: ])]), bh-empty))
    is bh-link(0, bt-node(10, [list: ]),
               bh-link(1, bt-node(20, [list: bt-node(30, [list: ])]), bh-empty))
  merge(bh-link(1, bt-node(10, [list: bt-node(40, [list: ])]), bh-empty),
        bh-link(1, bt-node(20, [list: bt-node(30, [list: ])]), bh-empty))
    is bh-link(2, bt-node(10, [list: bt-node(20, [list: bt-node(30, [list: ])]),
                               bt-node(40, [list: ])]), bh-empty)
end

fun<a> insert(bh :: BinomialHeap<a>, val :: a) -> BinomialHeap<a>:
  merge(bh, bh-link(0, bt-node(val, [list: ]), bh-empty))
where:
  bh-empty ^ insert(_, 10) is bh-link(0, bt-node(10, [list: ]), bh-empty)
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15)
    is bh-link(0, bt-node(15, [list: ]),
               bh-link(1, bt-node(10, [list: bt-node(20, [list: ])]), bh-empty))
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15)
           ^ insert(_, 40) ^ insert(_, 30) ^ insert(_, 25)
    is bh-link(1, bt-node(25, [list: bt-node(30, [list: ])]),
               bh-link(2, bt-node(10, [list: bt-node(15, [list: bt-node(40, [list: ])]),
                                       bt-node(20, [list: ])]),
                       bh-empty))
end

fun<a> peek(bh :: BinomialHeap<a>):
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
  bh-empty ^ peek(_) raises "peek"
  bh-empty ^ insert(_, 10) ^ peek(_) is 10
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15) ^ peek(_) is 10
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15)
           ^ insert(_, 40) ^ insert(_, 30) ^ insert(_, 25) ^ peek(_)
    is 10
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15)
           ^ insert(_, 40) ^ insert(_, 30) ^ insert(_, 25)
           ^ insert(_, -5) ^ insert(_, 45) ^ insert(_, 0) ^ peek(_)
    is -5
end

fun<a> remove-min(bh :: BinomialHeap<a>) -> BinomialHeap<a>:
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
  bh-empty ^ remove-min(_) raises "remove-min"
  bh-empty ^ insert(_, 10) ^ remove-min(_) ^ peek(_) raises "peek"
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15) ^ remove-min(_) ^ peek(_) is 15
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15)
           ^ insert(_, 40) ^ insert(_, 30) ^ insert(_, 25)
           ^ remove-min(_) ^ peek(_)
    is 15
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15)
           ^ insert(_, 40) ^ insert(_, 30) ^ insert(_, 25)
           ^ insert(_, -5) ^ insert(_, 45) ^ insert(_, 0)
           ^ remove-min(_) ^ peek(_)
    is 0
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15)
           ^ remove-min(_) ^ remove-min(_)
           ^ insert(_, 40) ^ insert(_, 30) ^ insert(_, 25)
           ^ remove-min(_) ^ peek(_)
    is 25
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15)
           ^ remove-min(_)
           ^ insert(_, 40) ^ insert(_, 30) ^ insert(_, 25)
           ^ remove-min(_) ^ remove-min(_)
           ^ insert(_, -5) ^ insert(_, 45) ^ insert(_, 0)
           ^ remove-min(_) ^ remove-min(_) ^ peek(_)
    is 25
end

fun<a> heap-to-list(bh :: BinomialHeap<a>) -> List<a>:
  cases(BinomialHeap) bh:
    | bh-empty => empty
    | bh-link(_, _, _) => link(peek(bh), heap-to-list(remove-min(bh)))
  end
where:
  bh-empty ^ heap-to-list(_) is [list: ]
  bh-empty ^ insert(_, 10) ^ heap-to-list(_) is [list: 10]
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15) ^ heap-to-list(_)
    is [list: 10, 15, 20]
  bh-empty ^ insert(_, 10) ^ remove-min(_) ^ heap-to-list(_) is [list: ]
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15) ^ remove-min(_) ^ heap-to-list(_)
    is [list: 15, 20]
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15)
           ^ insert(_, 40) ^ insert(_, 30) ^ insert(_, 25)
           ^ remove-min(_) ^ heap-to-list(_)
    is [list: 15, 20, 25, 30, 40]
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15)
           ^ insert(_, 40) ^ insert(_, 30) ^ insert(_, 25)
           ^ insert(_, -5) ^ insert(_, 45) ^ insert(_, 0)
           ^ remove-min(_) ^ heap-to-list(_)
    is [list: 0, 10, 15, 20, 25, 30, 40, 45]
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15)
           ^ remove-min(_) ^ remove-min(_)
           ^ insert(_, 40) ^ insert(_, 30) ^ insert(_, 25)
           ^ remove-min(_) ^ heap-to-list(_)
    is [list: 25, 30, 40]
  bh-empty ^ insert(_, 10) ^ insert(_, 20) ^ insert(_, 15)
           ^ remove-min(_)
           ^ insert(_, 40) ^ insert(_, 30) ^ insert(_, 25)
           ^ remove-min(_) ^ remove-min(_)
           ^ insert(_, -5) ^ insert(_, 45) ^ insert(_, 0)
           ^ remove-min(_) ^ remove-min(_) ^ heap-to-list(_)
    is [list: 25, 30, 40, 45]
end

fun<a> list-to-heap(lst :: List<a>) -> BinomialHeap<a>:
  fold(insert, bh-empty, lst)
where:
  fun check-compose(l):
    l.sort() == (l ^ list-to-heap(_) ^ heap-to-list(_))
  end

  [list: ] satisfies check-compose
  [list: 1] satisfies check-compose
  [list: 1,2] satisfies check-compose
  [list: 2,1] satisfies check-compose
  [list: 4,2,3,1] satisfies check-compose
  [list: 1,5,1,3,4] satisfies check-compose
end
