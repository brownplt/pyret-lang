provide *
provide-types *

data ConcatList<a>:
  | concat-empty with:
    is-empty(self): true end,
    join-str(self, sep): "" end,
  | concat-singleton(element) with:
    getFirst(self): self.element end,
    getLast(self): self.element end,
    is-empty(self): false end,
    join-str(self, sep): tostring(self.element) end,
  | concat-append(left :: ConcatList<a>, right :: ConcatList<a>) with:
    getFirst(self): if self.left.is-empty(): self.right.getFirst() else: self.left.getFirst() end end,
    getLast(self): if self.right.is-empty(): self.left.getLast() else: self.right.getLast() end end,
    is-empty(self): self.left.is-empty() and self.right.is-empty() end,
    join-str(self, sep):
      l = self.left.join-str(sep)
      r = self.right.join-str(sep)
      if l == "": r
      else if r == "": l
      else: l + sep + r
      end
    end,
  | concat-cons(first :: a, rest :: ConcatList<a>) with:
    getFirst(self): self.first end,
    getLast(self): if self.rest.is-empty(): self.first else: self.rest.getLast() end end,
    is-empty(self): false end,
    join-str(self, sep):
      l = tostring(self.first)
      r = self.rest.join-str(sep)
      if r == "": l
      else: l + sep + r
      end
    end,
  | concat-snoc(head :: ConcatList<a>, last :: a) with:
    getFirst(self): if self.head.is-empty(): self.last else: self.head.getFirst() end end,
    getLast(self): self.last end,
    is-empty(self): false end,
    join-str(self, sep):
      h = self.head.join-str(sep)
      l = tostring(self.last)
      if h == "": l
      else: h + sep + l
      end
    end,
sharing:
  _plus(self, other :: ConcatList):
    if is-concat-empty(self): other
    else if is-concat-empty(other): self
    else: concat-append(self, other)
    end
  end,
  map-to-list-acc(self, f, acc): map-to-list-acc(self, f, acc) end,
  to-list-acc(self, acc): to-list-acc(self, acc) end,
  to-list(self): self.to-list-acc(empty) end,
  map-to-list-left(self, f): revmap-to-list-acc(self, f, empty).reverse() end,
  map-to-list(self, f): self.map-to-list-acc(f, empty) end,
  find(self, f): find(f, self) end,
  length(self): length(self) end,
  reverse(self): reverse(self) end,
  map(self, f): map(f, self) end,
  each(self, f): each(f, self) end,
  foldl(self, f, acc): foldl(f, acc, self) end,
  foldr(self, f, acc): foldr(f, acc, self) end
where:
  ce = concat-empty
  co = concat-singleton
  ca = concat-append
  cc = concat-cons
  cs = concat-snoc
  l1 = ca(cs(cc(1, ce), 2), cc(3, cs(ce, 4)))
  l1.foldl(lam(base, e): base + tostring(e * e) end, "B") is "B14916"
  l1.foldr(lam(base, e): tostring(e * e) + base end, "B") is "14916B"

  ca(ce,ce).is-empty() is true
  cc(1, ce).getFirst() is 1
  ca(ce, cc(1, ce)).getFirst() is 1
  ca(cs(ce, 1), ce).getLast() is 1

  var aux = ""
  l1.map-to-list-left(lam(e):
      aux := aux + tostring(e)
      tostring(e)
    end) is [list: "1", "2", "3", "4"]
  aux is "1234"
  aux := ""
  l1.map-to-list(lam(e):
      aux := aux + tostring(e)
      tostring(e)
    end) is [list: "1", "2", "3", "4"]
  aux is "3421"
end

fun revmap-to-list-acc(self, f, revhead):
  cases (ConcatList) self:
    | concat-empty => revhead
    | concat-singleton(e) => link(f(e), revhead)
    | concat-append(l1, l2) => revmap-to-list-acc(l2, f, revmap-to-list-acc(l1, f, revhead))
    | concat-cons(e, r) => revmap-to-list-acc(r, f, link(f(e), revhead))
    | concat-snoc(r, e) =>
      newhead = revmap-to-list-acc(r, f, revhead)
      link(f(e), newhead) # order of operations matters
  end
end

fun map-to-list-acc(self, f, acc):
  cases (ConcatList) self:
    | concat-empty => acc
    | concat-singleton(e) => link(f(e), acc)
    | concat-append(l1, l2) => map-to-list-acc(l1, f, map-to-list-acc(l2, f, acc))
    | concat-cons(e, r) => link(f(e), map-to-list-acc(r, f, acc))
    | concat-snoc(r, e) => map-to-list-acc(r, f, link(f(e), acc))
  end
end

fun to-list-acc(self, acc):
  cases (ConcatList) self:
    | concat-empty => acc
    | concat-singleton(e) => link(e, acc)
    | concat-append(l1, l2) => to-list-acc(l1, to-list-acc(l2, acc))
    | concat-cons(e, r) => link(e, to-list-acc(r, acc))
    | concat-snoc(r, e) => to-list-acc(r, link(e, acc))
  end
end

rec shadow length = lam(original-lst):
  fun helper(lst, acc):
    cases (ConcatList) lst:
      | concat-empty => acc
      | concat-singleton(_) => acc + 1
      | concat-append(l1, l2) => helper(l2, helper(l1, acc))
      | concat-cons(_, r) => helper(r, acc + 1)
      | concat-snoc(r, _) => helper(r, acc + 1)
    end
  end
  helper(original-lst, 0)
end

rec shadow reverse = foldl(lam(r, f): concat-cons(f, r) end, concat-empty, _)

rec shadow map = lam(f, lst):
  cases (ConcatList) lst:
    | concat-empty => concat-empty
    | concat-singleton(e) => concat-singleton(f(e))
    | concat-append(l1, l2) => concat-append(map(f, l1), map(f, l2))
    | concat-cons(e, r) => concat-cons(f(e), map(f, r))
    | concat-snoc(r, e) => concat-snoc(map(f, r), f(e))
  end
end

rec shadow foldl = lam(f, base, lst):
  cases (ConcatList) lst:
    | concat-empty => base
    | concat-singleton(e) => f(base, e)
    | concat-append(l1, l2) => foldl(f, foldl(f, base, l1), l2)
    | concat-cons(e, r) => foldl(f, f(base, e), r)
    | concat-snoc(r, e) => f(foldl(f, base, r), e)
  end
end

rec shadow foldr = lam(f, base, lst):
  cases (ConcatList) lst:
    | concat-empty => base
    | concat-singleton(e) => f(base, e)
    | concat-append(l1, l2) => foldr(f, foldr(f, base, l2), l1)
    | concat-cons(e, r) => f(foldr(f, base, r), e)
    | concat-snoc(r, e) => foldr(f, f(base, e), r)
  end
end

rec shadow each = lam(f, lst):
  cases (ConcatList) lst:
    | concat-empty => nothing
    | concat-singleton(e) =>
      f(e)
      nothing
    | concat-append(l1, l2) =>
      each(f, l1)
      each(f, l2)
    | concat-cons(e, r) =>
      f(e)
      each(f, r)
    | concat-snoc(r, e) =>
      each(f, r)
      f(e)
      nothing
  end
end

rec shadow find = lam<a>(f :: (a -> Boolean), l :: ConcatList<a>) -> Option<a>:
  doc: "Takes a predicate and returns on option containing either the first item in this list that passes the predicate, or none"
  cases (ConcatList) l:
    | concat-empty => none
    | concat-singleton(e) => if f(e): some(e) else: none end
    | concat-append(l1, l2) =>
      result-left = find(f, l1)
      if is-none(result-left):
        find(f, l2)
      else:
        result-left
      end
    | concat-cons(e, r) => if f(e): some(e) else: find(f, r) end
    | concat-snoc(r, e) =>
      result-left = find(f, r)
      if is-none(result-left):
        if f(e): some(e) else: none end
      else:
        result-left
      end
  end
end

clist = {
  make: lam(arr):
      for raw-array-fold(clst from concat-empty, elt from arr, _ from 0):
        concat-snoc(clst, elt)
      end
    end
}

fun map_list_n<a, b>(f :: (Number, a -> b), n :: Number, lst :: List<a>) -> ConcatList<b>:
  doc: "Returns a catenable list made up of f(n, e1), f(n+1, e2) .. for e1, e2 ... in lst"
  if is-empty(lst):
    concat-empty
  else:
    concat-cons(f(n, lst.first), map_list_n(f, n + 1, lst.rest))
  end
end

fun map_list<a, b>(f :: (a -> b), lst :: List<a>) -> ConcatList<b>:
  if is-empty(lst):
    concat-empty
  else:
    concat-cons(f(lst.first), map_list(f, lst.rest))
  end
end

fun map_list2<a, b, c>(f :: (a, b -> c), l1 :: List<a>, l2 :: List<b>) -> ConcatList<c>:
  doc: "Returns a catenable list made up of f(elem1, elem2) for each elem1 in l1, elem2 in l2"
  if is-empty(l1) or is-empty(l2):
    concat-empty
  else:
    concat-cons(f(l1.first, l2.first), map_list2(f, l1.rest, l2.rest))
  end
end

check:
  [clist: 1, 2, 3] is concat-snoc(concat-snoc(concat-snoc(concat-empty, 1), 2), 3)
  [clist: 1, 2, 3].reverse().to-list() is [list: 3, 2, 1]

  [clist: 1, 2, 3].find(_ == 4) is none
  [clist: 1, 2, 3].find(_ == 2) is some(2)

  e1 = some(1)
  e2 = some(2)
  e3 = some(2)
  e4 = some(1)
  concat-append(
    concat-append(
      concat-snoc(concat-empty, e1),
      concat-cons(e2, concat-cons(e3, concat-empty))),
    concat-snoc(concat-empty, e4)).find(_ == some(2)).value is<=> e2
end
