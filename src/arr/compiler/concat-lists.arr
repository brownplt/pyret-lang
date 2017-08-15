provide *
provide-types *

data ConcatList<a>:
  | concat-empty with:
    method to-list-acc(self, rest): rest end,
    method map-to-list-acc(self, f, rest): rest end,
    method map(self, f): self end,
    method each(self, f): nothing end,
    method foldl(self, f, base): base end,
    method foldr(self, f, base): base end,
    method is-empty(self): true end,
    method length(self): 0 end,
    method join-str(self, sep): "" end,
    method reverse(self): self end,
    method all(self, f): true end
  | concat-singleton(element) with:
    method to-list-acc(self, rest): link(self.element, rest) end,
    method map-to-list-acc(self, f, rest): link(f(self.element), rest) end,
    method map(self, f): concat-singleton(f(self.element)) end,
    method each(self, f) block:
      f(self.element)
      nothing
    end,
    method foldl(self, f, base): f(base, self.element) end,
    method foldr(self, f, base): f(base, self.element) end,
    method getFirst(self): self.element end,
    method getLast(self): self.element end,
    method is-empty(self): false end,
    method length(self): 1 end,
    method join-str(self, sep): tostring(self.element) end,
    method reverse(self): self end,
    method all(self, f): f(self.element) end
  | concat-append(left :: ConcatList<a>, right :: ConcatList<a>) with:
    method to-list-acc(self, rest :: List):
      self.left.to-list-acc(self.right.to-list-acc(rest))
    end,
    method map-to-list-acc(self, f, rest :: List):
      self.left.map-to-list-acc(f, self.right.map-to-list-acc(f, rest))
    end,
    method map(self, f): concat-append(self.left.map(f), self.right.map(f)) end,
    method each(self, f) block:
      self.left.each(f)
      self.right.each(f)
    end,
    method foldl(self, f, base): self.right.foldl(f, self.left.foldl(f, base)) end,
    method foldr(self, f, base): self.left.foldr(f, self.right.foldr(f, base)) end,
    method getFirst(self): if self.left.is-empty(): self.right.getFirst() else: self.left.getFirst() end end,
    method getLast(self): if self.right.is-empty(): self.left.getLast() else: self.right.getLast() end end,
    method is-empty(self): self.left.is-empty() and self.right.is-empty() end,
    method length(self): self.left.length() + self.right.length() end,
    method join-str(self, sep):
      l = self.left.join-str(sep)
      r = self.right.join-str(sep)
      if l == "": r
      else if r == "": l
      else: l + sep + r
      end
    end,
    method reverse(self): concat-append(self.right.reverse(), self.left.reverse()) end,
    method all(self, f): self.left.all(f) and self.right.all(f) end
  | concat-cons(first :: a, rest :: ConcatList<a>) with:
    method to-list-acc(self, rest): link(self.first, self.rest.to-list-acc(rest)) end,
    method map-to-list-acc(self, f, rest): link(f(self.first), self.rest.map-to-list-acc(f, rest)) end,
    method map(self, f): concat-cons(f(self.first), self.rest.map(f)) end,
    method each(self, f) block:
      f(self.first)
      self.rest.each(f)
    end,
    method foldl(self, f, base): self.rest.foldl(f, f(base, self.first)) end,
    method foldr(self, f, base): f(self.rest.foldr(f, base), self.first) end,
    method getFirst(self): self.first end,
    method getLast(self): if self.rest.is-empty(): self.first else: self.rest.getLast() end end,
    method is-empty(self): false end,
    method length(self): 1 + self.rest.length() end,
    method join-str(self, sep):
      l = tostring(self.first)
      r = self.rest.join-str(sep)
      if r == "": l
      else: l + sep + r
      end
    end,
    method reverse(self): concat-snoc(self.rest.reverse(), self.first) end,
    method all(self, f): f(self.first) and self.rest.all(f) end
  | concat-snoc(head :: ConcatList<a>, last :: a) with:
    method to-list-acc(self, rest): self.head.to-list-acc(link(self.last, rest)) end,
    method map-to-list-acc(self, f, rest): self.head.map-to-list-acc(f, link(f(self.last), rest)) end,
    method map(self, f): concat-snoc(self.head.map(f), f(self.last)) end,
    method each(self, f) block:
      self.head.each(f)
      f(self.last)
      nothing
    end,
    method foldl(self, f, base): f(self.head.foldl(f, base), self.last) end,
    method foldr(self, f, base): self.head.foldr(f, f(base, self.last)) end,
    method getFirst(self): if self.head.is-empty(): self.last else: self.head.getFirst() end end,
    method getLast(self): self.last end,
    method is-empty(self): false end,
    method length(self): self.head.length() + 1 end,
    method join-str(self, sep):
      h = self.head.join-str(sep)
      l = tostring(self.last)
      if h == "": l
      else: h + sep + l
      end
    end,
    method reverse(self): concat-cons(self.last, self.head.reverse()) end,
    method all(self, f): self.head.all(f) and f(self.last) end
sharing:
  method _plus(self, other :: ConcatList):
    if is-concat-empty(self): other
    else if is-concat-empty(other): self
    else: concat-append(self, other)
    end
  end,
  method to-list(self): self.to-list-acc(empty) end,
  method map-to-list-left(self, f): revmap-to-list-acc(self, f, empty).reverse() end,
  method map-to-list(self, f): self.map-to-list-acc(f, empty) end,
  method find(self, f): find(f, self) end
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
  l1.map-to-list-left(lam(e) block:
      aux := aux + tostring(e)
      tostring(e)
    end) is [list: "1", "2", "3", "4"]
  aux is "1234"
  aux := ""
  l1.map-to-list(lam(e) block:
      aux := aux + tostring(e)
      tostring(e)
    end) is [list: "1", "2", "3", "4"]
  aux is "3421"
end
fun revmap-to-list-acc(self, f, revhead):
  if is-concat-empty(self): revhead
  else if is-concat-singleton(self): link(f(self.element), revhead)
  else if is-concat-append(self): revmap-to-list-acc(self.right, f, revmap-to-list-acc(self.left, f, revhead))
  else if is-concat-cons(self): revmap-to-list-acc(self.rest, f, link(f(self.first), revhead))
  else if is-concat-snoc(self):
    newhead = revmap-to-list-acc(self.head, f, revhead)
    link(f(self.last), newhead) # order of operations matters
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

shadow foldl = lam(f, base, lst): lst.foldl(f, base) end
shadow foldr = lam(f, base, lst): lst.foldr(f, base) end
shadow map = lam(f, lst): lst.map(f) end
shadow each = lam(f, lst): lst.each(f) end

fun all<a>(f :: (a -> Boolean), lst :: ConcatList<a>) -> Boolean:
  lst.all(f)
end

clist = {
  make: lam(arr):
      for raw-array-fold(clst from concat-empty, elt from arr, _ from 0):
        concat-snoc(clst, elt)
      end
    end,
  make0: lam(): concat-empty end,
  make1: concat-singleton,
  make2: lam(a, b): concat-cons(a, concat-singleton(b)) end,
  make3: lam(a, b, c): concat-cons(a, concat-cons(b, concat-singleton(c))) end,
  make4: lam(a, b, c, d): concat-cons(a, concat-cons(b, concat-cons(c, concat-singleton(d)))) end,
  make5:
    lam(a, b, c, d, e): concat-cons(a, concat-cons(b, concat-cons(c, concat-cons(d, concat-singleton(e))))) end,
}

fun map_list_n<a, b>(f :: (Number, a -> b), n :: Number, lst :: List<a>) -> ConcatList<b>:
  doc: "Returns a catenable list made up of f(n, e1), f(n+1, e2) .. for e1, e2 ... in lst"
  if is-empty(lst):
    concat-empty
  else:
    concat-cons(f(n, lst.first), map_list_n(f, n + 1, lst.rest))
  end
end

shadow each_n = lam <a>(f :: (Number, a -> Nothing), n :: Number, lst :: ConcatList<a>) -> Nothing:
  var shadow n = n
  for each(item from lst) block:
    f(n, item)
    n := n + 1
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
  # [clist: 1, 2, 3] is concat-cons(1, concat-cons(2, concat-singleton(3)))
  nothing
end
