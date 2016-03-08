provide *
provide-types *

data ConcatList<a>:
  | concat-empty with:
    to-list-acc(self, rest): rest end,
    map-to-list-acc(self, f, rest): rest end,
    map(self, f): self end,
    each(self, f): nothing end,
    foldl(self, f, base): base end,
    foldr(self, f, base): base end,
    is-empty(self): true end,
    length(self): 0 end,
    join-str(self, sep): "" end,
    reverse(self): self end
  | concat-singleton(element) with:
    to-list-acc(self, rest): link(self.element, rest) end,
    map-to-list-acc(self, f, rest): link(f(self.element), rest) end,
    map(self, f): concat-singleton(f(self.element)) end,
    each(self, f):
      f(self.element)
      nothing
    end,
    foldl(self, f, base): f(base, self.element) end,
    foldr(self, f, base): f(base, self.element) end,
    getFirst(self): self.element end,
    getLast(self): self.element end,
    is-empty(self): false end,
    length(self): 1 end,
    join-str(self, sep): tostring(self.element) end,
    reverse(self): self end
  | concat-append(left :: ConcatList<a>, right :: ConcatList<a>) with:
    to-list-acc(self, rest :: List):
      self.left.to-list-acc(self.right.to-list-acc(rest))
    end,
    map-to-list-acc(self, f, rest :: List):
      self.left.map-to-list-acc(f, self.right.map-to-list-acc(f, rest))
    end,
    map(self, f): concat-append(self.left.map(f), self.right.map(f)) end,
    each(self, f):
      self.left.each(f)
      self.right.each(f)
    end,
    foldl(self, f, base): self.right.foldl(f, self.left.foldl(f, base)) end,
    foldr(self, f, base): self.left.foldr(f, self.right.foldr(f, base)) end,
    getFirst(self): if self.left.is-empty(): self.right.getFirst() else: self.left.getFirst() end end,
    getLast(self): if self.right.is-empty(): self.left.getLast() else: self.right.getLast() end end,
    is-empty(self): self.left.is-empty() and self.right.is-empty() end,
    length(self): self.left.length() + self.right.length() end,
    join-str(self, sep):
      l = self.left.join-str(sep)
      r = self.right.join-str(sep)
      if l == "": r
      else if r == "": l
      else: l + sep + r
      end
    end,
    reverse(self): concat-append(self.right.reverse(), self.left.reverse()) end
  | concat-cons(first :: a, rest :: ConcatList<a>) with:
    to-list-acc(self, rest): link(self.first, self.rest.to-list-acc(rest)) end,
    map-to-list-acc(self, f, rest): link(f(self.first), self.rest.map-to-list-acc(f, rest)) end,
    map(self, f): concat-cons(f(self.first), self.rest.map(f)) end,
    each(self, f):
      f(self.first)
      self.rest.each(f)
    end,
    foldl(self, f, base): self.rest.foldl(f, f(base, self.first)) end,
    foldr(self, f, base): f(self.rest.foldr(f, base), self.first) end,
    getFirst(self): self.first end,
    getLast(self): if self.rest.is-empty(): self.first else: self.rest.getLast() end end,
    is-empty(self): false end,
    length(self): 1 + self.rest.length() end,
    join-str(self, sep):
      l = tostring(self.first)
      r = self.rest.join-str(sep)
      if r == "": l
      else: l + sep + r
      end
    end,
    reverse(self): concat-snoc(self.rest.reverse(), self.first) end
  | concat-snoc(head :: ConcatList<a>, last :: a) with:
    to-list-acc(self, rest): self.head.to-list-acc(link(self.last, rest)) end,
    map-to-list-acc(self, f, rest): self.head.map-to-list-acc(f, link(f(self.last), rest)) end,
    map(self, f): concat-snoc(self.head.map(f), f(self.last)) end,
    each(self, f):
      self.head.each(f)
      f(self.last)
      nothing
    end,
    foldl(self, f, base): f(self.head.foldl(f, base), self.last) end,
    foldr(self, f, base): self.head.foldr(f, f(base, self.last)) end,
    getFirst(self): if self.head.is-empty(): self.last else: self.head.getFirst() end end,
    getLast(self): self.last end,
    is-empty(self): false end,
    length(self): self.head.length() + 1 end,
    join-str(self, sep):
      h = self.head.join-str(sep)
      l = tostring(self.last)
      if h == "": l
      else: h + sep + l
      end
    end,
    reverse(self): concat-cons(self.last, self.head.reverse()) end
sharing:
  _plus(self, other :: ConcatList):
    if is-concat-empty(self): other
    else if is-concat-empty(other): self
    else: concat-append(self, other)
    end
  end,
  to-list(self): self.to-list-acc(empty) end,
  map-to-list-left(self, f): revmap-to-list-acc(self, f, empty).reverse() end,
  map-to-list(self, f): self.map-to-list-acc(f, empty) end
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
  if is-concat-empty(self): revhead
  else if is-concat-singleton(self): link(f(self.element), revhead)
  else if is-concat-append(self): revmap-to-list-acc(self.right, f, revmap-to-list-acc(self.left, f, revhead))
  else if is-concat-cons(self): revmap-to-list-acc(self.rest, f, link(f(self.first), revhead))
  else if is-concat-snoc(self):
    newhead = revmap-to-list-acc(self.head, f, revhead)
    link(f(self.last), newhead) # order of operations matters
  end
end

shadow foldl = lam(f, base, lst): lst.foldl(f, base) end
shadow foldr = lam(f, base, lst): lst.foldr(f, base) end
shadow map = lam(f, lst): lst.map(f) end
shadow each = lam(f, lst): lst.each(f) end

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
