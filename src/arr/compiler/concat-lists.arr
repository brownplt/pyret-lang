provide *
provide-types *

data ConcatList<a>:
  | concat-empty with:
    to-list-acc(self, rest): rest end,
    map(self, f): self end,
    each(self, f): nothing end,
    foldl(self, f, base): base end,
    foldr(self, f, base): base end
  | concat-singleton(element) with:
    to-list-acc(self, rest): link(self.element, rest) end,
    map(self, f): concat-singleton(f(self.element)) end,
    each(self, f):
      f(self.element)
      nothing
    end,
    foldl(self, f, base): f(base, self.element) end,
    foldr(self, f, base): f(self.element, base) end
  | concat-append(left :: ConcatList<a>, right :: ConcatList<a>) with:
    to-list-acc(self, rest :: List):
      self.left.to-list-acc(self.right.to-list-acc(rest))
    end,
    map(self, f): concat-append(self.left.map(f), self.right.map(f)) end,
    each(self, f):
      self.left.each(f)
      self.right.each(f)
    end,
    foldl(self, f, base): self.right.foldl(f, self.left.foldl(f, base)) end,
    foldr(self, f, base): self.left.foldr(f, self.right.foldr(f, base)) end
  | concat-cons(first :: a, rest :: ConcatList<a>) with:
    to-list-acc(self, rest): link(self.first, self.rest.to-list-acc(rest)) end,
    map(self, f): concat-cons(f(self.first), self.rest.map(f)) end,
    each(self, f):
      f(self.first)
      self.rest.each(f)
    end,
    foldl(self, f, base): self.rest.foldl(f, f(base, self.first)) end,
    foldr(self, f, base): f(self.first, self.rest.foldr(f, base)) end
  | concat-snoc(head :: ConcatList<a>, last :: a) with:
    to-list-acc(self, rest): self.head.to-list-acc(link(self.last, rest)) end,
    map(self, f): concat-snoc(self.head.map(f), f(self.last)) end,
    each(self, f):
      self.head.each(f)
      f(self.last)
      nothing
    end,
    foldl(self, f, base): f(self.head.foldl(f, base), self.last) end,
    foldr(self, f, base): self.head.foldr(f, f(self.last, base)) end
sharing:
  _plus(self, other :: ConcatList):
    concat-append(self, other)
  end,
  to-list(self): self.to-list-acc([list: ]) end
where:
  ce = concat-empty
  co = concat-singleton
  ca = concat-append
  cc = concat-cons
  cs = concat-snoc
  l1 = ca(cs(cc(1, ce), 2), cc(3, cs(ce, 4)))
  l1.foldl(lam(base, e): base + tostring(e * e) end, "B") is "B14916"
  l1.foldr(lam(e, base): tostring(e * e) + base end, "B") is "14916B"
end
fun concat-foldl(f, base, lst): lst.foldl(f, base) end
fun concat-foldr(f, base, lst): lst.foldr(f, base) end
