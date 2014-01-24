#lang pyret/library

provide *
import option as O

none = O.none
is-none = O.is-none
some = O.some
is-some = O.is-some
Option = O.Option

data List:
  | empty with:

    length(self) -> Number: 0 end,

    each(self, f :: (Any -> Nothing)) -> Nothing: nothing end,

    map(self, f :: (Any -> Any)) -> List: empty end,

    filter(self, f :: (Any -> Bool)) -> List: empty end,

    find(self, f :: (Any -> Bool)) -> Option: none end,

    partition(self, f): { is-true: empty, is-false: empty } end,

    foldr(self, f, base): base end,

    foldl(self, f, base): base end,

    member(self, elt): false end,

    append(self, other): other end,

    last(self): raise('last: took last of empty list') end,

    reverse(self): self end,

    _equals(self, other): is-empty(other) end,

    tostring(self): "[]" end,

    _torepr(self): "[]" end,

    sort-by(self, cmp, eq): self end,

    sort(self): self end,

    join-str(self, str): "" end

  | link(first :: Any, rest :: List) with:

    length(self): 1 + self.rest.length() end,

    each(self, f):
      f(self.first)
      self.rest.each(f)
    end,

    map(self, f): f(self.first)^link(self.rest.map(f)) end,

    filter(self, f):
      if f(self.first): self.first^link(self.rest.filter(f))
      else:             self.rest.filter(f)
      end
    end,

    partition(self, f): partition(f, self) end,

    find(self, f): find(f, self) end,

    member(self, elt): (elt == self.first) or self.rest.member(elt) end,

    foldr(self, f, base): f(self.first, self.rest.foldr(f, base)) end,

    foldl(self, f, base): self.rest.foldl(f, f(self.first, base)) end,

    append(self, other): self.first^link(self.rest.append(other)) end,

    last(self):
      if is-empty(self.rest): self.first
      else: self.rest.last()
      end
    end,

    reverse(self): reverse-help(self, empty) end,

    _equals(self, other):
      if is-link(other):
        others-equal = (self:first == other:first)
        others-equal and (self:rest == other:rest)
      else:
        false
      end
    end,

    tostring(self):
      "[" +
        for raw-fold(combined from tostring(self:first), elt from self:rest):
          combined + ", " + tostring(elt)
        end
      + "]"
    end,

    _torepr(self):
      "[" +
        for raw-fold(combined from torepr(self:first), elt from self:rest):
          combined + ", " + torepr(elt)
        end
      + "]"
    end,

#    sort-by(self, cmp, eq):
#      doc: "Takes a comparator to check for elements that are strictly greater
#        or less than one another, and an equality procedure for elements that are
#        equal, and sorts the list accordingly.  The sort is not guaranteed to be stable."
#      pivot = self.first
#      # builds up three lists, split according to cmp and eq
#      # Note: We use foldl, which is tail-recursive, but which causes the three
#      # list parts to grow in reverse order.  This isn't a problem, since we're
#      # about to sort two of those parts anyway.
#      three-way-split = self.foldl(fun(e, acc):
#          if cmp(e, pivot):     acc.{are-lt: e^link(acc.are-lt)}
#          else if eq(e, pivot): acc.{are-eq: e^link(acc.are-eq)}
#          else:                 acc.{are-gt: e^link(acc.are-gt)}
#          end
#        end,
#        {are-lt: [], are-eq: [], are-gt: []})
#      less =    three-way-split.are-lt.sort-by(cmp, eq)
#      equal =   three-way-split.are-eq
#      greater = three-way-split.are-gt.sort-by(cmp, eq)
#      less.append(equal.append(greater))
#    end,

    sort(self):
      self.sort-by(fun(e1,e2): e1 < e2 end, fun(e1,e2): e1 == e2 end)
    end,

    join-str(self, str):
      if is-link(self.rest):
         tostring(self.first) + str + self.rest.join-str(str)
      else:
         tostring(self.first)
      end
    end,
    split-at(self, n): split-at(n, self) end,
    take(self, n): split-at(n, self).prefix end,
    drop(self, n): split-at(n, self).suffix end,

    get(self, n): get-help(self, n) end,
    set(self, n, e): set-help(self, n, e) end,


sharing:
  push(self, elt):
    doc: "Adds an element to the front of the list, returning a new list"
    link(elt, self)
  end,
  _plus(self :: List, other :: List): self.append(other) end,
  to-set(self :: List): list-to-set(self) end

end
fun get-help(lst, n :: Number):
  fun help(l, cur):
    if is-empty(l): raise("get: n too large " + tostring(n))
    else if cur == 0: l.first
    else: help(l.rest, cur - 1)
    end
  end
  if n < 0: raise("get: invalid argument: " + tostring(n))
  else: help(lst, n)
  end
end
fun set-help(lst, n :: Number, v):
  fun help(l, cur):
    if is-empty(l): raise("set: n too large " + tostring(n))
    else if cur == 0: v ^ link(l.rest)
    else: l.first ^ link(help(l.rest, cur - 1))
    end
  end
  if n < 0: raise("set: invalid argument: " + tostring(n))
  else: help(lst, n)
  end
end
fun reverse-help(lst, acc):
  cases(List) lst:
    | empty => acc
    | link(first, rest) => reverse-help(rest, first^link(acc))
  end
end

fun raw-fold(f, base, lst :: List):
  if is-empty(lst):
    base
  else:
    raw-fold(f, f(base, lst:first), lst.rest)
  end
end

fun range(start, stop):
  doc: "Creates a list of numbers, starting with start, ending with stop-1"
  if start < stop:       link(start, range(start + 1, stop))
  else if start == stop: empty
  else:  raise("range: start greater than stop: ("
                                 + start.tostring()
                                 + ", "
                                 + stop.tostring()
                                 + ")")
  end
end

fun repeat(n :: Number, e :: Any) -> List:
  doc: "Creates a list with n copies of e"
  if n > 0:       link(e, repeat(n - 1, e))
  else if n == 0: empty
  else:           raise("repeat: can't have a negative argument'")
  end
where:
  repeat(0, 10) is []
  repeat(3, -1) is [-1, -1, -1]
  repeat(1, "foo") is ["foo"]
end

fun filter(f, lst :: List):
  doc: "Returns the subset of lst for which f(elem) is true"
  if is-empty(lst):
    empty
  else:
    if f(lst.first):
      lst.first^link(filter(f, lst.rest))
    else:
      filter(f, lst.rest)
    end
  end
end

fun partition(f, lst :: List):
  doc: "Splits the list into two lists, one for which f(elem) is true, and one for which f(elem) is false"
  fun help(inner-lst):
    if is-empty(inner-lst):
      { is-true: empty, is-false: empty }
    else:
      split-tail = help(inner-lst.rest)
      if f(inner-lst.first):
        { is-true: inner-lst.first^link(split-tail.is-true), is-false: split-tail.is-false }
      else:
        { is-true: split-tail.is-true, is-false: inner-lst.first^link(split-tail.is-false) }
      end
    end
  end
  help(lst)
end

fun find(f :: (Any -> Bool), lst :: List) -> Option:
  doc: "Returns some(elem) where elem is the first elem in lst for which
        f(elem) returns true, or none otherwise"
  if is-empty(lst):
    none
  else:
    if f(lst.first):
      some(lst.first)
    else:
      find(f, lst.rest)
    end
  end
where:
#  find(fun(elt): elt > 1 end, [1,2,3]) is some(2)
#  find(fun(elt): true end, ["find-me"]) is some("find-me")
#  find(fun(elt): elt > 4 end, [1,2,3]) is none
#  find(fun(elt): true end, []) is none
#  find(fun(elt): false end, []) is none
#  find(fun(elt): false end, [1]) is none
end

fun split-at(n :: Number, lst :: List) -> { prefix: List, suffix: List }:
  doc: "Splits the list into two lists, one containing the first n elements, and the other containing the rest"
  when n < 0:
    raise("Invalid index")
  end
  fun help(ind, l):
    if ind == 0:
      { prefix: [], suffix: l }
    else:
      cases(List) l:
        | empty => raise("Index too large")
        | link(fst, rst) =>
          split = help(ind - 1, rst)
          { prefix: link(fst, split.prefix), suffix: split.suffix }
      end
    end
  end
  help(n, lst)
end

fun any(f :: (Any -> Bool), lst :: List) -> Bool:
  doc: "Returns true if f(elem) returns true for any elem of lst"
  is-some(find(f, lst))
where:
#  any(fun(n): n > 1 end, [1,2,3]) is true
#  any(fun(n): n > 3 end, [1,2,3]) is false
#  any(fun(x): true end, []) is false
#  any(fun(x): false end, []) is false
end

fun all(f :: (Any -> Bool), lst :: List) -> Bool:
  doc: "Returns true if f(elem) returns true for all elems of lst"
  is-none(find(fun(v): not f(v) end, lst))
where:
#  all(fun(n): n > 1 end, [1,2,3]) is false
#  all(fun(n): n <= 3 end, [1,2,3]) is true
#  all(fun(x): true end, []) is true
#  all(fun(x): false end, []) is true
end


fun find(f :: (Any -> Bool), lst :: List) -> Option:
  doc: "Returns some(elem) where elem is the first elem in lst for which
        f(elem) returns true, or none otherwise"
  if is-empty(lst):
    none
  else:
    if f(lst.first):
      some(lst.first)
    else:
      find(f, lst.rest)
    end
  end
where:
#  find(fun(elt): elt > 1 end, [1,2,3]) is some(2)
#  find(fun(elt): true end, ["find-me"]) is some("find-me")
#  find(fun(elt): elt > 4 end, [1,2,3]) is none
#  find(fun(elt): true end, []) is none
#  find(fun(elt): false end, []) is none
#  find(fun(elt): false end, [1]) is none
end

fun map(f, lst :: List):
  doc: "Returns a list made up of f(elem) for each elem in lst"
  if is-empty(lst):
    empty
  else:
    f(lst.first)^link(map(f, lst.rest))
  end
end

fun map2(f, l1 :: List, l2 :: List):
  doc: "Returns a list made up of f(elem1, elem2) for each elem1 in l1, elem2 in l2"
  if is-empty(l1) or is-empty(l2):
    empty
  else:
    f(l1.first, l2.first)^link(map2(f, l1.rest, l2.rest))
  end
end

fun map3(f, l1 :: List, l2 :: List, l3 :: List):
  doc: "Returns a list made up of f(e1, e2, e3) for each e1 in l1, e2 in l2, e3 in l3"
  if is-empty(l1) or is-empty(l2) or is-empty(l3):
    empty
  else:
    f(l1.first, l2.first, l3.first)^link(map3(f, l1.rest, l2.rest, l3.rest))
  end
end

fun map4(f, l1 :: List, l2 :: List, l3 :: List, l4 :: List):
  doc: "Returns a list made up of f(e1, e2, e3, e4) for each e1 in l1, e2 in l2, e3 in l3, e4 in l4"
  if is-empty(l1) or is-empty(l2) or is-empty(l3) or is-empty(l4):
    empty
  else:
    f(l1.first, l2.first, l3.first, l4.first)^link(map4(f, l1.rest, l2.rest, l3.rest, l4.rest))
  end
end

fun map_n(f, n :: Number, lst :: List):
  doc: "Returns a list made up of f(n, e1), f(n+1, e2) .. for e1, e2 ... in lst"
  if is-empty(lst):
    empty
  else:
    f(n, lst.first)^link(map_n(f, n + 1, lst.rest))
  end
end

fun map2_n(f, n :: Number, l1 :: List, l2 :: List):
  if is-empty(l1) or is-empty(l2):
    empty
  else:
    f(n, l1.first, l2.first)^link(map2_n(f, n + 1, l1.rest, l2.rest))
  end
end

fun map3_n(f, n :: Number, l1 :: List, l2 :: List, l3 :: List):
  if is-empty(l1) or is-empty(l2) or is-empty(l3):
    empty
  else:
    f(n, l1.first, l2.first, l3.first)^link(map3_n(f, n + 1, l1.rest, l2.rest, l3.rest))
  end
end

fun map4_n(f, n :: Number, l1 :: List, l2 :: List, l3 :: List, l4 :: List):
  if is-empty(l1) or is-empty(l2) or is-empty(l3) or is-empty(l4):
    empty
  else:
    f(n, l1.first, l2.first, l3.first, l4.first)^link(map4(f, n + 1, l1.rest, l2.rest, l3.rest, l4.rest))
  end
end

fun each(f, lst :: List):
  doc: "Calls f for each elem in lst, and returns nothing"
  fun help(l):
    if is-empty(l):
      nothing
    else:
      f(l.first)
      help(l.rest)
    end
  end
  help(lst)
end

fun each2(f, lst1 :: List, lst2 :: List):
  doc: "Calls f on each pair of corresponding elements in l1 and l2, and returns nothing.  Stops after the shortest list"
  fun help(l1, l2):
    if is-empty(l1) or is-empty(l2):
      nothing
    else:
      f(l1.first, l2.first)
      help(l1.rest, l2.rest)
    end
  end
  help(lst1, lst2)
end

fun each3(f, lst1 :: List, lst2 :: List, lst3 :: List):
  doc: "Calls f on each triple of corresponding elements in l1, l2 and l3, and returns nothing.  Stops after the shortest list"
  fun help(l1, l2, l3):
    if is-empty(l1) or is-empty(l2) or is-empty(l3):
      nothing
    else:
      f(l1.first, l2.first, l3.first)
      help(l1.rest, l2.rest, l3.rest)
    end
  end
  help(lst1, lst2, lst3)
end

fun each4(f, lst1 :: List, lst2 :: List, lst3 :: List, lst4 :: List):
  doc: "Calls f on each tuple of corresponding elements in l1, l2, l3 and l4, and returns nothing.  Stops after the shortest list"
  fun help(l1, l2, l3, l4):
    if is-empty(l1) or is-empty(l2) or is-empty(l3) or is-empty(l4):
      nothing
    else:
      f(l1.first, l2.first, l3.first, l4.first)
      help(l1.rest, l2.rest, l3.rest, l4.rest)
    end
  end
  help(lst1, lst2, lst3, lst4)
end

fun each_n(f, num :: Number, lst:: List):
  fun help(n, l):
    if is-empty(l):
      nothing
    else:
      f(n, l.first)
      help(n + 1, l.rest)
    end
  end
  help(num, lst)
end

fun each2_n(f, num :: Number, lst1 :: List, lst2 :: List):
  fun help(n, l1, l2):
    if is-empty(l1) or is-empty(l2):
      nothing
    else:
      f(n, l1.first, l2.first)
      help(n + 1, l1.rest, l2.rest)
    end
  end
  help(num, lst1, lst2)
end

fun each3_n(f, num :: Number, lst1 :: List, lst2 :: List, lst3 :: List):
  fun help(n, l1, l2, l3):
    if is-empty(l1) or is-empty(l2) or is-empty(l3):
      nothing
    else:
      f(n, l1.first, l2.first, l3.first)
      help(n + 1, l1.rest, l2.rest, l3.rest)
    end
  end
  help(num, lst1, lst2, lst3)
end

fun each4_n(f, num :: Number, lst1 :: List, lst2 :: List, lst3 :: List, lst4 :: List):
  fun help(n, l1, l2, l3, l4):
    if is-empty(l1) or is-empty(l2) or is-empty(l3) or is-empty(l4):
      nothing
    else:
      f(n, l1.first, l2.first, l3.first, l4.first)
      help(n + 1, l1.rest, l2.rest, l3.rest, l4.rest)
    end
  end
  help(num, lst1, lst2, lst3, lst4)
end

fun fold(f, base, lst :: List):
  if is-empty(lst):
    base
  else:
    fold(f, f(base, lst.first), lst.rest)
  end
end

fun fold2(f, base, l1 :: List, l2 :: List):
  if is-empty(l1) or is-empty(l2):
    base
  else:
    fold2(f, f(base, l1.first, l2.first), l1.rest, l2.rest)
  end
end

fun fold3(f, base, l1 :: List, l2 :: List, l3 :: List):
  if is-empty(l1) or is-empty(l2) or is-empty(l3):
    base
  else:
    fold3(f, f(base, l1.first, l2.first, l3.first), l1.rest, l2.rest, l3.rest)
  end
end

fun fold4(f, base, l1 :: List, l2 :: List, l3 :: List, l4 :: List):
  if is-empty(l1) or is-empty(l2) or is-empty(l3) or is-empty(l4):
    base
  else:
    fold4(f, f(base, l1.first, l2.first, l3.first, l4.first), l1.rest, l2.rest, l3.rest, l4.rest)
  end
end

fun fold_n(f, num :: Number, base, lst :: List):
  fun help(n, acc, partial-list):
    if is-empty(partial-list):
      acc
    else:
      help(n + 1, f(n, acc, partial-list.first), partial-list.rest)
    end
  end
  help(num, base, lst)
end


fun raw-fold(f, base, lst :: List):
  if is-empty(lst):
    base
  else:
    raw-fold(f, f(base, lst:first), lst.rest)
  end
end

fun index(l :: List, n :: Number):
  cases(List) l:
    | empty      => raise("index: list too short, avast!")
    | link(f, r) => if (n == 0): f else: index(r, n - 1) end
  end
where:
#  l0 = []
#  l1 = [1]
#  l2 = [{some: "object"}, {some-other: "object"}]
#  l3 = ["a", "b", "c"]
#  index(l0, 1) raises "list too short"
#  index(l1, 0) is 1
#  index(l2, 1) is {some-other: "object"}
#  index(l3, 2) is "c"
end

