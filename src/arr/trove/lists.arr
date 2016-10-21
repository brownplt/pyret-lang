#lang pyret/library

provide *
provide-types *
import global as _
import option as O
import either as E
import equality as equality
import valueskeleton as VS

none = O.none
is-none = O.is-none
some = O.some
is-some = O.is-some
type Option = O.Option

left = E.left
right = E.right
type Either = E.Either

data List<a>:
  | empty with:

    method length(self :: List<a>) -> Number:
      doc: "Takes no other arguments and returns the number of links in the list"
      0
    end,

    method each(self :: List<a>, f :: (a -> Nothing)) -> Nothing:
      doc: "Takes a function and calls that function for each element in the list. Returns nothing"
      nothing
    end,

    method map<b>(self, f :: (a -> b)) -> List<b>:
      doc: "Takes a function and returns a list of the result of applying that function every element in this list"
      empty
    end,

    method filter(self :: List<a>, f :: (a -> Boolean)) -> List<a>:
      doc: "Takes a predicate and returns a list containing the items in this list for which the predicate returns true."
      empty
    end,

    method find(self :: List<a>, f :: (a -> Boolean)) -> O.Option<a>:
      doc: "Takes a predicate and returns on option containing either the first item in this list that passes the predicate, or none"
      none
    end,

    method partition(self :: List<a>, f :: (a -> Boolean)) -> {is-true :: List<a>, is-false :: List<a>}:
      doc: ```Takes a predicate and returns an object with two fields:
            the 'is-true' field contains the list of items in this list for which the predicate holds,
            and the 'is-false' field contains the list of items in this list for which the predicate fails```
      { is-true: empty, is-false: empty }
    end,

    method foldr<b>(self :: List<a>, f :: (a, b -> b), base :: b) -> b:
      doc: ```Takes a function and an initial value, and folds the function over this list from the right,
            starting with the base value```
      base
    end,

    method foldl<b>(self :: List<a>, f :: (a, b -> b), base :: b) -> b:
      doc: ```Takes a function and an initial value, and folds the function over this list from the left,
            starting with the base value```
      base
    end,

    method all(self :: List<a>, f :: (a -> Boolean)) -> Boolean:
      doc: ```Returns true if the given predicate is true for every element in this list```
      true
    end,

    method any(self :: List<a>, f :: (a -> Boolean)) -> Boolean:
      doc: ```Returns true if the given predicate is true for any element in this list```
      false
    end,

    method member(self :: List<a>, elt :: a) -> Boolean:
      doc: "Returns true when the given element is equal to a member of this list"
      false
    end,

    method append(self :: List<a>, other :: List<a>) -> List<a>:
      doc: "Takes a list and returns the result of appending the given list to this list"
      other
    end,

    method last(self :: List<a>) -> a:
      doc: "Returns the last element of this list, or raises an error if the list is empty"
      raise('last: took last of empty list')
    end,

    method reverse(self :: List<a>) -> List<a>:
      doc: "Returns a new list containing the same elements as this list, in reverse order"
      self
    end,

    method _tostring(self :: List<a>, shadow tostring :: (Any -> String)) -> String: "[list: ]" end,

    method _torepr(self :: List<a>, shadow torepr :: (Any -> String)) -> String: "[list: ]" end,

    method sort-by(self :: List<a>, cmp :: (a, a -> Boolean), eq :: (a, a -> Boolean)) -> List<a>:
      doc: ```Takes a comparator to check for elements that are strictly greater
            or less than one another, and an equality procedure for elements that are
            equal, and sorts the list accordingly.  The sort is not guaranteed to be stable.```
      self
    end,

    method sort(self :: List<a>) -> List<a>:
      doc: ```Returns a new list whose contents are the smae as those in this list,
            sorted by the default ordering and equality```
      self
    end,

    method join-str(self :: List<a>, str :: String) -> String:
      doc: ```Returns a string containing the tostring() forms of the elements of this list,
            joined by the provided separator string```
      ""
    end

  | link(first :: a, rest :: List<a>) with:

    method length(self :: List<a>) -> Number:
      doc: "Takes no other arguments and returns the number of links in the list"
      1 + self.rest.length()
    end,

    method each(self :: List<a>, f :: (a -> Nothing)) -> Nothing:
      doc: "Takes a function and calls that function for each element in the list. Returns nothing"
      each(f, self)
    end,

    method map<b>(self, f :: (a -> b)) -> List<b>:
      doc: "Takes a function and returns a list of the result of applying that function every element in this list"
      map(f, self)
    end,

    method filter(self :: List<a>, f :: (a -> Boolean)) -> List<a>:
      doc: "Takes a predicate and returns a list containing the items in this list for which the predicate returns true."
      if f(self.first): self.first ^ link(_, self.rest.filter(f))
      else:             self.rest.filter(f)
      end
    end,

    method partition(self :: List<a>, f :: (a -> Boolean)) -> {is-true :: List<a>, is-false :: List<a>}:
      doc: ```Takes a predicate and returns an object with two fields:
            the 'is-true' field contains the list of items in this list for which the predicate holds,
            and the 'is-false' field contains the list of items in this list for which the predicate fails```
      partition(f, self)
    end,

    method find(self :: List<a>, f :: (a -> Boolean)) -> O.Option<a>:
      doc: "Takes a predicate and returns on option containing either the first item in this list that passes the predicate, or none"
      find(f, self)
    end,

    method member(self :: List<a>, elt :: a) -> Boolean:
      doc: "Returns true when the given element is equal to a member of this list"
      (elt == self.first) or self.rest.member(elt)
    end,

    method foldr<b>(self :: List<a>, f :: (a, b -> b), base :: b) -> b:
      doc: ```Takes a function and an initial value, and folds the function over this list from the right,
            starting with the initial value```
      f(self.first, self.rest.foldr(f, base))
    end,

    method foldl<b>(self :: List<a>, f :: (a, b -> b), base :: b) -> b:
      doc: ```Takes a function and an initial value, and folds the function over this list from the left,
            starting with the initial value```
      self.rest.foldl(f, f(self.first, base))
    end,

    method all(self :: List<a>, f :: (a -> Boolean)) -> Boolean:
      doc: ```Returns true if the given predicate is true for every element in this list```
      f(self.first) and self.rest.all(f)
    end,

    method any(self :: List<a>, f :: (a -> Boolean)) -> Boolean:
      doc: ```Returns true if the given predicate is true for any element in this list```
      f(self.first) or self.rest.any(f)
    end,

    method append(self :: List<a>, other :: List<a>) -> List<a>:
      doc: "Takes a list and returns the result of appending the given list to this list"
      self.first ^ link(_, self.rest.append(other))
    end,

    method last(self :: List<a>) -> a:
      doc: "Returns the last element of this list, or raises an error if the list is empty"
      if is-empty(self.rest): self.first
      else: self.rest.last()
      end
    end,

    method reverse(self :: List<a>) -> List<a>:
      doc: "Returns a new list containing the same elements as this list, in reverse order"
      reverse-help(self, empty)
    end,

    method _tostring(self :: List<a>, shadow tostring :: (Any -> String)) -> String:
      "[list: " +
        for fold(combined from tostring(self.first), elt from self.rest):
          combined + ", " + tostring(elt)
        end
      + "]"
    end,

    method _torepr(self :: List<a>, shadow torepr :: (Any -> String)) -> String:
      "[list: " +
        for fold(combined from torepr(self.first), elt from self.rest):
          combined + ", " + torepr(elt)
        end
      + "]"
    end,

    method sort-by(self :: List<a>, cmp :: (a, a -> Boolean), eq :: (a, a -> Boolean)) -> List<a> block:
      doc: ```Takes a comparator to check for elements that are strictly greater
            or less than one another, and an equality procedure for elements that are
            equal, and sorts the list accordingly.  The sort is not guaranteed to be stable.```
      pivot = self.first

      # builds up three lists, split according to cmp and eq
      # Note: We use each, which is tail-recursive, but which causes the three
      # list parts to grow in reverse order.  This isn't a problem, since we're
      # about to sort two of those parts anyway.
      var are-lt = empty
      var are-eq = empty
      var are-gt = empty
      self.each(lam(e):
          if cmp(e, pivot):     are-lt := e ^ link(_, are-lt)
          else if eq(e, pivot): are-eq := e ^ link(_, are-eq)
          else:                 are-gt := e ^ link(_, are-gt)
          end
        end)
      less =    are-lt.sort-by(cmp, eq)
      equal =   are-eq
      greater = are-gt.sort-by(cmp, eq)
      less.append(equal.append(greater))
    end,

    method sort(self :: List<a>) -> List<a>:
      doc: ```Returns a new list whose contents are the same as those in this list,
            sorted by the default ordering and equality```
      self.sort-by(lam(e1,e2): e1 < e2 end, within(~0))
    end,

    method join-str(self :: List<a>, str :: String) -> String:
      doc: ```Returns a string containing the tostring() forms of the elements of this list,
            joined by the provided separator string```
      if is-link(self.rest):
         tostring(self.first) + str + self.rest.join-str(str)
      else:
         tostring(self.first)
      end
    end,

sharing:
  method _output(self :: List<a>) -> VS.ValueSkeleton:
    len = self.length()
    var curr = self
    VS.vs-collection-iter(self.length(), "list", {
      next: lam():
        cases(List) curr block:
          | empty => none
          | link(f, r) =>
            curr := r
            some(f)
        end
      end
    })
  end,
  
  method _plus(self :: List<a>, other :: List<a>) -> List<a>:
    self.append(other)
  end,

  method push(self :: List<a>, elt :: a) -> List<a>:
    doc: "Adds an element to the front of the list, returning a new list"
    link(elt, self)
  end,
  method split-at(self :: List<a>, n :: Number) -> { prefix :: List<a>, suffix :: List<a> }:
    doc: "Splits this list into two lists, one containing the first n elements, and the other containing the rest"
    split-at(n, self)
  end,
  method take(self :: List<a>, n :: Number) -> List<a>:
    doc: "Returns the first n elements of this list"
    split-at(n, self).prefix
  end,
  method drop(self :: List<a>, n :: Number) -> List<a>:
    doc: "Returns all but the first n elements of this list"
    split-at(n, self).suffix
  end,

  method get(self :: List<a>, n :: Number) -> a:
    doc: "Returns the nth element of this list, or raises an error if n is out of range"
    get(self, n)
  end,
  method set(self :: List<a>, n :: Number, e :: a) -> List<a>:
    doc: "Returns a new list with the nth element set to the given value, or raises an error if n is out of range"
    set(self, n, e)
  end,
  method remove(self :: List<a>, e :: a) -> List<a>:
    doc: "Returns the list without the element if found, or the whole list if it is not"
    remove(self, e)
  end
end

fun get<a>(lst :: List<a>, n :: Number) -> a:
  doc: "Returns the nth element of the given list, or raises an error if n is out of range"
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

fun set<a>(lst :: List<a>, n :: Number, v) -> a:
  doc: ```Returns a new list with the same values as the given list but with the nth element
        set to the given value, or raises an error if n is out of range```
  fun help(l, cur):
    if is-empty(l): raise("set: n too large " + tostring(n))
    else if cur == 0: v ^ link(_, l.rest)
    else: l.first ^ link(_, help(l.rest, cur - 1))
    end
  end
  if n < 0: raise("set: invalid argument: " + tostring(n))
  else: help(lst, n)
  end
end

fun push<a>(l :: List<a>, elt :: a) -> List<a>:
  link(elt, l)
end

fun reverse-help<a>(lst :: List<a>, tail :: List<a>) -> List<a>:
  doc: "Returns a new list containing the same elements as this list, in reverse order"
  builtins.raw-list-fold(lam(acc, elt): link(elt, acc) end, tail, lst)
where:
  reverse-help([list: ], [list: ]) is [list: ]
  reverse-help([list: 1, 3], [list: ]) is [list: 3, 1]
end

fun reverse<a>(lst :: List<a>) -> List<a>: reverse-help(lst, empty) end

fun sort-by<a>(lst :: List<a>, cmp :: (a, a -> Boolean), eq :: (a, a -> Boolean)) -> List<a>:
  lst.sort-by(cmp, eq)
end

fun sort<a>(lst :: List<a>) -> List<a>:
  lst.sort()
end

fun range(start :: Number, stop :: Number) -> List<Number>:
  doc: "Creates a list of numbers, starting with start, ending with stop-1"
  if start > stop: raise("range: start greater than stop: ("
                                 + tostring(start)
                                 + ", "
                                 + tostring(stop)
                                 + ")")
  else: raw-array-to-list(raw-array-build(_ + start, stop - start))
  end
end

fun range-by(start :: Number, stop :: Number, delta :: Number) -> List<Number>:
  doc: ```Creates a list of numbers, starting with start, in intervals of delta,
          until reaching (but not including) stop```
  if delta == 0:
    if start == stop: empty
    else: raise("range-by: an interval of 0 would produce an infinite list")
    end
  else:
    length = num-max(num-ceiling((stop - start) / delta), 0)
    raw-array-to-list(raw-array-build(lam(i): start + (i * delta) end, length))
  end
where:
  range-by(1, 10, 4) is [list: 1, 5, 9]
  range-by(10, 1, -4) is [list: 10, 6, 2]
  range-by(3, 20, 9) is [list: 3, 12]
  range-by(20, 3, 9) is empty
  range-by(20, 3, -9) is [list: 20, 11]
  range-by(2, 3, 0) raises "interval of 0"
end

fun repeat<a>(n :: Number, e :: a) -> List<a>:
  doc: "Creates a list with n copies of e"
  if n < 0: raise("repeat: can't have a negative argument'")
  else: raw-array-to-list(raw-array-of(e, n))
  end
end

fun filter<a>(f :: (a -> Boolean), lst :: List<a>) -> List<a>:
  doc: "Returns the subset of lst for which f(elem) is true"
  builtins.raw-list-filter(f, lst)
end

fun append<a>(front :: List<a>, back :: List<a>) -> List<a>:
  cases(List<a>) front:
    | empty => back
    | link(f, r) => link(f, append(r, back))
  end
end

fun partition<a>(f :: (a -> Boolean), lst :: List<a>) -> {is-true :: List<a>, is-false :: List<a>} block:
  doc: "Splits the list into two lists, one for which f(elem) is true, and one for which f(elem) is false"
  var is-true = empty
  var is-false = empty
  fun help(inner-lst):
    if is-empty(inner-lst) block:
      nothing
    else:
      help(inner-lst.rest)
      if f(inner-lst.first):
        is-true := inner-lst.first ^ link(_, is-true)
      else:
        is-false := inner-lst.first ^ link(_, is-false)
      end
    end
  end
  help(lst)
  { is-true: is-true, is-false: is-false }
end

fun remove<a>(lst :: List<a>, elt :: a) -> List<a>:
  doc: ```Returns the list without the element if found, or the whole list if it is not```
  if is-empty(lst):
    empty
  else:
    if elt == lst.first:
      remove(lst.rest, elt)
    else:
      link(lst.first, remove(lst.rest, elt))
    end
  end
end

fun find<a>(f :: (a -> Boolean), lst :: List<a>) -> O.Option<a>:
  doc: ```Returns some(elem) where elem is the first elem in lst for which
        f(elem) returns true, or none otherwise```
  if is-empty(lst):
    none
  else:
    if f(lst.first):
      some(lst.first)
    else:
      find(f, lst.rest)
    end
  end
end

fun split-at<a>(n :: Number, lst :: List<a>) -> { prefix :: List<a>, suffix :: List<a> } block:
  doc: "Splits the list into two lists, one containing the first n elements, and the other containing the rest"
  when n < 0:
    raise("Invalid index")
  end
  var prefix = empty
  var suffix = empty
  fun help(ind, l):
    if ind == 0: suffix := l
    else:
      cases(List) l block:
        | empty => raise("Index too large")
        | link(fst, rst) =>
          help(ind - 1, rst)
          prefix := fst ^ link(_, prefix)
      end
    end
  end
  help(n, lst)
  { prefix: prefix, suffix: suffix }
end

fun take<a>(n :: Number, lst :: List<a>) -> List<a>:
  doc: "Returns a list containing the first n elements of the given list"
  split-at(n, lst).prefix
end

fun drop<a>(n :: Number, lst :: List<a>) -> List<a>:
  doc: "Returns a list containing all but the first n elements of the given list"
  split-at(n, lst).suffix
end

fun last<a>(l :: List<a>) -> a:
  l.last()
end

fun any<a>(f :: (a -> Boolean), lst :: List<a>) -> Boolean:
  doc: "Returns true if f(elem) returns true for any elem of lst"
  lst.any(f)
end

fun all<a>(f :: (a -> Boolean), lst :: List<a>) -> Boolean:
  doc: "Returns true if f(elem) returns true for all elems of lst"
  lst.all(f)
end

fun all2<a, b>(f :: (a, b -> Boolean), lst1 :: List<b>, lst2 :: List<b>) -> Boolean:
  doc: ```Returns true if f(elem1, elem2) returns true for all corresponding elems of lst1 and list2.
        Returns true when either list is empty```
  fun help(l1, l2):
    if is-empty(l1) or is-empty(l2): true
    else: f(l1.first, l2.first) and help(l1.rest, l2.rest)
    end
  end
  help(lst1, lst2)
end

fun map<a, b>(f :: (a -> b), lst :: List<a>) -> List<b> block:
  doc: "Returns a list made up of f(elem) for each elem in lst"
  builtins.raw-list-map(f, lst)
end

fun map2<a, b, c>(f :: (a, b -> c), l1 :: List<a>, l2 :: List<b>) -> List<c>:
  doc: "Returns a list made up of f(elem1, elem2) for each elem1 in l1, elem2 in l2"
  if is-empty(l1) or is-empty(l2):
    empty
  else:
    f(l1.first, l2.first) ^ link(_, map2(f, l1.rest, l2.rest))
  end
end

fun map3<a, b, c, d>(f :: (a, b, c -> d), l1 :: List<a>, l2 :: List<b>, l3 :: List<c>) -> List<d>:
  doc: "Returns a list made up of f(e1, e2, e3) for each e1 in l1, e2 in l2, e3 in l3"
  if is-empty(l1) or is-empty(l2) or is-empty(l3):
    empty
  else:
    f(l1.first, l2.first, l3.first) ^ link(_, map3(f, l1.rest, l2.rest, l3.rest))
  end
end

fun map4<a, b, c, d, e>(f :: (a, b, c, d -> e), l1 :: List<a>, l2 :: List<b>, l3 :: List<c>, l4 :: List<d>) -> List<e>:
  doc: "Returns a list made up of f(e1, e2, e3, e4) for each e1 in l1, e2 in l2, e3 in l3, e4 in l4"
  if is-empty(l1) or is-empty(l2) or is-empty(l3) or is-empty(l4):
    empty
  else:
    f(l1.first, l2.first, l3.first, l4.first) ^ link(_, map4(f, l1.rest, l2.rest, l3.rest, l4.rest))
  end
end

fun map_n<a, b>(f :: (Number, a -> b), n :: Number, lst :: List<a>) -> List<b>:
  doc: "Returns a list made up of f(n, e1), f(n+1, e2) .. for e1, e2 ... in lst"
  if is-empty(lst):
    empty
  else:
    f(n, lst.first) ^ link(_, map_n(f, n + 1, lst.rest))
  end
end

fun map2_n<a, b, c>(f :: (Number, a, b -> c), n :: Number, l1 :: List<a>, l2 :: List<b>) -> List<c>:
  doc: "Returns a list made up of f(i, e1, e2) for each e1 in l1, e2 in l2, and i counting up from n"
  if is-empty(l1) or is-empty(l2):
    empty
  else:
    f(n, l1.first, l2.first) ^ link(_, map2_n(f, n + 1, l1.rest, l2.rest))
  end
end

fun map3_n<a, b, c, d>(f :: (Number, a, b, c -> d), n :: Number, l1 :: List<a>, l2 :: List<b>, l3 :: List<c>) -> List<d>:
  doc: "Returns a list made up of f(i, e1, e2, e3) for each e1 in l1, e2 in l2, e3 in l3, and i counting up from n"
  if is-empty(l1) or is-empty(l2) or is-empty(l3):
    empty
  else:
    f(n, l1.first, l2.first, l3.first) ^ link(_, map3_n(f, n + 1, l1.rest, l2.rest, l3.rest))
  end
end

fun map4_n<a, b, c, d, e>(f :: (Number, a, b, c, d -> e), n :: Number, l1 :: List<a>, l2 :: List<b>, l3 :: List<c>, l4 :: List<d>) -> List<e>:
  doc: "Returns a list made up of f(i, e1, e2, e3, e4) for each e1 in l1, e2 in l2, e3 in l3, e4 in l4, and i counting up from n"
  if is-empty(l1) or is-empty(l2) or is-empty(l3) or is-empty(l4):
    empty
  else:
    f(n, l1.first, l2.first, l3.first, l4.first) ^ link(_, map4_n(f, n + 1, l1.rest, l2.rest, l3.rest, l4.rest))
  end
end

fun each<a>(f :: (a -> Nothing), lst :: List<a>) -> Nothing block:
  doc: "Calls f for each elem in lst, and returns nothing"
  builtins.raw-list-fold(lam(_, elt): f(elt) end, nothing, lst)
  nothing
end

fun each2<a, b>(f :: (a, b -> Nothing), lst1 :: List<a>, lst2 :: List<b>) -> Nothing:
  doc: "Calls f on each pair of corresponding elements in l1 and l2, and returns nothing.  Stops after the shortest list"
  fun help(l1, l2):
    if is-empty(l1) or is-empty(l2) block:
      nothing
    else:
      f(l1.first, l2.first)
      help(l1.rest, l2.rest)
    end
  end
  help(lst1, lst2)
end

fun each3<a, b, c>(f :: (a, b, c -> Nothing), lst1 :: List<a>, lst2 :: List<b>, lst3 :: List<c>) -> Nothing:
  doc: "Calls f on each triple of corresponding elements in l1, l2 and l3, and returns nothing.  Stops after the shortest list"
  fun help(l1, l2, l3):
    if is-empty(l1) or is-empty(l2) or is-empty(l3) block:
      nothing
    else:
      f(l1.first, l2.first, l3.first)
      help(l1.rest, l2.rest, l3.rest)
    end
  end
  help(lst1, lst2, lst3)
end

fun each4<a, b, c, d>(f :: (a, b, c, d -> Nothing), lst1 :: List<a>, lst2 :: List<b>, lst3 :: List<c>, lst4 :: List<d>):
  doc: "Calls f on each tuple of corresponding elements in l1, l2, l3 and l4, and returns nothing.  Stops after the shortest list"
  fun help(l1, l2, l3, l4):
    if is-empty(l1) or is-empty(l2) or is-empty(l3) or is-empty(l4) block:
      nothing
    else:
      f(l1.first, l2.first, l3.first, l4.first)
      help(l1.rest, l2.rest, l3.rest, l4.rest)
    end
  end
  help(lst1, lst2, lst3, lst4)
end

fun each_n<a>(f :: (Number, a -> Nothing), num :: Number, lst:: List<a>) -> Nothing:
  doc: "Calls f(i, e) for each e in lst and with i counting up from num, and returns nothing"
  fun help(n, l):
    if is-empty(l) block:
      nothing
    else:
      f(n, l.first)
      help(n + 1, l.rest)
    end
  end
  help(num, lst)
end

fun each2_n<a, b>(f :: (Number, a, b -> Nothing), num :: Number, lst1 :: List<a>, lst2 :: List<b>) -> Nothing:
  doc: "Calls f(i, e1, e2) for each e1 in lst1, e2 in lst2 and with i counting up from num, and returns nothing"
  fun help(n, l1, l2):
    if is-empty(l1) or is-empty(l2) block:
      nothing
    else:
      f(n, l1.first, l2.first)
      help(n + 1, l1.rest, l2.rest)
    end
  end
  help(num, lst1, lst2)
end

fun each3_n<a, b, c>(f :: (Number, a, b, c -> Nothing), num :: Number, lst1 :: List<a>, lst2 :: List<b>, lst3 :: List<c>) -> Nothing:
  doc: "Calls f(i, e1, e2, e3) for each e1 in lst1, e2 in lst2, e3 in lst3 and with i counting up from num, and returns nothing"
  fun help(n, l1, l2, l3):
    if is-empty(l1) or is-empty(l2) or is-empty(l3) block:
      nothing
    else:
      f(n, l1.first, l2.first, l3.first)
      help(n + 1, l1.rest, l2.rest, l3.rest)
    end
  end
  help(num, lst1, lst2, lst3)
end

fun each4_n<a, b, c, d>(f :: (a, b, c, d -> Nothing), num :: Number, lst1 :: List<a>, lst2 :: List<b>, lst3 :: List<c>, lst4 :: List<d>) -> Nothing:
  doc: "Calls f(i, e1, e2, e3, e4) for each e1 in lst1, e2 in lst2, e3 in lst3, e4 in lst4 and with i counting up from num, and returns nothing"
  fun help(n, l1, l2, l3, l4):
    if is-empty(l1) or is-empty(l2) or is-empty(l3) or is-empty(l4) block:
      nothing
    else:
      f(n, l1.first, l2.first, l3.first, l4.first)
      help(n + 1, l1.rest, l2.rest, l3.rest, l4.rest)
    end
  end
  help(num, lst1, lst2, lst3, lst4)
end

fun fold-while<a, b>(f :: (a, b -> Either<a, a>), base :: a, lst :: List<b>) -> a:
  doc: ```Takes a function that takes two arguments and returns an Either, and also a base value, and folds
        over the given list from the left as long as the function returns a left() value, and returns either
        the final value or the right() value```
  cases(List) lst:
    | empty => base
    | link(elt, r) =>
      cases(E.Either) f(base, elt):
        | left(v) => fold-while(f, v, r)
        | right(v) => v
      end
  end
end

fun fold<a, b>(f :: (a, b -> a), base :: a, lst :: List<b>) -> a:
  doc: ```Takes a function, an initial value and a list, and folds the function over the list from the left,
        starting with the initial value```
  builtins.raw-list-fold(f, base, lst)
end

rec foldl = fold

fun foldr<a, b>(f :: (a, b -> a), base :: a, lst :: List<b>) -> a:
  doc: ```Takes a function, an initial value and a list, and folds the function over the list from the right,
        starting with the initial value```
  if is-empty(lst):
    base
  else:
    f(foldr(f, base, lst.rest), lst.first)
  end
end

fun fold2<a, b, c>(f :: (a, b, c -> a), base :: a, l1 :: List<b>, l2 :: List<c>) -> a:
  doc: ```Takes a function, an initial value and two lists, and folds the function over the lists in parallel
        from the left, starting with the initial value and ending when either list is empty```
  if is-empty(l1) or is-empty(l2):
    base
  else:
    fold2(f, f(base, l1.first, l2.first), l1.rest, l2.rest)
  end
end

fun fold3<a, b, c, d>(f :: (a, b, c, d -> a), base :: a, l1 :: List<b>, l2 :: List<c>, l3 :: List<d>) -> a:
  doc: ```Takes a function, an initial value and three lists, and folds the function over the lists in parallel
        from the left, starting with the initial value and ending when any list is empty```
  if is-empty(l1) or is-empty(l2) or is-empty(l3):
    base
  else:
    fold3(f, f(base, l1.first, l2.first, l3.first), l1.rest, l2.rest, l3.rest)
  end
end

fun fold4<a, b, c, d, e>(f :: (a, b, c, d, e -> a), base :: a, l1 :: List<b>, l2 :: List<c>, l3 :: List<d>, l4 :: List<e>) -> a:
  doc: ```Takes a function, an initial value and four lists, and folds the function over the lists in parallel
        from the left, starting with the initial value and ending when any list is empty```
  if is-empty(l1) or is-empty(l2) or is-empty(l3) or is-empty(l4):
    base
  else:
    fold4(f, f(base, l1.first, l2.first, l3.first, l4.first), l1.rest, l2.rest, l3.rest, l4.rest)
  end
end

fun fold_n<a, b>(f :: (Number, a, b -> a), num :: Number, base :: a, lst :: List<b>) -> a:
  doc: ```Takes a function, an initial value and a list, and folds the function over the list from the left,
        starting with the initial value and passing along the index (starting with the given num)```
  fun help(n, acc, partial-list):
    if is-empty(partial-list):
      acc
    else:
      help(n + 1, f(n, acc, partial-list.first), partial-list.rest)
    end
  end
  help(num, base, lst)
end

fun member-with<a>(lst :: List<a>, elt :: a, eq :: (a, a -> equality.EqualityResult)):
  ask:
    | is-empty(lst) then: equality.NotEqual("list", elt, lst)
    | is-link(lst) then:
      f = lst.first
      r = lst.rest
      first-elt-equal = eq(f, elt)
      cases(equality.EqualityResult) first-elt-equal:
        | Equal => equality.Equal
        | else => equality.equal-or(first-elt-equal, member-with(r, elt, eq))
      end
  end
end

fun member3<a>(lst :: List<a>, elt :: a) -> equality.EqualityResult:
  member-with(lst, elt, equal-always3)
end

fun member<a>(lst :: List<a>, elt :: a) -> Boolean:
  equality.to-boolean(member3(lst, elt))
end

member-always3 = member3
member-always = member

fun member-now3<a>(lst :: List<a>, elt :: a) -> equality.EqualityResult:
  member-with(lst, elt, equal-now3)
end

fun member-now<a>(lst :: List<a>, elt :: a) -> Boolean:
  equality.to-boolean(member-now3(lst, elt))
end

fun member-identical3<a>(lst :: List<a>, elt :: a) -> equality.EqualityResult:
  member-with(lst, elt, identical3)
end

fun member-identical<a>(lst :: List<a>, elt :: a) -> Boolean:
  equality.to-boolean(member-identical3(lst, elt))
end

fun shuffle<a>(lst :: List<a>) -> List<a>:
  if is-empty(lst): empty
  else:
    elts = for fold_n(i from 1, arr from raw-array-of(lst.first, lst.length()), e from lst.rest) block:
      ix = random(i)
      raw-array-set(arr, i, raw-array-get(arr, ix))
      raw-array-set(arr, ix, e)
      arr
    end
    raw-array-to-list(elts)
  end
end

fun filter-map<a, b>(f :: (a -> Option<b>), lst :: List<a>) -> List<b>:
  cases(List<a>) lst:
    | empty => empty
    | link(first, rest) =>
      cases(Option<b>) f(first):
        | none => filter-map(f, rest)
        | some(v) => link(v, filter-map(f, rest))
      end
  end
end

fun filter-values<a>(lst :: List<Option<a>>) -> List<a>:
  cases(List<a>) lst:
    | empty => empty
    | link(first, rest) =>
      cases(Option<a>) first:
        | none => filter-values(rest)
        | some(v) => link(v, filter-values(rest))
      end
  end
end  

fun distinct(l :: List) -> List:
  doc: "returns a list with exactly the distinct elements of the original list removing the first instance"
  cases (List) l:
    | empty => empty
    | link(first, rest) =>
      cases(equality.EqualityResult) member3(rest, first):
        | NotEqual(_, _, _) => link(first, distinct(rest))
        | Unknown(_, _, _) => link(first, distinct(rest))
        | Equal => distinct(rest)
      end
  end
end

fun length(l :: List) -> Number:
  l.length()
end

fun join-str(l :: List<String>, s :: String) -> String:
  l.join-str(s)
end

list = {
  make: raw-array-to-list,
  make0: lam(): empty end,
  make1: lam(a): link(a, empty) end,
  make2: lam(a, b): link(a, link(b, empty)) end,
  make3: lam(a, b, c): link(a, link(b, link(c, empty))) end,
  make4: lam(a, b, c, d): link(a, link(b, link(c, link(d, empty)))) end,
  make5: lam(a, b, c, d, e): link(a, link(b, link(c, link(d, link(e, empty))))) end,
}
