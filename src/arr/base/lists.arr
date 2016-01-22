#lang pyret/library
provide *
provide-types *
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
    foldr<b>(self :: List<a>, f :: (a, b -> b), base :: b) -> b:
      doc: ```Takes a function and an initial value, and folds the function over this list from the right,
            starting with the base value```
      base
    end,
    foldl<b>(self :: List<a>, f :: (a, b -> b), base :: b) -> b:
      doc: ```Takes a function and an initial value, and folds the function over this list from the left,
            starting with the base value```
      base
    end,
    append(self :: List<a>, other :: List<a>) -> List<a>:
      doc: "Takes a list and returns the result of appending the given list to this list"
      other
    end,
    _tostring(self :: List<a>, shadow tostring :: (Any -> String)) -> String: "[list: ]" end,
    _torepr(self :: List<a>, shadow torepr :: (Any -> String)) -> String: "[list: ]" end,
    sort-by(self :: List<a>, cmp :: (a, a -> Boolean), eq :: (a, a -> Boolean)) -> List<a>:
      doc: ```Takes a comparator to check for elements that are strictly greater
            or less than one another, and an equality procedure for elements that are
            equal, and sorts the list accordingly.  The sort is not guaranteed to be stable.```
      self
    end,
    sort(self :: List<a>) -> List<a>:
      doc: ```Returns a new list whose contents are the smae as those in this list,
            sorted by the default ordering and equality```
      self
    end,
    join-str(self :: List<a>, str :: String) -> String:
      doc: ```Returns a string containing the tostring() forms of the elements of this list,
            joined by the provided separator string```
      ""
    end
  | link(first :: a, rest :: List<a>) with:
    foldr<b>(self :: List<a>, f :: (a, b -> b), base :: b) -> b:
      doc: ```Takes a function and an initial value, and folds the function over this list from the right,
            starting with the initial value```
      f(self.first, self.rest.foldr(f, base))
    end,
    foldl<b>(self :: List<a>, f :: (a, b -> b), base :: b) -> b:
      doc: ```Takes a function and an initial value, and folds the function over this list from the left,
            starting with the initial value```
      self.rest.foldl(f, f(self.first, base))
    end,
    append(self :: List<a>, other :: List<a>) -> List<a>:
      doc: "Takes a list and returns the result of appending the given list to this list"
      self.first ^ link(_, self.rest.append(other))
    end,
    _tostring(self :: List<a>, shadow tostring :: (Any -> String)) -> String:
      "[list: " +
        for fold(combined from tostring(self.first), elt from self.rest):
          combined + ", " + tostring(elt)
        end
      + "]"
    end,
    _torepr(self :: List<a>, shadow torepr :: (Any -> String)) -> String:
      "[list: " +
        for fold(combined from torepr(self.first), elt from self.rest):
          combined + ", " + torepr(elt)
        end
      + "]"
    end,
    sort-by(self :: List<a>, cmp :: (a, a -> Boolean), eq :: (a, a -> Boolean)) -> List<a>:
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
    sort(self :: List<a>) -> List<a>:
      doc: ```Returns a new list whose contents are the same as those in this list,
            sorted by the default ordering and equality```
      self.sort-by(lam(e1,e2): e1 < e2 end, lam(e1,e2): e1 == e2 end)
    end,
    join-str(self :: List<a>, str :: String) -> String:
      doc: ```Returns a string containing the tostring() forms of the elements of this list,
            joined by the provided separator string```
      if is-link(self.rest):
         tostring(self.first) + str + self.rest.join-str(str)
      else:
         tostring(self.first)
      end
    end,
sharing:
  _output(self :: List<a>) -> VS.ValueSkeleton: VS.vs-collection("list", self.map(VS.vs-value)) end,
  _plus(self :: List<a>, other :: List<a>) -> List<a>:
    self.append(other)
  end,
  length(self :: List<a>) -> Number:
    doc: "Takes no other arguments and returns the number of links in the list"
    length(self)
  end,
  each(self :: List<a>, f :: (a -> Nothing)) -> Nothing:
    doc: "Takes a function and calls that function for each element in the list. Returns nothing"
    each(f, self)
  end,
  map<b>(self, f :: (a -> b)) -> List<b>:
    doc: "Takes a function and returns a list of the result of applying that function every element in this list"
    map(f, self)
  end,
  filter(self :: List<a>, f :: (a -> Boolean)) -> List<a>:
    doc: "Takes a predicate and returns a list containing the items in this list for which the predicate returns true."
    filter(f, self)
  end,
  find(self :: List<a>, f :: (a -> Boolean)) -> O.Option<a>:
    doc: "Takes a predicate and returns on option containing either the first item in this list that passes the predicate, or none"
    find(f, self)
  end,
  partition(self :: List<a>, f :: (a -> Boolean)) -> {is-true :: List<a>, is-false :: List<a>}:
    doc: ```Takes a predicate and returns an object with two fields:
          the 'is-true' field contains the list of items in this list for which the predicate holds,
          and the 'is-false' field contains the list of items in this list for which the predicate fails```
    partition(f, self)
  end,
  member(self :: List<a>, elt :: a) -> Boolean:
    doc: "Returns true when the given element is equal to a member of this list"
    member(self, elt)
  end,
  reverse(self :: List<a>) -> List<a>:
    doc: "Returns a new list containing the same elements as this list, in reverse order"
    reverse(self)
  end,
  last(self :: List<a>) -> a:
    doc: "Returns the last element of this list, or raises an error if the list is empty"
    last(self)
  end,
  all(self :: List<a>, f :: (a -> Boolean)) -> Boolean:
    doc: ```Returns true if the given predicate is true for every element in this list```
    all(f, self)
  end,
  any(self :: List<a>, f :: (a -> Boolean)) -> Boolean:
    doc: ```Returns true if the given predicate is true for any element in this list```
    any(f, self)
  end,
  push(self :: List<a>, elt :: a) -> List<a>:
    doc: "Adds an element to the front of the list, returning a new list"
    link(elt, self)
  end,
  split-at(self :: List<a>, n :: Number) -> { prefix :: List<a>, suffix :: List<a> }:
    doc: "Splits this list into two lists, one containing the first n elements, and the other containing the rest"
    split-at(n, self)
  end,
  take(self :: List<a>, n :: Number) -> List<a>:
    doc: "Returns the first n elements of this list"
    split-at(n, self).prefix
  end,
  drop(self :: List<a>, n :: Number) -> List<a>:
    doc: "Returns all but the first n elements of this list"
    split-at(n, self).suffix
  end,
  get(self :: List<a>, n :: Number) -> a:
    doc: "Returns the nth element of this list, or raises an error if n is out of range"
    get(self, n)
  end,
  set(self :: List<a>, n :: Number, e :: a) -> List<a>:
    doc: "Returns a new list with the nth element set to the given value, or raises an error if n is out of range"
    set(self, n, e)
  end,
  remove(self :: List<a>, e :: a) -> List<a>:
    doc: "Returns the list without the element if found, or the whole list if it is not"
    remove(self, e)
  end,
  join-str2(self :: List<a>, str :: String) -> String:
    doc: ```Returns a string containing the tostring() forms of the elements of this list,
          joined by the provided separator string.```
    # Note: use array's join string for performance
    before-to-string = raw-array-from-list(self)
    init-array = raw-array-of("", raw-array-length(before-to-string))
    for raw-array-fold(result from init-array, elt from before-to-string, index from 0):
      raw-array-set(result, index, tostring(elt))
    end
      ^ raw-array-join-str(_, str)
  end
end

fun length<a>(lst :: List<a>) -> Number:
  doc: "Takes a list and returns the number of links in the list"
  fun help(l, cur) -> Number:
    cases (List) lst:
      | empty => cur
      | link(_, r) => help(r, cur + 1)
    end
  end
  help(lst, 0)
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

fun reverse<a>(lst :: List<a>) -> List<a>:
  doc: "Returns a new list containing the same elements as this list, in reverse order"
  fun reverse-help(l :: List<a>, acc :: List<a>) -> List<a>:
    cases(List) l:
      | empty => acc
      | link(first, rest) => reverse-help(rest, first ^ link(_, acc))
    end
  end
  reverse-help(lst, empty)
where:
  reverse([list: ]) is [list: ]
  reverse([list: 1, 3]) is [list: 3, 1]
end

fun last<a>(lst :: List<a>) -> a:
  doc: "Returns the last element of this list, or raises an error if the list is empty"
  fun helper(l :: List<a>) -> a:
    if is-empty(l.rest):
      l.first
    else:
      helper(l.rest)
    end
  end
  if is-empty(lst):
    raise('last: took last of empty list')
  else:
    helper(lst)
  end
end

fun range(start :: Number, stop :: Number) -> List<Number>:
  doc: "Creates a list of numbers, starting with start, ending with stop-1"
  if start < stop:       link(start, range(start + 1, stop))
  else if start == stop: empty
  else:  raise("range: start greater than stop: ("
                                 + tostring(start)
                                 + ", "
                                 + tostring(stop)
                                 + ")")
  end
end

fun range-by(start :: Number, stop :: Number, delta :: Number) -> List<Number>:
  doc: ```Creates a list of numbers, starting with start, in intervals of delta,
          until reaching (but not including) stop```
  if delta == 0:
    if start == stop: empty
    else: raise("range-by: an interval of 0 would produce an infinite list")
    end
  else if delta < 0:
    if start <= stop: empty
    else: link(start, range-by(start + delta, stop, delta))
    end
  else:
    if start >= stop: empty
    else: link(start, range-by(start + delta, stop, delta))
    end
  end
end

fun repeat<a>(n :: Number, e :: a) -> List<a>:
  doc: "Creates a list with n copies of e"
  if n > 0:       link(e, repeat(n - 1, e))
  else if n == 0: empty
  else:           raise("repeat: can't have a negative argument'")
  end
end

fun filter<a>(f :: (a -> Boolean), lst :: List<a>) -> List<a>:
  doc: "Returns the subset of lst for which f(elem) is true"
  if is-empty(lst):
    empty
  else:
    if f(lst.first):
      lst.first ^ link(_, filter(f, lst.rest))
    else:
      filter(f, lst.rest)
    end
  end
end

fun partition<a>(f :: (a -> Boolean), lst :: List<a>) -> {is-true :: List<a>, is-false :: List<a>}:
  doc: "Splits the list into two lists, one for which f(elem) is true, and one for which f(elem) is false"
  var is-true = empty
  var is-false = empty
  fun help(inner-lst):
    if is-empty(inner-lst): nothing
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

fun split-at<a>(n :: Number, lst :: List<a>) -> { prefix :: List<a>, suffix :: List<a> }:
  doc: "Splits the list into two lists, one containing the first n elements, and the other containing the rest"
  when n < 0:
    raise("Invalid index")
  end
  var prefix = empty
  var suffix = empty
  fun help(ind, l):
    if ind == 0: suffix := l
    else:
      cases(List) l:
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

fun any<a>(f :: (a -> Boolean), lst :: List<a>) -> Boolean:
  doc: "Returns true if f(elem) returns true for any elem of lst"
  if is-empty(lst):
    false
  else:
    f(lst.first) or any(f, lst.rest)
  end
end

fun all<a>(f :: (a -> Boolean), lst :: List<a>) -> Boolean:
  doc: "Returns true if f(elem) returns true for all elems of lst"
  if is-empty(lst):
    true
  else:
    f(lst.first) and all(f, lst.rest)
  end
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

fun map<a, b>(f :: (a -> b), lst :: List<a>) -> List<b>:
  doc: "Returns a list made up of f(elem) for each elem in lst"
  if is-empty(lst):
    empty
  else:
    f(lst.first) ^ link(_, map(f, lst.rest))
  end
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

fun each<a>(f :: (a -> Nothing), lst :: List<a>) -> Nothing:
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

fun each2<a, b>(f :: (a, b -> Nothing), lst1 :: List<a>, lst2 :: List<b>) -> Nothing:
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

fun each3<a, b, c>(f :: (a, b, c -> Nothing), lst1 :: List<a>, lst2 :: List<b>, lst3 :: List<c>) -> Nothing:
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

fun each4<a, b, c, d>(f :: (a, b, c, d -> Nothing), lst1 :: List<a>, lst2 :: List<b>, lst3 :: List<c>, lst4 :: List<d>):
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

fun each_n<a>(f :: (Number, a -> Nothing), num :: Number, lst:: List<a>) -> Nothing:
  doc: "Calls f(i, e) for each e in lst and with i counting up from num, and returns nothing"
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

fun each2_n<a, b>(f :: (Number, a, b -> Nothing), num :: Number, lst1 :: List<a>, lst2 :: List<b>) -> Nothing:
  doc: "Calls f(i, e1, e2) for each e1 in lst1, e2 in lst2 and with i counting up from num, and returns nothing"
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

fun each3_n<a, b, c>(f :: (Number, a, b, c -> Nothing), num :: Number, lst1 :: List<a>, lst2 :: List<b>, lst3 :: List<c>) -> Nothing:
  doc: "Calls f(i, e1, e2, e3) for each e1 in lst1, e2 in lst2, e3 in lst3 and with i counting up from num, and returns nothing"
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

fun each4_n<a, b, c, d>(f :: (a, b, c, d -> Nothing), num :: Number, lst1 :: List<a>, lst2 :: List<b>, lst3 :: List<c>, lst4 :: List<d>) -> Nothing:
  doc: "Calls f(i, e1, e2, e3, e4) for each e1 in lst1, e2 in lst2, e3 in lst3, e4 in lst4 and with i counting up from num, and returns nothing"
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
  if is-empty(lst):
    base
  else:
    fold(f, f(base, lst.first), lst.rest)
  end
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
    ix = random(lst.length())
    elt = lst.get(ix)
    link(elt, shuffle(remove(lst, elt)))
  end
end

index = get
get-help = get
set-help = set

list = {
  make: lam(arr):
    raw-array-to-list(arr)
  end
}