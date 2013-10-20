#lang pyret/library

provide {
  list: list,
  sets: sets,
  builtins: builtins,
  error: error,
  checkers: checkers,
  option: option,
  cs173: cs173
}
end

# BUILTINS

fun identical(obj1, obj2):
  if has-field(obj1, "eq") and has-field(obj2, "eq"):
    obj1.eq(obj2)
  else:
    raise("Identical got values that weren't created by data: " + torepr(obj1) + " and " + torepr(obj2))
  end
end

fun mklist(obj):
  doc: "Creates a List from something with `first` and `rest` fields, recursively"
  if obj.is-empty: empty
  else:            link(obj.first, mklist(obj.rest))
  end
end

fun keys(obj):
  doc: "Returns a List of the keys of an object, as strings"
  mklist(prim-keys(obj))
end

fun has-field(obj, name):
  doc: "Returns true if the object has a field with the name specified"
  prim-has-field(obj, name)
end

fun num-keys(obj):
  doc: "Returns the Number of fields in an object"
  prim-num-keys(obj)
end

fun Eq():
  b = brander()
  {
    extend: fun(obj):
        obj.{eq(self, other): b.test(self) and b.test(other) end}
      end,
    brand: fun(obj): b.brand(obj) end
  }
end

builtins = {
  identical: identical,
  keys: keys,
  has-field: has-field,
  mklist: mklist,
  equiv: equiv,
  data-to-repr: data-to-repr,
  data-equals: data-equals,
  Eq: Eq
}

# LISTS

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
fun take-help(lst, n :: Number):
  fun help(l, cur):
    if cur == 0:        empty
    else if is-link(l): l.first^link(help(l.rest, cur - 1))
    else:               raise('take: n too large: ' + tostring(n))
    end
  end
  if n < 0: raise("take: invalid argument: " + tostring(n))
  else: help(lst, n)
  end
end
fun drop-help(lst, n :: Number):
  fun help(l, cur):
    if cur == 0:        l
    else if is-link(l): l.rest.drop(cur - 1)
    else:               raise("drop: n to large: " + tostring(n))
    end
  end
  if n < 0: raise("drop: invalid argument: " + tostring(n))
  else: help(lst, n)
  end
end


fun list-to-set(lst :: List):
  doc: "Convert a list into a set."
  for fold(s from __set([]), elem from lst):
    s.add(elem)
  end
end

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

    take(self, n): take-help(self, n) end,

    drop(self, n): drop-help(self, n) end,

    reverse(self): self end,

    get(self, n): get-help(self, n) end,

    set(self, n, e): set-help(self, n, e) end,

    _equals(self, other): is-empty(other) end,

    tostring(self): "[]" end,

    _torepr(self): "[]" end,

    sort-by(self, cmp, eq): self end,

    sort(self): self end,

    join-str(self, str): "" end

  | link(cyclic first :: Any, cyclic rest :: List) with:

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

    take(self, n): take-help(self, n) end,

    drop(self, n): drop-help(self, n) end,

    get(self, n): get-help(self, n) end,

    set(self, n, e): set-help(self, n, e) end,

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

    sort-by(self, cmp, eq):
      doc: "Takes a comparator to check for elements that are strictly greater
        or less than one another, and an equality procedure for elements that are
        equal, and sorts the list accordingly."
      pivot = self.first
      less = self.filter(fun(e): cmp(e,pivot) end).sort-by(cmp, eq)
      equal = self.filter(fun(e): eq(e,pivot) end)
      greater = self.filter(fun(e): cmp(pivot,e) end).sort-by(cmp, eq)
      less.append(equal).append(greater)
    end,

    sort(self):
      self.sort-by(fun(e1,e2): e1 < e2 end, fun(e1,e2): e1 == e2 end)
    end,

    join-str(self, str):
      if is-link(self.rest):
         tostring(self.first) + str + self.rest.join-str(str)
      else:
         tostring(self.first)
      end
    end

sharing:
  push(self, elt):
    doc: "Adds an element to the front of the list, returning a new list"
    link(elt, self)
  end,
  _plus(self :: List, other :: List): self.append(other) end,
  to-set(self :: List): list-to-set(self) end

where:
  eq = checkers.check-equals

  eq("list set", [1,2,3].set(1, 5), [1,5,3])

  o1 = {
    _lessthan(self, other): self.x < other.x end,
    _equals(self, other): self.x == other.x end,
    x: 5
  }
  o2 = o1.{
    _lessthan(self, other): self.x < other.x end,
    _equals(self, other): self.x == other.x end,
    x: 10
  }
  [o2, o1].sort() is [o1, o2]
end

fun range(start, stop):
  doc: "Creates a list of numbers, starting with start, ending with stop-1"
  if start < stop:       link(start, range(start + 1, stop))
  else if start == stop: empty
  else if start > stop:  raise("range: start greater than stop: ("
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
      { is-true: [], is-false: [] }
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

fun any(f :: (Any -> Bool), lst :: List) -> Bool:
  doc: "Returns true if f(elem) returns true for any elem of lst"
  is-some(find(f, lst))
where:
  any(fun(n): n > 1 end, [1,2,3]) is true
  any(fun(n): n > 3 end, [1,2,3]) is false
  any(fun(x): true end, []) is false
  any(fun(x): false end, []) is false
end

fun all(f :: (Any -> Bool), lst :: List) -> Bool:
  doc: "Returns true if f(elem) returns true for all elems of lst"
  is-none(find(fun(v): not f(v) end, lst))
where:
  all(fun(n): n > 1 end, [1,2,3]) is false
  all(fun(n): n <= 3 end, [1,2,3]) is true
  all(fun(x): true end, []) is true
  all(fun(x): false end, []) is true
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
  find(fun(elt): elt > 1 end, [1,2,3]) is some(2)
  find(fun(elt): true end, ["find-me"]) is some("find-me")
  find(fun(elt): elt > 4 end, [1,2,3]) is none
  find(fun(elt): true end, []) is none
  find(fun(elt): false end, []) is none
  find(fun(elt): false end, [1]) is none
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
      help(n + 1, f(n, base, partial-list.first), partial-list.rest)
    end
  end
  help(0, base, lst)
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
  l0 = []
  l1 = [1]
  l2 = [{some: "object"}, {some-other: "object"}]
  l3 = ["a", "b", "c"]
  index(l0, 1) raises "list too short"
  index(l1, 0) is 1
  index(l2, 1) is {some-other: "object"}
  index(l3, 2) is "c"
end

list = {
    List: List,
    is-empty: is-empty,
    is-link: is-link,
    empty: empty,
    link: link,

    range: range,
    repeat: repeat,
    filter: filter,
    partition: partition,
    any: any,
    all: all,
    find: find,
    map: map,
    map2: map2,
    map3: map3,
    map4: map4,
    map_n: map_n,
    map2_n: map2_n,
    map3_n: map3_n,
    map4_n: map4_n,
    each: each,
    each2: each2,
    each3: each3,
    each4: each4,
    each_n: each_n,
    each2_n: each2_n,
    each3_n: each3_n,
    each4_n: each4_n,
    fold: fold,
    fold2: fold2,
    fold3: fold3,
    fold4: fold4,
    fold_n: fold_n,
    index: index,
    to-set: list-to-set
  }

data Location:
  | location(file :: String, line, column) with:
    _equals(self, other):
      is-location(other) and
      (self.file == other.file) and
      (self.line == other.line) and
      (self.column == other.column)
    end,
    format(self):
      self.file +
      ": line " +
      self.line.tostring() +
      ", column " +
      self.column.tostring()
    end,
    tostring(self): self.format() end
end

data Error:
  | opaque-error(message :: String, location :: Location, trace :: List<Location>) with:
    name(self): "Error using opaque internal value" end
  | field-not-found(message :: String, location :: Location, trace :: List<Location>) with:
    name(self): "Field not found" end
  | field-non-string(message :: String, location :: Location, trace :: List<Location>) with:
    name(self): "Non-string in field name" end
  | cases-miss(message :: String, location :: Location, trace :: List<Location>) with:
    name(self): "No cases matched" end
  | invalid-case(message :: String, location :: Location, trace :: List<Location>) with:
    name(self): "Invalid case" end
  | eval-error(message :: String, location :: Location, trace :: List<Location>) with:
    name(self): "Eval Error" end
  | user-contract-failure(message :: String, location :: Location, trace :: List<Location>) with:
    name(self): "Contract failure" end
  | arity-error(message :: String, location :: Location, trace :: List<Location>) with:
    name(self): "Arity mismatch" end
  | div-0(message :: String, location :: Location, trace :: List<Location>) with:
    name(self): "Division by zero" end
  | type-error(message :: String, location :: Location, trace :: List<Location>) with:
    name(self): "Type Error" end
  | lazy-error(message :: String, location :: Location, trace :: List<Location>) with:
    name(self): "Error" end
sharing:
  tostring(self): self.format() end,
  format(self):
    self.location.format().append(":\n").append(self.name()).append(": ").append(self.message) end
end


fun make-error(obj):
  trace = if has-field(obj, "trace"):
    for map(l from mklist(obj.trace)):
      location(l.path, l.line, l.column)
    end
  else:
    []
  end
  loc = location(obj.path, obj.line, obj.column)
  if obj.system:
    type = obj.value.type
    msg = obj.value.message
    if (type == "opaque"): opaque-error(msg, loc, trace)
    else if (type == "field-not-found"): field-not-found(msg, loc, trace)
    else if (type == "field-non-string"): field-non-string(msg, loc, trace)
    else if (type == "user-contract-failure"): user-contract-failure(msg, loc, trace)
    else if (type == "eval-error"): eval-error(msg, loc, trace)
    else if (type == "arity-mismatch"): arity-error(msg, loc, trace)
    else if (type == "div-0"): div-0(msg, loc, trace)
    else if (type == "type-error"): type-error(msg, loc, trace)
    else: lazy-error(msg, loc, trace)
    end
  else: obj.value
  end
end

error = {
  opaque-error: opaque-error,
  is-opaque-error : is-opaque-error,
  field-not-found: field-not-found,
  is-field-not-found: is-field-not-found,
  cases-miss: cases-miss,
  is-cases-miss: is-cases-miss,
  invalid-case: invalid-case,
  is-invalid-case: is-invalid-case,
  user-contract-failure: user-contract-failure,
  is-user-contract-failure: is-user-contract-failure,
  div-0: div-0,
  is-div-0: is-div-0,
  make-error: make-error,
  Error: Error,
  Location: Location,
  location: location,
  is-location: is-location
}


data Set:
  | __set(elems :: List) with:

      member(self, elem :: Any) -> Bool:
        doc: 'Check to see if an element is in a set.'
        self.elems.member(elem)
      where:
        sets.set([1, 2, 3]).member(2) is true
        sets.set([1, 2, 3]).member(4) is false
      end,

      add(self, elem :: Any) -> Set:
        doc: "Add an element to the set if it is not already present."
        if (self.elems.member(elem)):
          self
        else:
          __set(link(elem, self.elems))
        end
      where:
        sets.set([]).add(1) is sets.set([1])
        sets.set([1]).add(1) is sets.set([1])
        sets.set([1, 2, 3]).add(2) is sets.set([1, 2, 3])
        sets.set([1, 2, 3]).add(1.5) is sets.set([1, 2, 3, 1.5])
      end,

      remove(self, elem :: Any) -> Set:
        doc: "Remove an element from the set if it is present."
        __set(self.elems.filter(fun (x): x <> elem end))
      where:
        sets.set([1, 2]).remove(18) is sets.set([1, 2])
        sets.set([1, 2]).remove(2) is sets.set([1])
      end,

      to-list(self) -> List:
        doc: 'Convert a set into a sorted list of elements.'
        self.elems.sort()
      where:
        sets.set([3, 1, 2]).to-list() is [1, 2, 3]
      end,

      union(self, other :: Set):
        doc: "Take the union of two sets."
        list-to-set(self.to-list().append(other.to-list()))
      where:
        sets.set([1, 2]).union(sets.set([2, 3])) is sets.set([1, 2, 3])
      end,

      _equals(self, other):
        Set(other) and (self.elems.sort() == other.elems.sort())
      where:
        (sets.set([1, 2.1, 3]) <> sets.set([1, 2.2, 3])) is true
        sets.set([1, 2, 4]) is sets.set([2, 1, 4])
      end
end

sets = {
  Set: Set,
  set: list-to-set
}


data Option:
  | none with:
      orelse(self, v :: Any):
        doc: "Return the default provided value"
        v
      where:
        none.orelse("any value") is "any value"
      end,
      andthen(self, f): self end
  | some(value) with:
      orelse(self, v :: Any):
        doc: "Return self.value, rather than the default"
        self.value
      where:
        some("value").orelse("unused default") is "value"
      end,
      andthen(self, f): f(self.value) end
end

option = {
  Option: Option,
  is-none: is-none,
  is-some: is-some,
  none: none,
  some: some,
}


data CheckResult:
  | success(name :: String, location :: Option<Location>)
  | failure(name :: String, reason :: String, location :: Option<Location>)
  | err(name :: String, exception :: Any, location :: Option<Location>)
end

var current-results = empty
fun add-result(result :: CheckResult):
  current-results := current-results + [result]
end

fun check-is(name, thunk1, thunk2, loc):
  try:
    val1 = thunk1()
    try:
      val2 = thunk2()
      try:
        if val1 == val2:
          add-result(success(name, some(loc)))
        else:
          add-result(
            failure(
              name,
              "Values not equal: \n" +
                torepr(val1) +
                "\n\n" +
                torepr(val2),
              some(loc)
            ))
        end
      except(e):
        add-result(err(name, e, some(loc)))
      end
    except(e):
      add-result(err(name, e, some(loc)))
    end
  except(e):
    add-result(err(name, e, some(loc)))
  end
end

fun check-raises(name, thunk, expected-str :: String, loc):
  doc: "Check that thunk raises either a string that contains expected-str,
        or an exception with a message that contains expected-str"
  fun bad-err(actual):
    add-result(failure(name, "Wrong error message.  The test expected:\n"
                              + torepr(expected-str)
                              + "\nBut this was actually raised:\n"
                              + torepr(actual),
                       some(loc)))
  end
  try:
    val1 = thunk()
    add-result(failure(name, "No exception raised.  The test expected " + torepr(expected-str), some(loc)))
  except(e):
    if String(e) and e.contains(expected-str):
      add-result(success(name, some(loc)))
    else if Error(e):
      if e.message.contains(expected-str):
        add-result(success(name, some(loc)))
      else:
        bad-err(e.message)
      end
    else:
      bad-err(e)
    end
  end
end

fun check-true(name, val): check-equals(name, val, true) end
fun check-false(name, val): check-equals(name, val, false) end
fun check-equals(name, val1, val2):
  try:
    values_equal = val1 == val2
    if values_equal:
      current-results := current-results.push(success(name, none))
    else:
      current-results :=
        current-results + [failure(name, "Values not equal: \n" +
                                     tostring(val1) +
                                     "\n\n" +
                                     tostring(val2),
                                     none)]
    end
    values_equal
  except(e):
    current-results := current-results + [err(name, e, none)]
  end
end

fun check-pred(name, val1, pred):
  try:
    if pred(val1):
      current-results := current-results + [success(name, none)]
    else:
      current-results :=
        current-results + [failure(name, "Value didn't satisfy predicate: " +
                                     tostring(val1) +
                                     ", " +
                                     pred._doc,
                                   none)]
    end
  except(e):
    current-results := current-results + [err(name, e, none)]
  end
end

fun check-exn(name, thunk, pred):
  try:
    thunk()
    current-results :=
      current-results + [failure(name, "Thunk didn't throw an exception: " + tostring(thunk),
                                   none)]
  except(e):
    if pred(e):
      current-results := current-results + [success(name, none)]
    else:
      current-results :=
        current-results + [failure(name, "Wrong exception thrown:" + tostring(e),
                                     none)]
    end
  end
end

data CheckResultList:
  | normal-result(name :: String, location :: Location, results :: List)
  | error-result(name :: String, location :: Location, results :: List, err :: Any)
end

var all-results :: List = empty

fun run-checks(checks):
  when checks.length() <> 0:
    fun lst-to-structural(lst):
      if has-field(lst, 'first'):
        { first: lst.first, rest: lst-to-structural(lst.rest), is-empty: false}
      else:
        { is-empty: true }
      end
    end
    these-checks = mklist(lst-to-structural(checks))
    old-results = current-results
    these-check-results =
      for map(chk from these-checks):
        l = chk.location
        loc = error.location(l.file, l.line, l.column)
        current-results := empty
        result = try:
          chk.run()
          normal-result(chk.name, loc, current-results)
        except(e):
          error-result(chk.name, loc, current-results, e)
        end
        result
      end

    relevant-results = these-check-results.filter(fun(elt):
      is-error-result(elt) or (elt.results.length() > 0)
    end)

    current-results := old-results
    when relevant-results.length() > 0:
      all-results := all-results.push(relevant-results)
    end
  end
  nothing
end

fun clear-results():
  all-results := empty
  nothing
end
fun get-results(val): {
  results: all-results,
  format(self): format-check-results(self.results) end,
  val: val
} end

fun format-check-results(results-list):
  print(check-results-summary(results-list).message)
end

fun check-results-summary(results-list):
  init = { passed: 0, failed : 0, test-errors: 0, other-errors: 0, total: 0 }
  var message = ""
  fun append-str(appendage):
    message := message + appendage + "\n"
  end
  counts = for fold(acc from init, results from results-list):
    for fold(inner-acc from acc, check-result from results):
      inner-results = check-result.results
      new-passed = inner-results.filter(is-success).length()
      new-failed = inner-results.filter(is-failure).length()
      new-errors = inner-results.filter(is-err).length()
      other-errors = link(check-result,empty).filter(is-error-result).length()
      new-results = inner-acc.{
        passed: inner-acc.passed + new-passed,
        failed: inner-acc.failed + new-failed,
        test-errors: inner-acc.test-errors + new-errors,
        other-errors: inner-acc.other-errors + other-errors,
        total: inner-acc.total + inner-results.length()
      }
      when (new-failed <> 0) or (new-errors <> 0) or (other-errors <> 0):
        append-str("\n\nIn check block at " + check-result.location.format())
      end
      for each(fail from inner-results.filter(is-failure)):
        cases(Option) fail.location:
          | none => nothing
          | some(loc) =>
            append-str("In test at " + loc.format())
        end
        append-str("Test " + fail.name + " failed:")
        append-str(fail.reason)
        append-str("")
      end
      for each(fail from inner-results.filter(is-err)):
        cases(Option) fail.location:
          | none => nothing
          | some(loc) =>
            append-str("In test at " + loc.format())
        end
        append-str("Test " + fail.name + " raised an error:")
        append-str(fail.exception.tostring())
        append-str("")
        when has-field(fail.exception, "trace"):
          append-str("Trace:")
          for each(loc from fail.exception.trace):
            append-str(loc.format())
          end
        end
      end
      when is-error-result(check-result):
        append-str("Check block " + check-result.name + " " + check-result.location.format() + " ended in an error: ")
        if Error(check-result.err):
          append-str(check-result.err.format())
        else:
          append-str(check-result.err)
        end
        append-str("")
        when has-field(check-result.err, "trace"):
          append-str("Trace:")
          for each(loc from check-result.err.trace):
            append-str("  " + loc.format())
          end
        end
      end
      new-results
    end
  end
  if (counts.other-errors == 0) and (counts.failed == 0) and (counts.test-errors == 0):
    if counts.passed == 0:
      append-str("
WARNING: Your program didn't define any tests.  Add some where: and check:
blocks to test your code, or run with the --no-checks option to signal that you
don't want tests run.
")
    else:
      if counts.passed == 1:
        append-str("Looks shipshape, your " + counts.passed.tostring() + " test passed, mate!")
      else:
        append-str("Looks shipshape, all " + counts.passed.tostring() + " tests passed, mate!")
      end
    end
  else:
    append-str("Avast, there be bugs!")
    append-str("Total: " + counts.total.tostring() +
          ", Passed: " + counts.passed.tostring() +
          ", Failed: " + counts.failed.tostring() +
          ", Errors in tests: " + counts.test-errors.tostring() +
          ", Errors in between tests: " + counts.other-errors.tostring())
  end
  counts.{ message: message }
end

checkers = {
  CheckResultList: CheckResultList,
  CheckResult: CheckResult,
  check-is: check-is,
  check-raises: check-raises,
  check-true: check-true,
  check-false: check-false,
  check-equals: check-equals,
  check-pred: check-pred,
  check-exn: check-exn,
  run-checks: run-checks,
  format-check-results: format-check-results,
  check-results-summary: check-results-summary,
  clear-results: clear-results,
  get-results: get-results,
  normal-result: normal-result,
  is-normal-result: is-normal-result,
  error-result: error-result,
  is-error-result: is-error-result,
  success: success,
  is-success: is-success,
  failure: failure,
  is-failure: is-failure,
  err: err,
  is-err: is-err
}






### cs 173 ###


fun interp-basic():

  data Value:
    | numV(value :: Number)
    | strV(value :: String)
    | funV(params :: List<String>, body :: Expr, env :: Env)
  end

  data Env:
    | mt-env
    | an-env(name :: String, val :: Value, env :: Env)
  end

  data Expr:
    | id(name :: String)
    | num(value :: Number)
    | str(value :: String)
    | bop(op :: Operator, left :: Expr, right :: Expr)
    | cif(cond :: Expr, consq :: Expr, altern :: Expr)
    | let(name :: String, expr :: Expr, body :: Expr)
    | lam(params :: List<String>, body :: Expr)
    | app(func :: Expr, args :: List<Expr>)
  end

  data Operator:
    | plus
    | minus
    | append
    | str-eq
  end

  fun parse(prog) -> Expr:
    doc: "Parse an s-expr in Paret's concrete syntax into an Expr."

    fun check-params(params :: List<String>) -> List<String>:
      doc: "Ensure that a function has no duplicate parameter names."
      for each(param from params):
        when params.filter(fun(x): x == param end).length() > 1:
          raise("parse: function has duplicate parameter " + param)
        end
      end
      params
    end

    fun convert(sexpr):
      doc: "Convert an s-expression into an Expr."
      if List(sexpr):
        head = sexpr.first
        if head == "string":
          str(sexpr.get(1))
        else if head == "if":
          cif(convert(sexpr.get(1)),
  		    convert(sexpr.get(2)),
              convert(sexpr.get(3)))
        else if head == "let":
          if List(sexpr.get(1)):
            let(sexpr.get(1).get(0),
                convert(sexpr.get(1).get(1)),
                convert(sexpr.get(2)))
          else:
            let(sexpr.get(1),
                convert(sexpr.get(2)),
                convert(sexpr.get(3)))
          end
        else if head == "fun":
          lam(check-params(sexpr.get(1)), convert(sexpr.get(2)))
        else if head == "+":
          bop(plus, convert(sexpr.get(1)), convert(sexpr.get(2)))
        else if head == "-":
          bop(minus, convert(sexpr.get(1)), convert(sexpr.get(2)))
        else if head == "++":
          bop(append, convert(sexpr.get(1)), convert(sexpr.get(2)))
        else if head == "==":
          bop(str-eq, convert(sexpr.get(1)), convert(sexpr.get(2)))
        else:
          func = convert(head)
          args = map(convert, sexpr.rest)
          app(func, args)
        end
      else if Number(sexpr):
        num(sexpr)
      else if String(sexpr):
        id(sexpr)
      end
    end
    convert(prog)
  end

  {
   Value: Value,
   numV: numV, is-numV: is-numV,
   strV: strV, is-strV: is-strV,
   funV: funV, is-funV: is-funV,

   Env: Env,
   mt-env: mt-env, is-mt-env: is-mt-env,
   an-env: an-env, is-an-env: is-an-env,

   Expr: Expr,
   id:id, is-id:is-id,
   num:num, is-num:is-num,
   str:str, is-str:is-str,
   bop:bop, is-bop:is-bop,
   cif:cif, is-cif:is-cif,
   let:let, is-let:is-let,
   lam:lam, is-lam:is-lam,
   app:app, is-app:is-app,

   Operator: Operator,
   plus:plus, is-plus:is-plus,
   minus:minus, is-minus:is-minus,
   append:append, is-append:is-append,
   str-eq:str-eq, is-str-eq:is-str-eq,

   parse:parse
  }
end



fun calculate-locals():

  data Expr:
    | id(name :: String)
    | num(value :: Number)
    | str(value :: String)
    | bop(op :: Operator, left :: Expr, right :: Expr)
    | cif(cond :: Expr, consq :: Expr, altern :: Expr)
    | let(name :: String, expr :: Expr, body :: Expr)
    | lam(params :: List<String>, body :: Expr)
    | app(func :: Expr, args :: List<Expr>)

    | hole
  end

  data Operator:
    | plus
    | minus
    | append
    | str-eq
  end

  fun parse(prog) -> Expr:
    fun convert(sexpr):
      if List(sexpr):
        head = sexpr.first
        if head == "string":
          str(sexpr.get(1))
        else if head == "if":
          cif(convert(sexpr.get(1)),
  		    convert(sexpr.get(2)),
              convert(sexpr.get(3)))
        else if head == "let":
          if List(sexpr.get(1)):
            let(sexpr.get(1).get(0),
                convert(sexpr.get(1).get(1)),
                convert(sexpr.get(2)))
          else:
            let(sexpr.get(1),
                convert(sexpr.get(2)),
                convert(sexpr.get(3)))
          end
        else if head == "fun":
          lam(sexpr.get(1), convert(sexpr.get(2)))
        else if head == "+":
          bop(plus, convert(sexpr.get(1)), convert(sexpr.get(2)))
        else if head == "-":
          bop(minus, convert(sexpr.get(1)), convert(sexpr.get(2)))
        else if head == "++":
          bop(append, convert(sexpr.get(1)), convert(sexpr.get(2)))
        else if head == "==":
          bop(str-eq, convert(sexpr.get(1)), convert(sexpr.get(2)))
        else:
          func = convert(head)
          args = map(convert, sexpr.rest)
          app(func, args)
        end
      else if Number(sexpr):
        num(sexpr)
      else if sexpr == "@":
        hole
      else if String(sexpr):
        id(sexpr)
      end
    end
    convert(prog)
  end

  {
   Expr: Expr,
   id:id, is-id:is-id,
   num:num, is-num:is-num,
   str:str, is-str:is-str,
   bop:bop, is-bop:is-bop,
   cif:cif, is-cif:is-cif,
   let:let, is-let:is-let,
   lam:lam, is-lam:is-lam,
   app:app, is-app:is-app,
   hole:hole, is-hole:is-hole,

   Operator: Operator,
   plus:plus, is-plus:is-plus,
   minus:minus, is-minus:is-minus,
   append:append, is-append:is-append,
   str-eq:str-eq, is-str-eq:is-str-eq,

   parse:parse
  }
end


cs173 = {
  interp-basic: interp-basic(),
  calculate-locals: calculate-locals(),
}
