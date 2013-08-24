#lang pyret/library

provide {
  list: list,
  builtins: builtins,
  error: error,
  checkers: checkers,
  option: option
}
end

# BUILTINS

fun mklist(obj):
  doc: "creates a List from something with `first` and `rest` fields, recursively"
  if obj.is-empty: empty
  else:            link(obj.first, mklist(obj.rest))
  end
end

fun keys(obj):
  doc: "returns a List of the keys of an object, as strings"
  mklist(prim-keys(obj))
end

fun has-field(obj, name):
  doc: "returns true if the object has a field with the name specified"
  prim-has-field(obj, name)
end

fun num-keys(obj):
  doc: "returns the Number of fields in an object"
  prim-num-keys(obj)
end

fun equiv(obj1, obj2):
  doc: "Check if two objects are equal via an _equals method, or
        have all the same keys with equiv fields"
  fun all_same(o1, o2):
    if Method(o1) or Function(o1):
      false
    else:
      left_keys = keys(o1)
      for fold(same from true, key from left_keys):
        if not (has-field(o2, key)): false
        else:
          left_val = o1.[key]
          right_val = o2.[key]
          same and equiv(left_val, right_val)
        end
      end
    end
  end
  if has-field(obj1, "_equals"):
    obj1._equals(obj2)
  else if num-keys(obj1)._equals(num-keys(obj2)):
    all_same(obj1, obj2)
  else:
    false
  end
where:
  eq = checkers.check-equals
  eq("empty objects", equiv({}, {}), true)
  eq("",equiv({x : 5}, {y : 6}), false)
  eq("",equiv({x : 5}, {x : 6}), false)
  eq("",equiv({x : 5}, {x : 5}), true)
  eq("",equiv({x : 5, y : 6}, {y : 6, x : 5}), true)
  eq("",equiv({x : {z: "foo"}, y : 6}, {y : 6, x : {z: "foo"}}), true)
  eq("",equiv({x : {z: "foo"}, y : [true, 6]}, {y : [true, 6], x : {z: "foo"}}), true)
  eq("",equiv(fun: end, fun: end), false)

  f = fun: end
  eq("functions in objects aren't ever equal", equiv({my_fun:f}, {my_fun:f}), false)
  m = method(self): end
  eq("methods in objects aren't ever equal", equiv({my_meth:m}, {my_meth:m}), false)

  eq("lists of objects", equiv([{}], [{}]), true)
  eq("lists of prims", equiv([5], [5]), true)
end

fun data-to-repr(val, name, fields):
  cases(List) fields:
    | empty => name + "()"
    | link(f, r) =>
      name + "(" +
      for fold(combined from torepr(val.[f]), key from r):
        combined + ", " + torepr(val.[key])
      end
      + ")"
  end
end

fun data-equals(self, other, brand, fields):
  brand(other) and
  for fold(acc from true, f from fields):
    acc and (self.[f] == other.[f])
  end
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


data List:
  | empty with:

    length(self): 0 end,

    each(self, f): nothing end,

    map(self, f): empty end,

    filter(self, f): empty end,

    find(self, f): none end,

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

  | link(first, rest :: List) with:

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
        others-equal = (self.first == other.first)
        others-equal and (self.rest == other.rest)
      else:
        false
      end
    end,

    tostring(self):
      "[" +
        for fold(combined from tostring(self.first), elt from self.rest):
          combined + ", " + tostring(elt)
        end
      + "]"
    end,

    _torepr(self):
      "[" +
        for fold(combined from torepr(self.first), elt from self.rest):
          combined + ", " + torepr(elt)
        end
      + "]"
    end,

    sort-by(self, cmp, eq):
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
    doc: "adds an element to the front of the list, returning a new list"
    link(elt, self)
  end,
  _plus(self, other): self.append(other) end

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
  doc: "creates a list of numbers, starting with start, ending with stop-1"
  if start < stop:       link(start, range(start + 1, stop))
  else if start == stop: empty
  else if start > stop:  raise("range: start greater than stop: ("
                                 + start.tostring()
                                 + ", "
                                 + stop.tostring()
                                 + ")")
  end
end

fun repeat(n :: Number, e :: Any):
  doc: "creates a list with n copies of e"
  if n > 0:       link(e, repeat(n - 1, e))
  else if n == 0: empty
  else:           raise("repeat: can't have a negative argument'")
  end
where:
  eq = checkers.check-equals

  eq("repeat 0", repeat(0, 10), [])
  eq("repeat 3", repeat(3, -1), [-1, -1, -1])
  eq("repeat 1", repeat(1, "foo"), ["foo"])
end

fun filter(f, lst :: List):
  doc: "returns the subset of lst for which f(elem) is true"
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
  doc: "splits the list into two lists, one for which f(elem) is true, and one for which f(elem) is false"
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

fun any(f, lst :: List):
  is-some(find(f, lst))
end

fun find(f, lst :: List):
  doc: "returns Some<elem> where elem is the first elem in lst for which
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
end

fun map(f, lst :: List):
  doc: "returns a list made up of f(elem) for each elem in lst"
  if is-empty(lst):
    empty
  else:
    f(lst.first)^link(map(f, lst.rest))
  end
end

fun map2(f, l1 :: List, l2 :: List):
  doc: "returns a list made up of f(elem1, elem2) for each elem1 in l1, elem2 in l2"
  if is-empty(l1) or is-empty(l2):
    empty
  else:
    f(l1.first, l2.first)^link(map2(f, l1.rest, l2.rest))
  end
end

fun map3(f, l1 :: List, l2 :: List, l3 :: List):
  doc: "returns a list made up of f(e1, e2, e3) for each e1 in l1, e2 in l2, e3 in l3"
  if is-empty(l1) or is-empty(l2) or is-empty(l3):
    empty
  else:
    f(l1.first, l2.first, l3.first)^link(map3(f, l1.rest, l2.rest, l3.rest))
  end
end

fun map4(f, l1 :: List, l2 :: List, l3 :: List, l4 :: List):
  doc: "returns a list made up of f(e1, e2, e3, e4) for each e1 in l1, e2 in l2, e3 in l3, e4 in l4"
  if is-empty(l1) or is-empty(l2) or is-empty(l3) or is-empty(l4):
    empty
  else:
    f(l1.first, l2.first, l3.first, l4.first)^link(map4(f, l1.rest, l2.rest, l3.rest, l4.rest))
  end
end

fun map_n(f, n :: Number, lst :: List):
  doc: "returns a list made up of f(n, e1), f(n+1, e2) .. for e1, e2 ... in lst"
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
    fold4: fold4
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
  | lazy-error(message :: String, location :: Location, trace :: List<Location>) with:
    name(self): "Email joe@cs.brown.edu or dbpatter@cs.brown.edu and complain that they were lazy" end
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
    else if (type == "eval-error"): eval-error(msg, loc, trace)
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
  make-error: make-error,
  Error: Error,
  Location: Location,
  location: location,
  is-location: is-location
}



data Option:
  | none with:
      orelse(self, v): v end,
      andthen(self, f): self end,
      tostring(self): "None" end
  | some(value) with:
      orelse(self, v): self.value end,
      andthen(self, f): f(self.value) end,
      tostring(self): "Some(" + tostring(self.value) + ")" end
end

option = {
  Option: Option,
  is-none: is-none,
  is-some: is-some,
  none: none,
  some: some,
}


data Result:
  | success(name :: String, location :: Option<Location>)
  | failure(name :: String, reason :: String, location :: Option<Location>)
  | err(name :: String, exception :: Any, location :: Option<Location>)
end

var current-results = empty

fun check-is(name, thunk1, thunk2, loc):
  try:
    val1 = thunk1()
    try:
      val2 = thunk2()
      try:
        if val1 == val2:
          current-results := current-results + [success(name, some(loc))]
        else:
          current-results := current-results +
            [failure(
               name,
               "Values not equal: \n" +
                 tostring(val1) +
                 "\n\n" +
                 tostring(val2),
               some(loc)
             )]
        end
      except(e):
        current-results := current-results + [err(name, e, some(loc))]
      end
    except(e):
      current-results := current-results + [err(name, e, some(loc))]
    end
  except(e):
    current-results := current-results + [err(name, e, some(loc))]
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

data CheckResult:
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
fun get-results(): {
  results: all-results,
  format(self): format-check-results(self.results) end
} end

fun format-check-results(results-list):
  init = { passed: 0, failed : 0, test-errors: 0, other-errors: 0, total: 0}
  counts = for fold(acc from init, results from results-list):
    for fold(inner-acc from acc, check-result from results):
      inner-results = check-result.results
      other-errors = link(check-result,empty).filter(is-error-result).length()
      print("In check block at " + check-result.location.format())
      for each(fail from inner-results.filter(is-failure)):
        cases(Option) fail.location:
          | none => nothing
          | some(loc) =>
            print("In test at " + loc.format())
        end
        print("Test " + fail.name + " failed:")
        print(fail.reason)
        print("")
      end
      for each(fail from inner-results.filter(is-err)):
        cases(Option) fail.location:
          | none => nothing
          | some(loc) =>
            print("In test at " + loc.format())
        end
        print("Test " + fail.name + " raised an error:")
        print(fail.exception)
        print("")
        when has-field(fail.exception, "trace"):
          print("Trace:")
          for each(loc from fail.exception.trace):
            print(loc)
          end
        end
      end
      when is-error-result(check-result):
        print("Check block " + check-result.name + " " + check-result.location.format() + " ended in an error: ")
        print(check-result.err)
        print("")
        when has-field(check-result.err, "trace"):
          print("Trace:")
          for each(loc from check-result.err.trace):
            print("  " + loc.format())
          end
        end
      end
      inner-acc.{
        passed: inner-acc.passed + inner-results.filter(is-success).length(),
        failed: inner-acc.failed + inner-results.filter(is-failure).length(),
        test-errors: inner-acc.test-errors + inner-results.filter(is-err).length(),
        other-errors: inner-acc.other-errors + other-errors,
        total: inner-acc.total + inner-results.length()
      }
    end
  end
  print("Total: " + counts.total.tostring() +
        ", Passed: " + counts.passed.tostring() +
        ", Failed: " + counts.failed.tostring() +
        ", Errors in tests: " + counts.test-errors.tostring() +
        ", Errors in between tests: " + counts.other-errors.tostring())
  nothing
end

checkers = {
  check-is: check-is,
  check-true: check-true,
  check-false: check-false,
  check-equals: check-equals,
  check-pred: check-pred,
  check-exn: check-exn,
  run-checks: run-checks,
  format-check-results: format-check-results,
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
