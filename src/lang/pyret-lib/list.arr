#lang pyret/library

provide {
  List: List,
  is-empty: is-empty,
  is-link: is-link,
  empty: empty,
  link: link,

  range: range,
  map: map,
  map2: map2,
  map3: map3,
  map4: map4,
  fold: fold,
  fold2: fold2,
  fold3: fold3,
  fold4: fold4
} end

data List:
  | empty with

    length(self): 0 end,

    each(self, f): empty end,

    map(self, f): empty end,

    filter(self, f): empty end,

    foldr(self, f, base): base end,

    foldl(self, f, base): base end,

    member(self, elt): false end,

    append(self, other): other end,

    last(self): raise('last: took last of empty list') end,

    take(self, n):
      case:
        | n == 0 => empty
        | n > 0 => raise('take: took too many')
        | else => raise('take: invalid argument')
      end
    end,

    drop(self, n):
      case:
        | n == 0 => empty
        | n > 0 => raise('drop: dropped too many')
        | else => raise('drop: invalid argument')
      end
    end,

    reverse(self): self end,

    get(self, n):
      case:
        | n >= 0 => raise('get: n too large: '.append(n.tostring()))
        | else => raise('drop: invalid argument')
      end
    end,

    equals(self, other): is-empty(other) end,

    tostring(self): "[]" end,

    sort-by(self, cmp, eq): self end,

    sort(self): self end

  | link(first, rest) with

    length(self): 1 + self.rest.length() end,

    each(self, f): f(self.first) self.rest.each(f) end,

    map(self, f): f(self.first)^link(self.rest.map(f)) end,

    filter(self, f):
      case:
        | f(self.first) => self.first^link(self.rest.filter(f))
        | else => self.rest.filter(f)
      end
    end,

    member(self, elt): elt.equals(self.first).or(self.rest.member(elt)) end,

    foldr(self, f, base): f(self.first, self.rest.foldr(f, base)) end,

    foldl(self, f, base): self.rest.foldl(f, f(self.first, base)) end,

    append(self, other): self.first^link(self.rest.append(other)) end,

    last(self):
      case:
        | is-empty(self.rest) => self.first
        | is-link(self.rest) => self.rest.last()
      end
    end,

    reverse(self):
       self.rest.reverse().append(self.first^link(empty))
    end,

    take(self, n):
      case:
        | n == 0 => empty
        | n >= 0 => self.first^link(self.rest.take(n - 1))
        | else => raise('take: invalid argument')
      end
    end,

    drop(self, n):
      case:
        | n == 0 => self
        | n > 0 => self.rest.drop(n - 1)
        | else => raise('drop: invalid argument')
      end
    end,

    get(self, n):
      case:
        | n > 0 => self.rest.get(n - 1)
        | n == 0 => self.first
        | else => raise('get: invalid argument: ' + n.tostring())
      end
    end,

    equals(self, other):
      case:
        | is-link(other) =>
          others-equal = (self.first == other.first)
          others-equal.and(self.rest == other.rest)
        | else => false
      end
    end,

    tostring(self):
      "[" +
        for fold(combined from tostring(self.first), elt from self.rest):
          combined + ", " + tostring(elt)
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
    end

sharing
  push(self, elt): link(elt, self) end
end

fun range(start, stop):
  case:
    | start.greaterthan(stop) => raise("range: start greater than stop: ("
                                        + start.tostring()
                                        + ", "
                                        + stop.tostring()
                                        + ")")
    | start.equals(stop)      => empty
    | start.lessthan(stop)    => link(start, range(start + 1, stop))
  end
end

fun map(f, lst):
  case:
    | is-empty(lst) => empty
    | is-link(lst) => f(lst.first)^link(map(f, lst.rest))
  end
end

fun map2(f, l1, l2):
  case:
    | is-empty(l1).or(is-empty(l2)) => empty
    | else => f(l1.first, l2.first)^link(map2(f, l1.rest, l2.rest))
  end
end

fun map3(f, l1, l2, l3):
  case:
    | is-empty(l1).or(is-empty(l2)).or(is-empty(l3)) => empty
    | else => f(l1.first, l2.first, l3.first)^link(map3(f, l1.rest, l2.rest, l3.rest))
  end
end

fun map4(f, l1, l2, l3, l4):
  case:
    | is-empty(l1).or(is-empty(l2)).or(is-empty(l3)).or(is-empty(l4)) => empty
    | else => f(l1.first, l2.first, l3.first, l4.first)^link(map4(f, l1.rest, l2.rest, l3.rest, l4.rest))
  end
end

fun fold(f, base, lst):
  case:
    | is-empty(lst) => base
    | is-link(lst) => fold(f, f(base, lst.first), lst.rest)
  end
end

fun fold2(f, base, l1, l2):
  case:
    | is-empty(l1).or(is-empty(l2)) => base
    | else => fold2(f, f(base, l1.first, l2.first), l1.rest, l2.rest)
  end
end

fun fold3(f, base, l1, l2, l3):
  case:
    | is-empty(l1).or(is-empty(l2)).or(is-empty(l3)) => base
    | else => fold3(f, f(base, l1.first, l2.first, l3.first), l1.rest, l2.rest, l3.rest)
  end
end

fun fold4(f, base, l1, l2, l3, l4):
  case:
    | is-empty(l1).or(is-empty(l2)).or(is-empty(l3)).or(is-empty(l4)) => base
    | else => fold4(f, f(base, l1.first, l2.first, l3.first, l4.first), l1.rest, l2.rest, l3.rest, l4.rest)
  end
end

