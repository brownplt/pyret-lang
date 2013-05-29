#lang pyret/library

provide
  {
    List: List,
    is-empty: is-empty,
    is-link: is-link,
    empty: empty,
    link: link,

    range: range,
    map: map,
    fold: fold
  }
end

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
      cond:
        | n == 0 => empty
        | n > 0 => raise('take: took too many')
        | else => raise('take: invalid argument')
      end
    end,

    drop(self, n):
      cond:
        | n == 0 => empty
        | n > 0 => raise('drop: dropped too many')
        | else => raise('drop: invalid argument')
      end
    end,

    reverse(self): self end,

    get(self, n):
      cond:
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
      cond:
        | f(self.first) => self.first^link(self.rest.filter(f))
        | else => self.rest.filter(f)
      end
    end,

    member(self, elt): elt.equals(self.first).or(self.rest.member(elt)) end,

    foldr(self, f, base): f(self.first, self.rest.foldr(f, base)) end,

    foldl(self, f, base): self.rest.foldl(f, f(self.first, base)) end,

    append(self, other): self.first^link(self.rest.append(other)) end,

    last(self):
      cond:
        | is-empty(self.rest) => self.first
        | is-link(self.rest) => self.rest.last()
      end
    end,

    reverse(self):
       self.rest.reverse().append(self.first^link(empty))
    end,

    take(self, n):
      cond:
        | n == 0 => empty
        | n >= 0 => self.first^link(self.rest.take(n - 1))
        | else => raise('take: invalid argument')
      end
    end,

    drop(self, n):
      cond:
        | n == 0 => self
        | n > 0 => self.rest.drop(n - 1)
        | else => raise('drop: invalid argument')
      end
    end,

    get(self, n):
      cond:
        | n > 0 => self.rest.get(n - 1)
        | n == 0 => self.first
        | else => raise('get: invalid argument: ' + n.tostring())
      end
    end,

    equals(self, other):
      cond:
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
  cond:
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
  cond:
    | is-empty(lst) => empty
    | is-link(lst) => f(lst.first)^link(map(f, lst.rest))
  end
end

fun fold(f, base, lst):
  cond:
    | is-empty(lst) => base
    | is-link(lst) => fold(f, f(base, lst.first), lst.rest)
  end
end

