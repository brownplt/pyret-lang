#lang pyret/library

provide
  {
    List: List,
    is-empty: is-empty,
    is-link: is-link,
    empty: empty,
    link: link,

    range: range
  }
end

data List:
  | empty() with

    length(self): 0,

    each(self, f): empty(),

    map(self, f): empty(),

    filter(self, f): empty(),

    foldr(self, f, base): base,

    foldl(self, f, base): base,

    member(self, elt): false,

    append(self, other): other,

    last(self): raise('last: took last of empty list'),

    take(self, n):
      cond:
        | n.equals(0) => empty()
        | n.greaterthan(0) => raise('take: took too many')
        | else => raise('take: invalid argument')
      end
    end,

    drop(self, n):
      cond:
        | n.equals(0) => empty()
        | n.greaterthan(0) => raise('drop: dropped too many')
        | else => raise('drop: invalid argument')
      end
    end,

    reverse(self):
       self
    end,

    get(self, n):
      cond:
        | n.greaterequal(0) => raise('get: n too large: '.append(n.tostring()))
        | else => raise('drop: invalid argument')
      end
    end,

    equals(self, other):
      is-empty(other)
    end,

    tostring(self): "[]"

  | link(first, rest) with

    length(self): 1.add(self.rest.length()),

    each(self, f): f(self.first) self.rest.map(f),

    map(self, f): f(self.first)^link(self.rest.map(f)),

    filter(self, f):
      cond:
        | f(self.first) => f(self.first)^link(self.rest.filter(f))
        | else => self.rest.filter(f)
      end
    end,

    member(self, elt): elt.equals(self.first).or(self.rest.member(elt)),

    foldr(self, f, base): f(self.first, self.rest.foldr(f, base)),

    foldl(self, f, base): self.rest.foldl(f, f(self.first, base)),

    append(self, other): self.first^link(self.rest.append(other)),

    last(self):
      cond:
        | is-empty(self.rest) => self.first
        | is-link(self.rest) => self.rest.last()
      end
    end,

    reverse(self):
       self.rest.reverse().append(self.first^link(empty()))
    end,

    take(self, n):
      cond:
        | n.equals(0) => empty()
        | n.greaterthan(0) => self.first^link(self.rest.take(n.minus(1)))
        | else => raise('take: invalid argument')
      end
    end,

    drop(self, n):
      cond:
        | n.equals(0) => self
        | n.greaterthan(0) => self.rest.drop(n.minus(1))
        | else => raise('drop: invalid argument')
      end
    end,

    get(self, n):
      cond:
        | n.equals(0) => self.first
        | n.greaterthan(0) => self.rest.get(n.minus(1))
        | else => raise('get: invalid argument: '.append(n.tostring()))
      end
    end,

    equals(self, other):
      cond:
        | is-link(other) => self.first.equals(other.first).and(self.rest.equals(other.rest))
        | else => false
      end
    end,

    tostring(self):
    "[".append(
      self.rest.foldl(
        \elt, s: (s.append(", ").append(tostring(elt))),
				tostring(self.first))
		).append("]")

sharing
  push(self, elt): link(elt, self)
end

fun range(start, stop):
  cond:
    | start.greaterthan(stop) => raise("range: start greater than stop: (".
                                        append(start.tostring()).
                                        append(", ").
                                        append(stop.tostring()).
                                        append(")"))
    | start.equals(stop)      => empty()
    | start.lessthan(stop)    => link(start, range(start.add(1), stop))
  end
end
