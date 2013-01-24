#lang pyret

provide
  {
    is-List: is-List,
    is-empty: is-empty,
    is-link: is-link,
    empty: empty,
    link: link
  }
end

data List
  | empty with
    length(self): 0,
    map(self, f): empty(),
    filter(self, f): empty(),
    foldr(self, f, base): base,
    foldl(self, f, base): base,
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
    equals(self, other):
      is-empty(other)
    end
  | link: first :: Any, rest :: List with
    length(self): 1.add(self.rest.length()),
    map(self, f): f(self.first)^link(self.rest.map(f)),
    filter(self, f):
      cond:
        | f(self.first) => f(self.first)^link(self.rest.filter(f))
        | else => self.rest.filter(f)
      end
    end,
    foldr(self, f, base): f(self.first, self.rest.foldr(f, base)),
    foldl(self, f, base): self.rest.foldl(f, f(self.first, base)),
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
    equals(self, other):
      cond:
        | is-link(other) => self.first.equals(other.first).and(self.rest.equals(other.rest))
        | else => false
      end
    end
sharing
  push(self, elt): link(elt, self)
end

