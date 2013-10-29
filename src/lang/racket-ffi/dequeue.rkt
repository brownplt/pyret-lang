#lang pyret

provide {
  MutQueue : MutQueue,
  empty-q : empty-q,
  single-q : single-q,
  each : q-each,
  fold : q-fold
} end

data MutQueueLink<T>:
  | mtq with: tostring(self): "[]" end,
    each(self, f): nothing end,
    fold(self, f, acc): acc end,
    length(self): 0 end
  | q(first :: T, mutable rest :: MutQueueLink<T>) with:
    tostring(self): tostring(self.first) + "!!" + tostring(self!rest) end,
    each(self, f):
      f(self.first)
      self!rest.each(f)
    end,
    fold(self, f, acc):
      new-acc = f(acc, self.first)
      self!rest.fold(f, new-acc)
    end,
    length(self): 1 + self!rest.length() end
end
data MutQueue<T>:
  | mut-q(mutable head :: MutQueueLink<T>, mutable last :: MutQueueLink<T>) with:
    tostring(self): tostring(self!head) end
sharing:
  enqueue(self, v):
    if self.is-empty():
      new-link = q(v, mtq)
      self!{head: new-link, last: new-link}
    else:
      new-link = q(v, mtq)
      self!last!{rest: new-link}
      self!{last: new-link}
    end
    self
  end,
  is-empty(self): is-mtq(self!head) end,
  length(self): self!head.length() end,
  append(self, other):
    doc: "Destructively appends the provided queue to the current one;
    when finished, the other queue will be empty"
    if other.is-empty(): self
    else: self.enqueue(other.dequeue()).append(other)
    end
  end,
  each(self, f): self!head.each(f) end,
  fold(self, f, acc): self!head.fold(f, acc) end,    
  enqueue-many(self, vs :: List):
    list.each(self.enqueue, vs)
    self
  end,
  dequeue(self):
    if is-mtq(self!last): nothing
    else:
      ret = self!head.first
      new-head = self!head!rest
      if is-mtq(new-head):
        self!{last: new-head, head: new-head}
      else:
        self!{head: new-head}
      end
      ret
    end
  end
where:
  my-q = mut-q(mtq, mtq).enqueue(1).enqueue(2).enqueue(3)
  my-q.dequeue() is 1
  my-q.dequeue() is 2
  my-q.enqueue(4).dequeue() is 3
  my-q.dequeue() is 4
  my-q.dequeue() is nothing
end

fun empty-q(): mut-q(mtq, mtq) end
fun single-q(v): empty-q().enqueue(v) end
fun q-each(f, queue): queue.each(f) end
fun q-fold(f, acc, queue):
  queue.fold(f, acc)
where:
  my-q = empty-q().enqueue(8).enqueue(7).enqueue(6).enqueue(5)
  folded = for q-fold(acc from [], num from my-q):
    when (num > 0) and (num.modulo(2) == 0):
      my-q.enqueue(num / 2)
    end
    num^list.link(acc)
  end
  var count = 0
  for q-each(num from my-q):
    count := count + 1
    my-q.dequeue()
  end
  folded is [1,2,3,4,5,6,7,8]
  my-q.is-empty() is true
  count is 8

  my-q.enqueue(8).enqueue(7).enqueue(6).enqueue(5)
  for q-each(num from my-q):
    when (num > 0) and (num.modulo(2) == 0):
      my-q.enqueue(num / 2)
    end
    my-q.dequeue()
  end
  my-q.is-empty() is true
end
