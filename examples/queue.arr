#lang pyret

data Queue:
  | queue(from-front, from-back)
end

mt-queue = queue([], [])

fun enqueue(q, elt):
  queue(link(elt, q.from-front), q.from-back)
where:
  enqueue(mt-queue, 4) is queue([4], [])
  mt-queue
    ^enqueue(4)
    ^enqueue(5) is queue([5, 4], [])
  full-q = queue([], [4])
  full-q
    ^enqueue(5)
    ^enqueue(6) is queue([6, 5], [4])
end

fun size(q):
  q.from-front.length() + q.from-back.length()
end

fun dequeue(q):
  cases(List) q.from-back:
    | empty =>
      cases(List) q.from-front:
        | empty => raise("Dequeue on an empty queue")
        | link(_, _) =>
          new-from-back = q.from-front.reverse()
          new-queue = queue([], new-from-back.rest)
          result = new-from-back.first
          { q: new-queue, v: result }
      end
    | link(f, r) => { q: queue(q.from-front, r), v: f }
  end
where:
  little-q = queue([], [4])
  dequeue(little-q) is { q: queue([], []), v: 4 }
  dequeue(dequeue(little-q).q) raises "empty queue"

  full-q2 = queue([1, 2, 3], [4, 5, 6])

  dq1 = dequeue(full-q2)
  dq1 is { q: queue([1, 2, 3], [5, 6]), v: 4 }
  dq1.q^dequeue() is { q: queue([1, 2, 3], [6]), v: 5 }
  dq1.q^dequeue().q^dequeue() is { q: queue([1, 2, 3], []), v: 6 }
  dq1.q^dequeue().q^dequeue().q^dequeue() is { q: queue([], [2, 1]), v: 3 }
end

