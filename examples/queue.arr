#lang pyret

data Queue:
  | queue(from-front, from-back)
end

mt-queue = queue([list: ], [list: ])

fun enqueue(q, elt):
  queue(link(elt, q.from-front), q.from-back)
where:
  enqueue(mt-queue, 4) is queue([list: 4], [list: ])
  mt-queue
    ^ enqueue(_, 4)
    ^ enqueue(_, 5) is queue([list: 5, 4], [list: ])
  full-q = queue([list: ], [list: 4])
  full-q
    ^ enqueue(_, 5)
    ^ enqueue(_, 6) is queue([list: 6, 5], [list: 4])
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
          new-queue = queue([list: ], new-from-back.rest)
          result = new-from-back.first
          { q: new-queue, v: result }
      end
    | link(f, r) => { q: queue(q.from-front, r), v: f }
  end
where:
  little-q = queue([list: ], [list: 4])
  dequeue(little-q) is { q: queue([list: ], [list: ]), v: 4 }
  dequeue(dequeue(little-q).q) raises "empty queue"

  full-q2 = queue([list: 1, 2, 3], [list: 4, 5, 6])

  dq1 = dequeue(full-q2)
  dq1 is { q: queue([list: 1, 2, 3], [list: 5, 6]), v: 4 }
  dq1.q ^ dequeue(_) is { q: queue([list: 1, 2, 3], [list: 6]), v: 5 }
  (dq1.q ^ dequeue(_)).q ^ dequeue(_) is { q: queue([list: 1, 2, 3], [list: ]), v: 6 }
  ((dq1.q ^ dequeue(_)).q ^ dequeue(_)).q ^ dequeue(_) is { q: queue([list: ], [list: 2, 1]), v: 3 }
end

