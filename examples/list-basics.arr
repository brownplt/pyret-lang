#lang pyret

fun sum(lst):
  cases(List) lst:
    | empty => 0
    | link(head, tail) =>
      head + sum(tail)
  end
where:
  sum([list:]) is 0
  sum([list: 1, 2, 3]) is 6
end

fun reverse(lst):
  fun reverse-h(forward, reversed):
    cases(List) forward:
      | empty => reversed
      | link(head, tail) => reverse-h(tail, link(head, reversed))
    end
  end
  reverse-h(lst, [list:])
where:
  reverse([list:]) is [list:]
  reverse([list: 1, 2, 3]) is [list: 3, 2, 1]
end