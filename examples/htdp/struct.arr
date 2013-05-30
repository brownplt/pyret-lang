#lang pyret

data NumList:
  | empty()
  | cons(first :: Number, rest :: NumList)
end

empty()
cons(4,empty())

fun length(l :: NumList) -> Number:
  case:
    | is-cons(l) => 1.add(length(l.rest))
    | is-empty(l) => 0
  end
end

length(cons(4,empty()))
length(cons(4,cons(3,empty())))

print("should be: runtime typecheck failed")
length({first: {}, rest: {}})
