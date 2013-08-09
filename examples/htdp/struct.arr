#lang pyret

data NumList:
  | empty()
  | cons(first :: Number, rest :: NumList)
end

empty()
cons(4,empty())

fun length(l :: NumList) -> Number:
  cases(NumList) l:
    | cons(_, rest) => 1 + length(rest)
    | empty => 0
  end
where:
  checkers.check-equals("length of single elem list",
                         length(cons(4,empty())), 1)
  checkers.check-equals("length of 2-elem list",
                        length(cons(4,cons(3,empty()))), 2)
  checkers.check-equals("length of empty list", length(empty()), 0)
end

#print("should be: runtime typecheck failed")
#length({first: {}, rest: {}})
