#lang pyret

data NumList:
  | mt()
  | cons(first :: Number, rest :: NumList)
end

mt()
cons(4,mt())

fun length(l :: NumList) -> Number:
  cases(NumList) l:
    | cons(_, rest) => 1 + length(rest)
    | mt => 0
  end
where:
  checkers.check-equals("length of single elem list",
                         length(cons(4,mt())), 1)
  checkers.check-equals("length of 2-elem list",
                        length(cons(4,cons(3,mt()))), 2)
  checkers.check-equals("length of mt list", length(mt()), 0)
end

#print("should be: runtime typecheck failed")
#length({first: {}, rest: {}})
