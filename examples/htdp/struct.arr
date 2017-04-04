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
    | mt() => 0
  end
where:
  length(cons(4,mt())) is 1
  length(cons(4,cons(3,mt()))) is 2
  length(mt()) is 0
end

#print("should be: runtime typecheck failed")
#length({first: {}, rest: {}})
