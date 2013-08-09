#lang pyret

data MyData:
  | single
  | multi(a, b, c)
where:
  eq = checkers.check-equals
  test1 = cases(MyData) single:
    | single => 5
  end

  eq("singleton", test1, 5)

  test2 = cases(MyData) single:
    | multi(a, b, c) => "not-this-one"
    | else => 43
  end

  eq("else", test2, 43)

  test3 = cases(MyData) multi(1,2,3):
    | multi(a,b,c) => b
  end

  eq("field lookup", test3, 2)

  try:
    cases(MyData) multi(1,2,3):
      | single => 5
    end
  except(e):
    eq("Miss", e.name(), "No cases matched")
  end

end
