#lang pyret

data D:
  | single
  | multi(a, b)
check:
  eq = checkers.check-equals
  test1 = D.case_matcher(
      single,
      [ { key: "single", action: fun: 5 end } ],
      fun: nothing end
    )
  eq("singleton", test1, 5)

  test2 = D.case_matcher(single, [
      { key: "multi", action: fun(a,b): "not-this-one" end }
    ],
    fun: 43 end)
  eq("else", test2, 43)

  try:
    D.case_matcher(single, [
        { key: "nothing-useful", action: fun(a,b): "should miss" end }
      ],
      fun: "miss" end)
  except(e):
    checkers.check-true("invalid case", e.name().contains("Invalid case"))
  end

end
