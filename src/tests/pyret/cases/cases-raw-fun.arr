#lang pyret

data D:
  | single
  | multi(a, b)
where:
  eq = checkers.check-equals
  dummy-loc = error.location("test", 0, 0)
  test1 = D.case_matcher(
      single,
      [ { key: "single", action: fun: 5 end } ],
      fun: nothing end,
      dummy-loc
    )
  eq("singleton", test1, 5)

  test2 = D.case_matcher(single, [
      { key: "multi", action: fun(a,b): "not-this-one" end }
    ],
    fun: 43 end,
    dummy-loc)
  eq("else", test2, 43)

  try:
    D.case_matcher(single, [
        { key: "nothing-useful", action: fun(a,b): "should miss" end }
      ],
      fun: "miss" end,
      dummy-loc)
  except(e):
    checkers.check-true("invalid case", e.name().contains("Invalid case"))
  end

end
