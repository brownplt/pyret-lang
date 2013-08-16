#lang pyret

data D:
  | single
  | multi(a, b)
where:
  eq = checkers.check-equals
  dummy-loc = error.location("test", 0, 0)
  test1 = single._match(
    { single: fun: 5 end } ,
    fun: nothing end
    )
  eq("singleton", test1, 5)

  test2 = single._match(
    { multi: fun(a,b): "not-this-one" end },
    fun: 43 end)
  eq("else", test2, 43)

end
