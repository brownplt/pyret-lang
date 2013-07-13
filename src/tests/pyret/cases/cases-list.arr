#lang pyret

fun matcher():
check:
  eq = checkers.check-equals
  test1 = cases(list.List) [1,2,3]:
    | empty => "not-this-one"
    | link(first, rest) => first
  end
  eq("list first", test1, 1)

  test2 = cases(list.List) []:
    | empty => "this-one"
    | link(first, rest) => raise("won't happen")
  end
  eq("list empty", test2, "this-one")

  test3 = cases(list.List) []:
    | link(first, rest) => []
    | else => 42
  end
  eq("list else", test3, 42)

  try:
    cases(list.List) "not-a-list":
      | else => "shouldn't hit this case"
    end
  except(e):
    checkers.check-true("case contract",
      e.contains("expected List"))
  end
end

