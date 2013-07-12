#lang pyret

data D1:
  | name-overlap(b)
end

fun mkD2():
  data D2:
    | name-overlap(a)
  end
  {
    D2: D2,
    name-overlap: name-overlap
  }
check:
  D2 = mkD2()

  eq = checkers.check-equals

  test1 = cases(D2.D2) name-overlap(42):
    | name-overlap(a) => a
    | else => "should miss"
  end
  eq("different (nested) namespace, same named predicate should miss",
     test1, "should miss")

  test2 = cases(D2.D2) D2.name-overlap(42):
    | name-overlap(a) => a
    | else => "shouldn't miss"
  end
  eq("same (nested) namespace, same named predicate should hit",
     test2, 42)

  test3 = cases(D1) D2.name-overlap(42):
    | name-overlap(a) => a
    | else => "should miss"
  end
  eq("different namespace, same named predicate should miss",
     test3, "should miss")

  test4 = cases(D1) name-overlap(42):
    | name-overlap(a) => a
    | else => "should miss"
  end
  eq("same namespace, same named predicate should miss",
     test4, 42)
end

