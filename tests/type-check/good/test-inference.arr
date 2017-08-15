fun f(x :: Number, y):
  x + y
where:
  f(1, 2) is 3
end

fun my-map(g, xs):
  cases(List) xs:
    | link(first, rest) => link(g(first), my-map(g, rest))
    | empty => empty
  end
where:
  my-map(lam(x :: Number): x + 1 end, [list: 1, 2, 3]) is [list: 2, 3, 4]
  my-map(lam(x :: String): x + "a" end, [list: "a", "b"]) is [list: "aa", "ba"]
  my-map(lam(x): string-length(x) end, [list: "a", "ab"]) is [list: 1, 2]
  my-map(lam(x): x end, empty) is empty
end

fun other-f(x :: String, y :: String):
  x + y
end

test-1 :: Number = f(1, 2)
test-2 :: List<Number> = my-map(lam(x): string-length(x) end, [list: "a"])

fun q(x, y):
  x.a + y.a
where:
  q({a: 1}, {a: 1, b: 2}) is 2
  q({a: 1}, {a: 2}) is 3
end

test-3 :: Number = q({a: 1}, {a: 2})
