fun my-map(g, xs):
  cases(List) xs:
    | link(first, rest) => link(g(first), my-map(g, rest))
    | empty => empty
  end
where:
  my-map(lam(x :: Number): x + 1 end, [list: 1, 2, 3]) is [list: 2, 3, 4]
  my-map(lam(x): x end, empty) is empty
end

test-1 :: Number = f(1, 2)
test-2 :: List<Number> = my-map(lam(x): string-length(x) end, [list: "a"])
