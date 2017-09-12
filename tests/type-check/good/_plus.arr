lst1 :: List<Number> = empty
lst-result1 :: List<Number> = lst1 + [list: 2]

lst2 :: List<String> = [list: "a"]
lst-result2 :: List<String> = lst2 + [list: "b"]

data Foo<A>:
  | foo1
  | foo2
sharing:
  method _plus(self, foo-thing :: Foo<A>) -> Number:
    4
  end
end

foo1 + foo2