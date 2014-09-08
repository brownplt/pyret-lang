
data Foo:
  | bar(a :: Number)
  | baz(a :: Number)
end

b :: Foo    = bar(5)
c :: Number = b.a
