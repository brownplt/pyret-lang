
data Foo<A>:
  | foo(a :: A)
end

b = foo("hello")
print(b.a)
