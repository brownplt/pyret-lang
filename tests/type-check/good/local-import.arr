import file("./export.arr") as T

a :: T.Foo = T.foo(1)

b = cases(T.Foo) a:
  | foo(x) => x
end

c = T.f(1)

