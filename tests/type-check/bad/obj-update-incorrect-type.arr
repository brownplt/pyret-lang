
data Foo:
  | foo(ref a :: Number)
end

a = foo(5)
a!{a : "hello"}
