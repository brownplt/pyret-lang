

data Foo:
  | foo(ref a :: Number, ref b :: Any)
end

a = foo(5, 6)
b :: Foo = a!{a : 6}
c = a!{b : "hello"}
d :: Foo = c
