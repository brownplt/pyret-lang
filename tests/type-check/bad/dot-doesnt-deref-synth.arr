data Foo:
  | foo(ref bar :: Number)
end

my-foo = foo(5)

e = my-foo.bar
f :: Number = e
