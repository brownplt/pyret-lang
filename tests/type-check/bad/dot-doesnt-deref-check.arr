data Foo:
  | foo(ref bar :: Number)
end

my-foo = foo(5)

e :: Number = my-foo.bar
