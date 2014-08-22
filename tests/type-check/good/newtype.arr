
newtype Foo as FooT

a = 5

b :: Foo = FooT.brand(a)
c :: Boolean = FooT.test(a)
d :: Boolean = FooT.test(b)
