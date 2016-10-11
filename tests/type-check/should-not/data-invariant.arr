
data Foo<C>:
  | foo(a :: C, b :: (C -> Any))
end

fun wants-foo-of-records(lst :: Foo<{ a :: Number }>):
  lst
end

fun a(r :: { a :: Number, b :: String, c :: Boolean }):
 nothing
end

wants-foo-of-records(foo({ a : 5, b : "hello", c : true }, a))
