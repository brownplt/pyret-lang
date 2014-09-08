
data Foo:
  | bar(a :: Number, b :: String)
end

fun wants-object(v :: { a :: Number, b :: String }):
  v
end

wants-object(bar(5, "hello"))
