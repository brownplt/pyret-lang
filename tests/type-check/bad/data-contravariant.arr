
data My-List<C>:
  | my-empty()
  | my-link(first :: (C -> Any), rest :: My-List<C>)
end

fun wants-list-of-records(lst :: My-List<{ a :: Number }>):
  lst
end

fun a(r :: { a :: Number, b :: String, c :: Boolean }):
 nothing
end

wants-list-of-records(my-link(a, my-empty()))
