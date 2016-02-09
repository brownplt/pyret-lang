
data My-List<C>:
  | my-empty()
  | my-link(first :: (C -> Any), rest :: My-List<C>)
end

fun wants-list-of-records(lst :: My-List<{ a :: Number, b :: String, c :: Boolean }>):
  lst
end

fun a(r :: { a :: Number, b :: String, c :: Boolean }):
 nothing
end

fun b(r :: { a :: Any, b :: String, c :: Any }):
 nothing
end

fun c(r :: { a :: Number, b :: String }):
 nothing
end

fun d(r :: { a :: Number, c :: Boolean }):
 nothing
end

fun e(r :: { b :: String, c :: Boolean }):
 nothing
end

fun f(r :: { }):
 nothing
end

fun g(r :: Any):
 nothing
end

wants-list-of-records(my-link(a, my-empty()))
wants-list-of-records(my-link(b, my-empty()))
wants-list-of-records(my-link(c, my-empty()))
wants-list-of-records(my-link(d, my-empty()))
wants-list-of-records(my-link(e, my-empty()))
wants-list-of-records(my-link(f, my-empty()))
wants-list-of-records(my-link(g, my-empty()))
