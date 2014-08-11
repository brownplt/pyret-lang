
data My-List<C>:
  | my-empty()
  | my-link(first :: C, rest :: My-List<C>)
end

fun wants-list-of-records(lst :: My-List<{ a :: Number }>):
  lst
end

wants-list-of-records(my-link({ a : 5, b : 6, c : true }, my-empty()))
wants-list-of-records(my-link({ a : 5, c : true }, my-empty()))
wants-list-of-records(my-link({ a : 5, c : "hello" }, my-empty()))
wants-list-of-records(my-link({ a : 5, c : my-link(5, my-link(6, my-empty())) }, my-empty()))
