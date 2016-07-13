
data My-List<C>:
  | my-empty()
  | my-link(first :: C, rest :: My-List<C>)
end

fun wants-list-of-records(lst :: My-List<{ a :: Number, d :: Number }>):
  lst
end

wants-list-of-records(my-link({ a : 5 }, my-empty()))
