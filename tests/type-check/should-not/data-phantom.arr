
data My-List<C,D>:
  | my-empty()
  | my-link(first :: C, rest :: My-List<C,D>)
end

a :: My-List<Number,{}> = my-link(5, my-empty())
b :: My-List<Number,String> = a
