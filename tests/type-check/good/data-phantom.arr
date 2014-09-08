
data My-List<C,D>:
  | my-empty()
  | my-link(first :: C, rest :: My-List<C,D>)
end

a :: My-List<Number,{}> = my-empty() # my-link(5, my-empty())
b :: My-List<Any,Any> = a
c :: My-List<Number,{ a :: Number}> = a
