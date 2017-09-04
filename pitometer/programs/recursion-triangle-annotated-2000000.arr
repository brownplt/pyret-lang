fun triangle(n :: Number) -> Number:
  if n <= 0: 1
  else: n + triangle(n - 1)
  end
end
triangle(2000000) 
# test recursion with simple annotations
