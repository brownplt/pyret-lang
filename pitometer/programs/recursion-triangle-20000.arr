fun triangle(n):
  if n <= 0: 1
  else: n + triangle(n - 1)
  end
end
triangle(20000) 
# test recursion
