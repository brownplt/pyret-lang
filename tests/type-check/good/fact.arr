

fun fact-rec(n :: Number):
  if n == 0:
    1
  else:
    n * fact-rec(n - 1)
  end
end


fact-five = fact-rec(5)
a :: Number = fact-five
