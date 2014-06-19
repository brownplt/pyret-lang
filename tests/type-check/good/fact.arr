

fun fact-rec(n :: Number):
  if n == 0:
    1
  else:
    n * fact-rec(n)
  end
end


fact-five = fact-rec(5)
a :: Number = fact-five
