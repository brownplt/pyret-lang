

fun even(n :: Number):
  if n == 0:
    true
  else:
    odd(n - 1)
  end
end

fun odd(n :: Number):
  if n == 1:
    true
  else:
    even(n - 1)
  end
end


a :: Boolean = even(5)
b :: Boolean = odd(6)
c :: Boolean = even(4)
d :: Boolean = odd(7)
