
fun f(x):
  y = x - 1
  if y > 0:
    g(y)
  else:
    y
  end
end

fun g(y):
  f(y)
end

test-print(f(10000))

