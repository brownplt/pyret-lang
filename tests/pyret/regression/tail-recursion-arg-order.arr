# https://github.com/brownplt/pyret-lang/issues/1230

fun iter(a, b, k):
  if k == 0:
    b
  else:
    iter(a + b, a, k - 1)
  end
where:
  iter(1, 0, 10) is 55
end

fun oops(a, b):
  if a == 1: b else: oops(1, a) end
end
fun working(a, b):
  if a == 1: b else: working(1, a) + 0 end
end

examples:
  oops(4, 5) is working(4, 5)
end
