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
