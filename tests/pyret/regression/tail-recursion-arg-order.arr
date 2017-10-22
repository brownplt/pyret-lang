fun iter(a, b, k):
  if k == 0:
    b
  else:
    iter(a + b, a, k - 1)
  end
end

check "https://github.com/brownplt/pyret-lang/issues/1230":
  iter(1, 0, 10) is 55
end
