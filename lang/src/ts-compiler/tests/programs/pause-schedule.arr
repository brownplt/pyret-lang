fun sum-to(n):
  if n == 0: 0
  else: n + sum-to(n - 1)
  end
end

fun flip(n):
  if n == 0: true
  else: flop(n - 1)
  end
end

fun flop(n):
  if n == 0: false
  else: flip(n - 1)
  end
end

print(sum-to(4000))
print("\n")
print(flip(50000))
print("\n")
print(torepr(map(lam(x): x * x end, range(0, 20))))
print("\n")

check:
  sum-to(1000) is 500500
  flip(10001) is false
end
