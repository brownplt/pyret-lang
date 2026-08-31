fun fact(n):
  if n == 0: 1
  else: n * fact(n - 1)
  end
end

fun fib(n):
  if n < 2: n
  else: fib(n - 1) + fib(n - 2)
  end
end

rec loop = lam(i, acc):
  if i == 0: acc
  else: loop(i - 1, acc + i)
  end
end

print(tostring(fact(20)) + "\n")
print(tostring(fib(15)) + "\n")
print(tostring(loop(100000, 0)) + "\n")
