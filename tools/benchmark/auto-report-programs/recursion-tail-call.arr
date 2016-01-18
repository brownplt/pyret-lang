fun triangle(n):
  fun help(shadow n, sum):
    if n <= 0: sum + 1
    else: help(n - 1, n + sum)
    end
  end
  help(n, 0)
end
print(triangle(200000))
