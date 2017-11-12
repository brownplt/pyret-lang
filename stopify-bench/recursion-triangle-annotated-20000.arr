import file("benchmark-base.arr") as B

fun triangle(n :: Number) -> Number:
  if n <= 0: 1
  else: n + triangle(n - 1)
  end
end

B.benchmark(lam(): triangle(2000) end, 1000)

# test recursion with simple annotations
